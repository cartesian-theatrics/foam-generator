(ns foam-generator.v5
  (:require
   [foam-generator.utils :as u]
   [foam-generator.grids :as g]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(def interior-radius 35)
(def interior-height 70)
(def wall-thickness 2)
(def exterior-radius (+ interior-radius wall-thickness))

(def propeller-intake-or 7.5)
(def propeller-intake-thickness 2)

(def blow-hole-step-size 2)
(def propeller-radius (- interior-radius 2))
(def propeller-height 20)
(def propeller-hole-width 4)
(def propeller-hole-height 10)
(def propeller-guard-length 15)
(def propeller-guard-width 2)
(def propeller-spin-point-length 5)

(def propeller-shaft-length (- interior-height propeller-height propeller-spin-point-length))

(def propeller-polyline-radius (- propeller-radius propeller-intake-or))

(def chamber-shaft-offset 1/2)
(def chamber-propeller-offset 1/2)
(def mixing-chamber-height (- propeller-shaft-length chamber-propeller-offset))
(def mixing-chamber-shaft-or (+ propeller-intake-or chamber-shaft-offset))
(def chamber-mixing-levels 3)
(def chamber-hole-size 2)
(def chamber-hole-density 1/6)            ; Ratio of hole area to non-hole surface area.
(def chamber-n-rows (/ (* interior-radius 2) chamber-hole-size))
(def chamber-n-cols chamber-n-rows)

(def chamber-output-radius 5)
(def chamber-output-height 5)

(assert (>= propeller-radius (* 2 propeller-intake-or)))

;; Propeller

(binding [m/*fn* 100]

  (def propeller-outer-polyline
    (let [r propeller-radius
          b propeller-intake-or
          a (Math/sqrt (- (Math/pow r 2) (Math/pow b 2)))
          rot (Math/atan (/ b a))
          curve-points (list* [0 b]
                              [0 (- b)]
                              (u/curve-points r (* 2 rot)))]
      (->> (m/polygon curve-points)
           (m/union (->> (m/polygon curve-points)
                         (m/rotatec [0 0 Math/PI]))))))

  (def propeller-inner-polyline
    (let [r (- propeller-radius wall-thickness)
          b (- propeller-intake-or wall-thickness)
          a (Math/sqrt (- (Math/pow r 2) (Math/pow b 2)))
          rot (Math/atan (/ b a))
          curve-points (list* [0 b]
                              [0 (- b)]
                              (u/curve-points r (* 2 rot)))]
      (->> (m/polygon curve-points)
           (m/union (->> (m/polygon curve-points)
                         (m/rotatec [0 0 Math/PI]))))))

  (def propeller-extrusion-shape
    (m/difference propeller-outer-polyline propeller-inner-polyline))

  (def propeller-extrusion-args {:height propeller-height :center false})

  (def propeller-hole-mask
    (let [hole
          (->> (m/square propeller-hole-width propeller-hole-width :center true)
               (m/extrude-linear {:height propeller-hole-height :center true})
               (m/translate [(- propeller-radius propeller-hole-width wall-thickness)
                             propeller-intake-or
                             (/ propeller-height 2)]))]
      (m/union hole (m/rotatec [0 0 Math/PI] hole))))

  (def propeller-air-guards
    (let [r propeller-radius
          rot (+ (Math/atan (/ propeller-guard-length r))
                 (Math/atan (/ propeller-intake-or 2 r)))
          guard
          (->> (u/curve r rot wall-thickness)
               (m/translate [0 0]))]
      (->> (m/union guard (m/rotatec [0 0 Math/PI] guard))
           (m/extrude-linear {:height propeller-height :center false}))))

  (def extruded-propeller
    (m/union
     propeller-air-guards
     (m/difference
      (m/extrude-linear {:height propeller-height :center false}
                        propeller-extrusion-shape)
      propeller-hole-mask)))

  (def propeller-outer-circle
    (binding [m/*fn* 100]
      (m/circle propeller-intake-or)))

  (def propeller-inner-circle
    (binding [m/*fn* 100]
      (m/circle (- propeller-intake-or propeller-intake-thickness))))

  (def propeller-shaft
    (->> (m/difference propeller-outer-circle propeller-inner-circle)
         (m/extrude-linear {:height propeller-shaft-length :center false})
         (m/translate [0 0 propeller-height ])))

  (def propeller-spin-point
    (m/hull (m/cylinder 1 wall-thickness :center false)
            (->> (m/cylinder propeller-intake-or wall-thickness :center false)
                 (m/translate [0 0 propeller-spin-point-length]))))

  (def propeller-top-support
    (->> (m/difference propeller-outer-polyline
                       propeller-inner-circle)
         (m/extrude-linear {:height wall-thickness :center false})
         (m/translate [0 0 (- propeller-height wall-thickness)])))

  (def propeller-bottom-support
    (->> propeller-outer-polyline
         (m/extrude-linear {:height wall-thickness :center false})))

  (def propeller-assembly
    (->> (m/union extruded-propeller
                  propeller-shaft
                  propeller-top-support
                  propeller-bottom-support)
         (m/translate [0 0 (+ propeller-spin-point-length)])
         (m/union propeller-spin-point)
         (m/translate [0 0 0.3])))

  ;; Outer container

  (def container-outer-cylinder
    (m/cylinder (+ interior-radius wall-thickness) interior-height :center false))

  (def container-inner-cylinder
    (->> (m/cylinder interior-radius interior-height :center false)
         (m/translate [0 0 wall-thickness])))

  (def propeller-spin-sock
    (m/difference
     (m/hull (->> (m/cylinder (+ propeller-intake-or wall-thickness) 1 :center false)
                  (m/translate [0 0 (- propeller-spin-point-length wall-thickness)]))
             (m/cylinder (+ propeller-intake-or wall-thickness 5) wall-thickness :center false))
     propeller-spin-point))

  (def container-cylinder
    (m/union (m/difference container-outer-cylinder container-inner-cylinder)
             propeller-spin-sock))

  ;; Mixing chamber

  (def chamber-shaft-inner-cylinder
    (m/cylinder mixing-chamber-shaft-or mixing-chamber-height :center false))

  (def chamber-shaft-outer-cylinder
    (m/cylinder (+ mixing-chamber-shaft-or wall-thickness) mixing-chamber-height :center false))

  (def chamber-shaft
    (m/difference chamber-shaft-outer-cylinder chamber-shaft-inner-cylinder))

  (def chamber-mixing-grid
    (-> (g/grid-3d chamber-mixing-levels chamber-n-rows chamber-n-cols chamber-hole-size chamber-hole-density true)
        (m/intersection container-outer-cylinder)
        (m/difference chamber-shaft-inner-cylinder)))

  (def chamber-assembly
    (->> (m/union chamber-shaft chamber-mixing-grid)
         (m/translate [0 0 (+ propeller-height propeller-spin-point-length chamber-propeller-offset)])))

  (def chamber-top-cylinder
    (m/cylinder exterior-radius wall-thickness :center false))

  (def chamber-output-translation (partial m/translate [(/ interior-radius 2) 0 0]))

  (def chamber-output-outer-cylinder
    (->> (m/cylinder chamber-output-radius (+ wall-thickness chamber-output-height) :center false)
         chamber-output-translation))

  (def chamber-output-inner-cylinder
    (->> (m/cylinder (- chamber-output-radius wall-thickness) (+ wall-thickness chamber-output-height) :center false)
         chamber-output-translation))

  (def chamber-output
    (m/difference chamber-output-outer-cylinder chamber-output-inner-cylinder))

  (def chamber-top-assembly
    (->> (m/difference chamber-top-cylinder chamber-output-inner-cylinder chamber-shaft-inner-cylinder)
         (m/union chamber-output)
         (m/translate [0 0 interior-height])))


  ;; Full assembly

  (def full-assembly
    (m/union propeller-assembly container-cylinder chamber-assembly chamber-top-assembly))

  )

(->> chamber-mixing-grid
     (s/write-scad)
     (spit "test.scad"))

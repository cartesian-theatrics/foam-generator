(ns foam-generator.v6
  (:require
   [foam-generator.utils :as u]
   [foam-generator.grids :as g]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(def interior-radius 35)
(def interior-height 70)
(def wall-thickness 1.6)
(def exterior-radius (+ interior-radius wall-thickness))

(def propeller-intake-thickness 2)

(def blow-hole-step-size 2)

(def chamber-shaft-offset 1/2)
(def chamber-propeller-offset 1/2)
(def mixing-chamber-height interior-height)
(def chamber-shaft-n-hole-layers 4)
(def chamber-intake-or 5.35)
(def mixing-chamber-shaft-or chamber-intake-or) ; fix
(def chamber-mixing-levels 20)
(def chamber-hole-size 0.85)
(def chamber-hole-density 1/6)            ; Ratio of hole area to non-hole surface area.
(def chamber-n-rows (/ (* interior-radius 2) chamber-hole-size))
(def chamber-n-cols chamber-n-rows)

(def chamber-output-radius 5)
(def chamber-output-height 5)

(binding [m/*fn* 100]

  ;; Outer container

  (def container-outer-cylinder
    (m/cylinder (+ interior-radius wall-thickness) interior-height :center false))

  (def container-inner-cylinder
    (->> (m/cylinder interior-radius interior-height :center false)
         (m/translate [0 0 wall-thickness])))

  (def container-cylinder
    (m/difference container-outer-cylinder container-inner-cylinder))

  ;; Mixing chamber

  (def chamber-shaft-inner-cylinder
    (m/cylinder mixing-chamber-shaft-or mixing-chamber-height :center false))

  (def chamber-shaft-outer-cylinder
    (m/cylinder (+ mixing-chamber-shaft-or wall-thickness) mixing-chamber-height :center false))

  (def chamber-shaft-hole
    (->> (m/square (* 2 (+ wall-thickness mixing-chamber-shaft-or)) chamber-hole-size :center true)
         (m/extrude-linear {:height chamber-hole-size :center false})))

  (def chamber-shaft-layer-hole-pattern
    (m/union (for [r (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) 8))]
               (->> chamber-shaft-hole
                    (m/rotatec [0 0 r])))))

  (def chamber-shaft-hole-layers
    (m/union (for [i (range chamber-shaft-n-hole-layers)]
               (->> chamber-shaft-layer-hole-pattern
                    (m/translate [0 0 (+ 0.85 (* i 2 0.85))])))))

  (def chamber-shaft
    (m/difference chamber-shaft-outer-cylinder chamber-shaft-inner-cylinder))

  (def chamber-grid-layer
    (loop [[step & steps] (cycle [0.2 1.65])
           lines []
           pos (- exterior-radius)]
      (if (>= pos exterior-radius)
        (->> (m/union lines)
             (m/extrude-linear {:height 0.85 :center false}))
        (recur steps
               (cond-> lines
                 (= step 1.65) (conj (u/polyline [[pos (- exterior-radius)]
                                                  [pos exterior-radius]]
                                                 (/ step 2))))
               (+ pos step)))))

  (def chamber-grid-layers
    (m/render
     (m/union
      (for [z (range chamber-mixing-levels)]
        (cond->> (->> chamber-grid-layer
                      (m/translate [0 0 (* 3/2 z)]))
          (odd? z) (m/rotatec [0 0 (/ Math/PI 2)]))))))


  (def chamber-mixing-grid
    (-> chamber-grid-layers
        (m/intersection container-outer-cylinder)
        (m/difference chamber-shaft-inner-cylinder)
        (m/render)))

  (def chamber-assembly
    (m/union chamber-shaft chamber-mixing-grid))

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
    (m/difference (m/union container-cylinder chamber-assembly chamber-top-assembly)
                  chamber-shaft-hole-layers)))

(->> full-assembly
     (s/write-scad)
     (spit "test.scad"))


(comment

  (let [support (->> (m/square (* 2 exterior-radius) 1.65 :center true)
                     (m/extrude-linear {:height 10 :center false}))]
    (->> (m/union (m/translate [0 (- exterior-radius) 0] support)
                  (m/translate [0 exterior-radius 0] support)
                  (m/translate [0 0 10] chamber-grid-layer))
         (s/write-scad)
         (spit "test.scad")))

  (->> chamber-grid-layer
       (m/translate [0 0 ]))

  (def chamber-grid-layers
    (m/render
     (m/union
      (for [z (range 40)]
        (cond->> (->> chamber-grid-layer
                      (m/translate [0 0 (* 3/2 z)]))
          (odd? z) (m/rotatec [0 0 (/ Math/PI 2)]))))))



  )

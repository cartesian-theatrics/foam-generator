(ns foam-generator.v4
  "Foam generator design that utilizes a spinning cork screw that
  mixes air into foam into the water as it extrudes it."
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(def step-size)

(def interior-radius 30)
(def interior-height 70)
(def wall-thickness 1)

(def propeller-intake-or 4)
(def propeller-intake-thickness 1)

(def blow-hole-step-size 2)
(def propeller-twist (/ Math/PI 6))
(def propeller-radius (- interior-radius 2))
(def propeller-height 20)
(def propeller-hole-size 4)
(def propeller-guard-length 10)
(def propeller-guard-width 2)

(def propeller-extrusion-shape
  (m/difference (u/polyline [[(- propeller-radius ) 0] [propeller-radius  0]]
                            propeller-intake-or)
                (u/polyline [[(- propeller-radius ) 0] [propeller-radius  0]]
                            (- propeller-intake-or propeller-intake-thickness))))

(def propeller-extrusion-args {:height propeller-height :twist propeller-twist :center false :step-size 1})

(def propeller-hole-mask
  (let [holes
        (->> (->> (m/square propeller-hole-size propeller-hole-size :center true)
                  (m/translate [(dec propeller-radius) (- (/ propeller-hole-size 2))]))
             (u/extrude-linear-stepwise propeller-extrusion-args)
             (m/union))]
    (m/union holes (m/rotatec [0 0 Math/PI] holes))))

(def propeller-air-guards
  (let [rot (+ (Math/atan (/ propeller-guard-length propeller-radius))
               (Math/atan (/ propeller-intake-or 2 propeller-radius)))
        guard
        (->> (u/curve propeller-radius rot 1)
             (m/rotatec [0 0 (- rot)])
             (m/translate [(inc (/ propeller-hole-size 2)) (- (/ propeller-guard-length))]))]
    (->> (m/union guard (m/rotatec [0 0 Math/PI] guard))
         (m/extrude-linear propeller-extrusion-args))))

#_(def propeller-hole-mask
  (let [rot (+ (Math/atan (/ propeller-guard-length propeller-radius))
               (Math/atan (/ propeller-intake-or 2 propeller-radius)))
        holes
        (->> (->> (u/curve propeller-radius rot 1)
                  (m/rotatec [0 0 (- rot)])
                  (m/translate [(dec propeller-radius) (- (/ propeller-hole-size 2))]))
             (u/extrude-linear-stepwise propeller-extrusion-args)
             (m/union))]
    (m/union holes (m/rotatec [0 0 Math/PI] holes))))



(def extruded-propeller
  (m/union
   propeller-air-guards
   (m/difference
    (u/rotate-extrude {:twist propeller-twist :height propeller-height :step-size 1/4}
                      propeller-extrusion-shape)
    propeller-hole-mask)))

(->> extruded-propeller
     (s/write-scad)
     (spit "test.scad"))

(defn top-and-bottom
  [{:keys [step-size step-offset twist height]} & block]
  (let [e (m/union block)
        n-steps (/ (* 1 height) step-size)
        twist-step-size (/ twist n-steps)
        top-support (m/difference
                     (m/hull
                      (->> (m/square 4 15 :center true)
                           (m/extrude-linear {:height 1 :center false})
                           (m/rotatec [0 0 (* (dec n-steps) twist-step-size)])
                           (m/translate [0 0 (+ height 44)]))
                      (->> (m/square 4 15 :center true)
                           (m/extrude-linear {:height 1 :center false})
                           (m/rotatec [0 0 (* (dec n-steps) twist-step-size)])
                           (m/translate [0 0 (dec height)])))
                     (->> (m/cylinder 3 (- 61 height) :center false)
                          (m/translate [0 0 (dec height)])))]
    (m/union top-support
             (for [step [0 (dec n-steps)]]
               (let [rot (- (* step twist-step-size))
                     z (* step step-size)]
                 (->> (if (zero? step) e (m/difference e (m/circle 3))) ; hack
                      (m/rotatec [0 0 rot])
                      (m/extrude-linear {:height step-size :twist twist-step-size :center false})
                      (m/translate [0 0 z])))))))

(defn shaft
  [outer-radius height thickness center?]
  (->>
   (m/difference
    (m/circle outer-radius)
    (m/circle (- outer-radius thickness)))
   (m/extrude-linear {:height height :center center?})))

(comment
  ;; Extrude with twist
  (->> (u/polyline [[0 0] [20 0]] 1)
       (u/extrude-linear-stepwise {:height 10 :twist (/ Math/PI 2) :step-size 1})
       (m/union)
       (s/write-scad)
       (spit "test.scad"))

  ;; Extrude with twist and hole.
  (->> (m/difference (u/polyline [[0 0] [20 0]] 2)
                     (->> (m/square 1 2 :center true)
                          (m/translate [19 -1 0])))
       (m/extrude-linear {:height 10 :twist (/ Math/PI 2)})
       (s/write-scad)
       (spit "test.scad"))

  (binding [m/*fn* 100]
    (let [width 35
          propeller-height 15
          propeller-intake-or 4
          propeller-intake-thickness 1
          line (m/difference (u/polyline [[0 0] [width 0]] propeller-intake-or)
                             (u/polyline [[0 0] [width 0]] (- propeller-intake-or propeller-intake-thickness)))
          propeller-extrusion-args {:height propeller-height :twist (/ Math/PI 4) :center false :step-size 1}
          top-and-bottom (top-and-bottom (update propeller-extrusion-args :step-size / 2)
                                                   (u/polyline [[0 0] [width 0]] propeller-intake-or))
          shaft-height 60
          air-shaft (shaft propeller-intake-or shaft-height propeller-intake-thickness false)
          shaft-hole (->> (u/polyline [[(- propeller-intake-thickness) 0] [propeller-intake-thickness 0]]
                                      (- propeller-intake-or propeller-intake-thickness))
                          (m/extrude-linear propeller-extrusion-args))
          blade (->> (m/difference
                      (->> line
                           (m/extrude-linear propeller-extrusion-args))
                      (->> (->> (m/square 4 4 :center true)
                                (m/translate [(dec width) -3 0]))
                           (u/extrude-linear-stepwise propeller-extrusion-args)
                           (m/union))))
          blades (m/union blade (m/rotatec [0 0 Math/PI] blade))
          assembly (m/union (m/difference (m/union blades air-shaft)
                                          shaft-hole)

                            top-and-bottom
                            (m/rotatec [0 0 Math/PI] top-and-bottom))]
      (->> assembly
           (s/write-scad)
           (spit "test.scad"))))

  )

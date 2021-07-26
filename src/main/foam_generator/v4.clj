(ns foam-generator.v4
  "Foam generator design that utilizes a spinning cork screw that
  mixes air into foam into the water as it extrudes it."
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(defn extrude-linear-stepwise
  [{:keys [step-size step-offset twist height] :as args} & block]
  (let [e (m/union block)
        n-steps (/ (* 1 height) step-size)
        twist-step-size (/ twist n-steps)
        ]
    (for [step (next (range n-steps))
          :when (even? step)]
      (let [rot (- (* step twist-step-size))
            z (* step step-size)]
        (->> e
             (m/rotatec [0 0 rot])
             (m/extrude-linear {:height step-size :twist twist-step-size :center false})
             (m/translate [0 0 z]))))))

(defn top-and-bottom
  [{:keys [step-size step-offset twist height]} & block]
  (let [e (m/union block)
        n-steps (/ (* 1 height) step-size)
        twist-step-size (/ twist n-steps)]
    (for [step [0 (dec n-steps)]]
      (let [rot (- (* step twist-step-size))
            z (* step step-size)]
        (->> (if (zero? step) e (m/difference e (m/circle 3)))             ; hack
             (m/rotatec [0 0 rot])
             (m/extrude-linear {:height step-size :twist twist-step-size :center false})
             (m/translate [0 0 z]))))))

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
       (extrude-linear-stepwise {:height 10 :twist (/ Math/PI 2) :step-size 1})
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
    (let [width 20
          fan-height 15
          intake-or 4
          intake-thickness 1
          line (m/difference (u/polyline [[0 0] [width 0]] intake-or)
                             (u/polyline [[0 0] [width 0]] (- intake-or intake-thickness)))
          extrusion-args {:height fan-height :twist (/ Math/PI 4) :center false :step-size 1}
          top-and-bottom (m/union (top-and-bottom (update extrusion-args :step-size / 2)
                                                  (u/polyline [[0 0] [width 0]] intake-or)))
          shaft-height 60
          air-shaft (shaft intake-or shaft-height intake-thickness false)
          shaft-hole (->> (u/polyline [[(- intake-thickness) 0] [intake-thickness 0]]
                                      (- intake-or intake-thickness))
                          (m/extrude-linear extrusion-args))
          blade (->> (m/difference
                      (->> line
                           (m/extrude-linear extrusion-args))
                      (->> (->> (m/square 4 4 :center true)
                                (m/translate [(dec width) -3 0]))
                           (extrude-linear-stepwise extrusion-args)
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

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
(def wall-thickness 2)

(def propeller-intake-or 6.5)
(def propeller-intake-thickness 2)

(def blow-hole-step-size 2)
(def propeller-twist (/ Math/PI 6))
(def propeller-radius (- interior-radius 2))
(def propeller-height 20)
(def propeller-hole-size 4)
(def propeller-guard-length 15)
(def propeller-guard-width 2)
(def propeller-spin-point-length 5)

(def propeller-shaft-length (- interior-height propeller-height wall-thickness))

(def propeller-polyline-radius (- propeller-radius propeller-intake-or))

(assert (>= propeller-radius (* 2 propeller-intake-or)))

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

(def propeller-extrusion-args {:height propeller-height :twist propeller-twist :center false :step-size 1})

(def propeller-hole-mask
  (let [holes
        (->> (->> (m/square propeller-hole-size propeller-hole-size :center true)
                  (m/translate [(- propeller-radius propeller-hole-size wall-thickness) (- propeller-intake-or)]))
             (u/extrude-linear-stepwise propeller-extrusion-args)
             (m/union))]
    (m/union holes (m/rotatec [0 0 Math/PI] holes))))

(def propeller-air-guards
  (let [r propeller-radius
        rot (+ (Math/atan (/ propeller-guard-length r))
               (Math/atan (/ propeller-intake-or 2 r)))
        guard
        (->> (u/curve r rot wall-thickness)
             (m/rotatec [0 0 (- rot)])
             (m/translate [0 0]))]
    (->> (m/union guard (m/rotatec [0 0 Math/PI] guard))
         (u/rotate-extrude {:twist propeller-twist :height propeller-height :step-size 1/2}))))

(def extruded-propeller
  (m/union
   propeller-air-guards
   (m/difference
    (u/rotate-extrude {:twist propeller-twist :height propeller-height :step-size 1/2}
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
       (m/translate [0 0 (- propeller-height wall-thickness)])))

(def propeller-spin-point
  (m/hull (m/cylinder propeller-intake-or wall-thickness :center false)
          (->> (m/cylinder 1 wall-thickness :center false)
               (m/translate [0 0 (- propeller-spin-point-length)]))))

(def propeller-top-support
  (->> (m/difference propeller-outer-polyline
                     propeller-inner-circle)
       (m/rotatec [0 0 (- propeller-twist)])
       (m/extrude-linear {:height wall-thickness :center false})
       (m/translate [0 0 (- propeller-height wall-thickness)])))

(def propeller-bottom-support
  (->> propeller-outer-polyline
       (m/extrude-linear {:height wall-thickness :center false})))

(def propeller-assembly
  (m/union extruded-propeller
           propeller-shaft
           propeller-top-support
           propeller-bottom-support
           #_propeller-spin-point))

(->> propeller-assembly
     (s/write-scad)
     (spit "test.scad"))

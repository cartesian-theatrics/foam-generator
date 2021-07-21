(ns foam-generator.cap
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(defn curve
  [a r steps]
  (let [step-size (/ r steps)
        curve (m/polygon
               (for [x (u/irange 0 (+ r step-size) step-size)]
                 [(* a (Math/cos x)) (* a (Math/sin x))]))]
    curve))

(defn round-cap [ir thickness]
  (let [a (m/circle (+ ir thickness 1))
        b (m/circle (+ ir thickness))
        c (m/circle ir)]
    (m/union
     (->> a
          (m/extrude-linear {:height 1 :center false}))
     (->> (m/union c (m/difference a b))
          (m/extrude-linear {:height 4 :center false})
          (m/translate [0 0 1])))))

(defn square-cap [x y thickness])

(comment
  (->> (curve 2 Math/PI 30)
       (s/write-scad)
       (spit "test.scad"))

  (binding [m/*fn* 100]
    (->> (round-cap 10 1)
         (s/write-scad)
         (spit "test.scad")))

  (m/poly)

  (u/irange 0 10)

  )

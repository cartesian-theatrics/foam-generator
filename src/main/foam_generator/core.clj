(ns main.foam-generator.core
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [context forward hull left right up down roll backward defmodel no-op translate
            lookup-transform rotate transform]]))

(def ctx
  {:fn 10 :curve-radius 10})

(def pi Math/PI)
(def hpi (/ Math/PI 2))

(defmodel air-hose-plug
  (assoc ctx :fn 10)
  (let [c1o (m/circle p/plug-or)
        c1i (m/circle p/plug-ir)

        c2o (m/circle p/plug-third-segment-radius)
        c2i (m/circle (- p/plug-third-segment-radius 3/2))

        bulge-circle (m/circle p/plug-bulge-or)]
    [[(context :shape c2o) (context :shape c2i)]
     (forward :length p/plug-third-segment-length)
     [(context :shape c1o) (context :shape c1i)]
     (forward :length p/plug-second-hull-length)
     (hull)
     (forward :length p/plug-second-segment-length)
     (forward :length p/plug-bulge-hull-length)
     [(context :shape bulge-circle)]
     (forward :length p/plug-bulge-segment-length)
     (hull)
     [(context :shape c1o)]
     (forward :length p/plug-bulge-hull-length)
     (hull)
     (forward :length p/plug-first-segment-length)]))


(defmodel tube-connector
  (assoc ctx :fn 10)
  (let [outer-circle-medium (m/circle p/tubing-ir)
        outer-circle-small (m/circle (- p/tubing-ir 1/2))
        inner-circle (m/circle (- p/tubing-ir 1/2 3/2))]
    (into []
          cat
          (for [x (range 3)]
            [[(context :shape outer-circle-medium) (context :shape inner-circle)]
             (forward :length 1)
             [(context :shape outer-circle-small)]
             (forward :length 4)
             (hull)]))))

(defmodel intake-plate
  ctx
  (let [c (m/circle p/intake-or)]
    [[(context :shape c)]
     (forward :length p/intake-plate-thickness)
     [:segment
      (vec
       (for [x (range 3)]
         (let [d (* x (/ (* 2 Math/PI) 3))]
           [:branch [(translate :x (* 10 (Math/cos d)) :y (* 10 (Math/sin d)))
                     [:segment (if (zero? x) air-hose-plug tube-connector)]]])))]]))

(defmodel squeeze-trigger
  (assoc ctx :curve-radius 40)
  (let [shape (m/circle 3)]
    [[(context :shape shape)]
     (forward :length 10)
     (left :curve-radius 3)
     (forward :length 60)
     (left :curve-radius 3)
     (forward :length 20)
     (hull :n-segments 3)
     (left :curve-radius 3)
     (roll :angle Math/PI)
     (left :angle (/ Math/PI 6))
     [(context :curve-radius 60)]
     (right :angle (/ Math/PI 6))
     (left :angle (/ Math/PI 3) :curve-radius 4)]))

(defmodel gun-body
  ctx
  (let [main-shape (m/minkowski (m/square 29 15)
                                (m/circle 3))]
    [[:branch
      [(rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
       [:segment air-hose-plug]]]
     [(context :shape main-shape :curve-radius 35/2)]
     (rotate :axis [0 1 0] :angle (/ Math/PI 8))
     #_(rotate :axis [0 1 0] :angle (/ Math/PI 6))
     (forward :length 90)
     (right :angle (- (/ Math/PI 2) (/ Math/PI 8)))
     [(context :shape (m/circle 15))]
     (forward :length 130)
     (hull)]))

(defmodel trigger
  ctx
  (let [shape (m/square 8 6)]
    [[(context :shape shape :curve-radius 15)]
     (rotate :axis [0 1 0] :angle (/ Math/PI 8))
     (forward :length 10 :gap false)
     [:branch
      [(right :angle (/ Math/PI 2) :curve-radius 4 :gap false)
       (forward :length 10)]]
     (forward :length 60)
     (right :angle (- (/ Math/PI 2) (/ Math/PI 8))
            :curve-radius 4)
     (forward :length (- 15 3/2))
     (right :angle (* 2 Math/PI) :curve-radius 4)
     (right :curve-radius 4 :angle (/ Math/PI 2.3))
     (right :angle (/ Math/PI 8) :curve-radius 40)
     (left :angle (/ Math/PI 3.8) :curve-radius 60)]))

(m/union trigger gun-body)

(defmodel spring
  (assoc ctx :fn 100)
  (let [a (/ pi 8)
        width 10
        n-turns 4
        thickness 1]
    (conj
     (into [[(context :shape (m/square thickness width))]
            (translate :x (/ thickness 2) :z thickness)
            (forward :length width)
            (rotate :axis [0 1 0] :angle pi)
            (forward :length width)
            (left :angle (- pi a) :curve-radius 1/2)]
           cat
           (for [_ (range n-turns)]
             [(forward :length (- (/ width (Math/cos a)) thickness) :angle (* a 2))
              (right :angle (- pi (* a 2)) :curve-radius (/ thickness 2))
              (roll :angle pi)]))
     (left :angle a :curve-radius (/ thickness 2))
     (forward :length 10))))

(defn cosine-hill [w h n-steps]
  (m/polygon
   (for [step (range n-steps)]
     (let [step step
           x (- (* step (/ w (dec n-steps))) (/ w 2))
           y (* (/ h 2) (Math/cos (* step (/ (* 2 Math/PI) (dec n-steps)))))]
       [x y]))))

(defmodel intake
  ctx
  [#_[:branch
    [(rotate :axis [0 1 0] :angle pi)
     [:segment air-hose-plug]]]
   [(context :shape (m/circle p/plug-third-segment-radius))
    (context :shape (m/circle (- p/plug-third-segment-radius 3/2)))]
   (forward :length 1)
   [(context :shape (cosine-hill 12 3 50)) (context :shape nil)]
   #_[:branch
    [(up :curve-radius 5)
     (down :curve-radius 5)]]
   (translate :y 4)
   (forward :length 10)
   (hull)
   (forward :length 20)
   (down :angle Math/PI :curve-radius 3/2)
   (forward :length 20)
   (forward :length 1)
   (translate :y 4 :z -9)
   [:segment
    (let [[s1 s2 & segs] (:path-spec (meta tube-connector))]
      (vec (list* s1 s2 (hull :n-segments 2) nil)))]])

(defmodel tmp
  ctx
  [[(context :shape (cosine-hill 12 3 50))
    (context :shape (cosine-hill 6 3/2 50))]
   (forward :length 50)])

(defmodel living-spring
  ctx
  (let [angle (/ Math/PI 4)
        r 40
        shape (m/square 2 10)]
    [[(context :shape shape)]
     (rotate :axis [0 1 0] :angle (- (/ angle 2)))
     (right :curve-radius r :angle angle)]))

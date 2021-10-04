(ns main.foam-generator.core
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [context forward hull left right up down roll backward defmodel no-op translate
            model branch segment set
            ctx spin lookup-transform rotate transform]]))

(def cx
  {:fn 10 :curve-radius 10})

(def pi Math/PI)
(def hpi (/ Math/PI 2))

(defmodel air-hose-plug
  :fn 10
  (segment
   (let [c1o (m/circle p/plug-or)
         c1i (m/circle p/plug-ir)

         c2o (m/circle p/plug-third-segment-radius)
         c2i (m/circle (- p/plug-third-segment-radius 3/2))

         bulge-circle (m/circle p/plug-bulge-or)]
     [(model :name :body :shape c2o :mask? false)
      (model :name :mask :shape c2i :mask? true)

      (backward :to [:mask] :length 0.01)
      (forward :to [:mask] :length 0.01)
      (forward :length p/plug-third-segment-length)

      (set :shape c1o :to [:body])
      (set :shape c1i :to [:mask])

      (forward :length p/plug-second-hull-length)
      (hull)
      (forward :length p/plug-second-segment-length)
      (forward :length p/plug-bulge-hull-length)

      (set :shape bulge-circle :to [:body])
      (forward :length p/plug-bulge-segment-length)
      (hull)
      (set :shape c1o :to [:body])
      (forward :length p/plug-bulge-hull-length)
      (hull)
      (forward :length p/plug-first-segment-length :to [:body])
      (forward :length (+ 0.01 p/plug-first-segment-length) :to [:mask])])))

(defmodel tube-connector
  :fn 100
  (segment
   (let [outer-circle-medium (m/circle p/tubing-ir)
         outer-circle-small (m/circle (- p/tubing-ir 1/2))
         inner-circle (m/circle (- p/tubing-ir 1/2 3/2))]
     (into [(model :name :body :shape outer-circle-medium :mask? false)
            (model :name :mask :shape inner-circle :mask? true)]
           cat
           (for [_ (range 3)]
             [(set :shape outer-circle-medium :to [:body])
              (set :shape inner-circle :to [:mask])

              (forward :length 1)
              (set :shape outer-circle-small :to [:body])
              (forward :length 4)
              (hull)])))))

(defmodel squeeze-trigger
  :fn 10
  (model :shape (m/circle 3) :name :body :mask? false)
  (forward :length 10)
  (left :curve-radius 3)
  (forward :length 60)
  (left :curve-radius 3)
  (forward :length 20)
  (hull :n-segments 3)
  (left :curve-radius 3)
  (roll :angle Math/PI)
  (left :angle (/ Math/PI 6))
  (set :curve-radius 60)
  (right :angle (/ Math/PI 6))
  (left :angle (/ Math/PI 3) :curve-radius 4))

(defmodel gun-body
  :fn 100
  (model :shape (m/minkowski (m/square 35 15) (m/circle 3))
         :name :body
         :curve-radius 41/2)

  (model :shape nil :name :mask :mask? true)

  (branch
   (rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
   (segment air-hose-plug))

  (branch
   (rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
   (translate :x 14 :z 0)
   (segment tube-connector))

  (branch
   (rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
   (translate :x (- 14) :z 0)
   (segment tube-connector))

  (rotate :axis [0 1 0] :angle (/ Math/PI 8))
  (forward :length 90)
  (right :angle (- (/ Math/PI 2) (/ Math/PI 8)))
  (set :shape (m/circle 15))
  (forward :length 130))

(defmodel valve-cap
  :fn 10
  (model :shape (m/circle 10) :mask? false :name :body)
  (model :shape (u/ovol 9 2) :mask? true :name :mask)
  (forward :length 4 :to [:body])
  (translate :z 4 :to [:mask])
  (spin :angle (* 2 pi) :to [:mask]))

(defmodel trigger
  :fn 10
  (model :shape (m/square 8 6) :curve-radius 15 :mask? false :name :body)
  (rotate :axis [0 1 0] :angle (/ Math/PI 8))
  (forward :length 10 :gap false)
  (branch
   (right :angle (/ Math/PI 2) :curve-radius 4 :gap false)
   (forward :length 10))

  (forward :length 60)
  (right :angle (- (/ Math/PI 2) (/ Math/PI 8))
         :curve-radius 4)
  (forward :length (- 15 3/2))
  (right :angle (* 2 Math/PI) :curve-radius 4)
  (right :curve-radius 4 :angle (/ Math/PI 2.3))
  (right :angle (/ Math/PI 8) :curve-radius 40)
  (left :angle (/ Math/PI 3.8) :curve-radius 60)
  )

(defn cosine-hill [w h n-steps]
  (m/polygon
   (for [step (range n-steps)]
     (let [step step
           x (- (* step (/ w (dec n-steps))) (/ w 2))
           y (* (/ h 2) (Math/cos (* step (/ (* 2 Math/PI) (dec n-steps)))))]
       [x y]))))

(defmodel intake
  :fn 10
  (model :shape (m/circle p/plug-third-segment-radius) :mask? false :name :body)
  (model :shape (m/circle (- p/plug-third-segment-radius 3/2)) :mask? false :name :mask)

  (forward :length 1)

  (set :shape (cosine-hill 12 3 50) :to [:body])
  (set :shape nil :to [:mask])

  (translate :y 4)
  (forward :length 10)
  (hull)
  (forward :length 20)
  (branch
   (forward :length 20))
  (down :angle Math/PI :curve-radius 3)
  (forward :length 20)
  (forward :length 1)
  (translate :y (- 2) :z 9)
  (segment tube-connector))

(defmodel spring
  (assoc cx :fn 100)
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

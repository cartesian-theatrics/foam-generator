(ns foam-generator.core
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path
            model branch segment set arc
            body mask
            spin lookup-transform rotate transform]]))

(def pi Math/PI)
(def hpi (/ Math/PI 2))

(defmodel air-hose-plug
  (segment
   (let [c1o (m/circle p/plug-or)
         c1i (m/circle p/plug-ir)

         c2o (m/circle p/plug-third-segment-radius)
         c2i (m/circle (- p/plug-third-segment-radius 3/2))

         bulge-circle (m/circle p/plug-bulge-or)]
     [(model :name :body :shape c2o :mask? false)
      (model :name :mask :shape c2i :mask? true)
      (set :fn 120)

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
  (segment
   (let [outer-circle-medium (m/circle p/tubing-ir)
         outer-circle-small (m/circle (- p/tubing-ir 1/2))
         inner-circle (m/circle (- p/tubing-ir 1/2 3/2))]
     (into [(model :name :body :shape outer-circle-medium :mask? false)
            (model :name :mask :shape inner-circle :mask? true)
            (set :fn 100)]
           cat
           (for [_ (range 3)]
             [(set :shape outer-circle-medium :to [:body])
              (set :shape inner-circle :to [:mask])

              (forward :length 1)
              (set :shape outer-circle-small :to [:body])
              (forward :length 4)
              (hull)])))))

(def trigger
  (path
   (model :shape (m/square 8 40) :curve-radius 15 :mask? false :name :body)
   (rotate :axis [0 1 0] :angle (/ Math/PI 8))
   (translate :z 20)
   (forward :length 10 :gap false)
   #_(branch
    :from :body
    (right :angle (/ Math/PI 2) :curve-radius 4 :gap false)
    (forward :length 10))

   (forward :length 40)
   (right :angle (- (/ Math/PI 2) (/ Math/PI 8))
          :curve-radius 4)
   (forward :length (- 15 3/2))
   (right :angle (* 2 Math/PI) :curve-radius 4)
   (right :curve-radius 4 :angle (/ Math/PI 2.3))
   (right :angle (/ Math/PI 8) :curve-radius 40)
   (left :angle (/ Math/PI 3.8) :curve-radius 60)))

(defmodel gun-body
  (model :shape (m/minkowski (m/square 35 15) (m/circle 3))
         :name :body
         :curve-radius 41/2)
  (model :shape nil :name :mask :mask? true)
  (set :fn 100)

  #_(branch
     :from :body
     (rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
     (segment air-hose-plug))

  (segment
   (for [x [-8 8]]
     (branch
      :from :body
      (rotate :axis [0 1 0] :angle (+ (/ Math/PI 8) Math/PI))
      (translate :x x)
      (segment tube-connector))))

  (rotate :axis [0 1 0] :angle (/ Math/PI 8))
  (forward :length 90)
  (right :angle (- (/ Math/PI 2) (/ Math/PI 8)))

  #_(set :shape (m/circle 10))
  #_(forward :length 130)
  #_(hull))

(m/difference gun-body trigger)

(defmodel valve
  (model :shape (m/circle 2) :mask? false :name :body :order 0 :fn 100)
  (model :shape nil :name :mask :mask? true)
  (set :fn 100)
  (set :shape (m/union (m/circle 2)
                       (m/intersection (->> (m/square 5 2)
                                            (m/translate [5/2 0]))
                                       (m/circle 5)))
       :to [:body])
  (set :shape (m/circle 15) :to [:body])
  (model :shape (binding [m/*fn* 100] (u/ovol 13.5 2)) :mask? true :name :mask :order 1 :fn 100)
  (branch
   :from :body
   (set :shape (m/square 2 (dec (* 15 2))) :to [:body])
   (translate :z 2)
   (forward :length 4 :order 2 :to [:body]))
  (branch
   :from :body
   (forward :length 2 :to [:body] :fn 15)
   (forward :length 4 :to [:body])
   (translate :z 6 :to [:mask])
   (spin :angle (* 2 pi) :to [:mask])))

(defmodel arcs
  (model :shape (m/square 1 3) :fn 5 :name :body)
  (set :fn 100)
  (rotate :axis :x :angle (/ pi 2))
  (translate :y 3/2)
  (segment
   (for [_ (range 10)]
     (segment
      (rotate :axis :y :angle (/ (* 2 pi) 10))
      (branch :from :body (arc :side-length 10 :curve-radius 7))))))

(defmodel resistance-wheel
  (model :shape (m/difference (m/circle 10) (m/circle 9))
         :fn 10
         :mask? false
         :name :body)
  (branch
   :from :body
   (segment arcs))
  (branch
   :from :body
   (forward :length 3)))

(defn cosine-hill [w h n-steps]
  (m/polygon
   (for [step (range n-steps)]
     (let [step step
           x (- (* step (/ w (dec n-steps))) (/ w 2))
           y (* (/ h 2) (Math/cos (* step (/ (* 2 Math/PI) (dec n-steps)))))]
       [x y]))))

(defmodel intake
  (model :shape (m/circle p/plug-third-segment-radius) :mask? false :name :body)
  (model :shape (m/circle (- p/plug-third-segment-radius 3/2)) :mask? false :name :mask)

  (forward :length 1)

  (set :shape (cosine-hill 12 3 50) :to [:body])
  (set :shape nil :to [:mask])

  (translate :y 4)
  (forward :length 10)
  (hull)
  (forward :length 20)
  (branch :from :body (forward :length 20))
  (down :angle Math/PI :curve-radius 3)
  (forward :length 20)
  (forward :length 1)
  (translate :y (- 2) :z 9)
  (segment tube-connector))

(defmodel spring
  (let [a (/ pi 8)
        width 10
        n-turns 4
        thickness 1]
    (segment
     (conj
      (into [(model :shape (m/square thickness width))
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
      (forward :length 10)))))

(def motor-r (/ 48.8 2))
(def motor-l 20)
(def motor-axle-w 5)
(def motor-axle-r 7/2)
(def motor-bolt-r 2.2)

(defmodel mount-bracket

  (model :shape (m/union (m/square (+ 4 (* 2 motor-bolt-r)) 3))
         :name :body)

  (model :shape (m/circle 2) :name :mask :mask? true :order 1)

  (set :curve-radius (+ motor-r (/ 9 2)))

  (translate :x (+ motor-r (- (/ (+ 4 (* 2 motor-bolt-r))
                                 2)
                              0.1)))
  (rotate :axis :x :angle (/ pi 2))
  (translate :y 3/2)
  (rotate :axis :x :angle pi)
  (left :side-length 4 :gap true)
  (rotate :axis :x :angle pi)
  (left :side-length 4 :to [:body])
  (left :side-length 4 :to [:mask] :gap true)
  (branch
   :from :body
   (rotate :axis :x :angle (- (/ pi 2)))
   (forward :length 3.01 :center true :to [:mask]))
  (left :side-length 6 :to [:body])
  (left :side-length 6 :to [:mask] :gap true)
  (branch
   :from :body
   (rotate :axis :x :angle (- (/ pi 2)))
   (forward :length 3.01 :center true :to [:mask]))
  (left :side-length 4 :to [:body]))

(defmodel motor
  (model :shape (m/circle motor-r) :name :body :order 0)
  (set :fn 50)
  (branch
   :from :body
   (segment mount-bracket))

  (branch
   :from :body
   (rotate :axis :y :angle pi)
   (translate :z (- 3))
   (segment mount-bracket))

  (branch
   :from :body
   (translate :y (- motor-r 12))
   (rotate :axis :y :angle pi)
   (set :shape (m/circle 6) :to [:body])
   (forward :length 2)
   (set :shape (m/circle motor-axle-r))
   (forward :length 2)
   (set :shape (m/intersection (m/circle motor-axle-r) (m/square motor-axle-w motor-axle-r)))
   (forward :length 5))

  (forward :length motor-l :to [:body]))

(defmodel motor-mount
  (model :shape
         (m/union (m/intersection (m/circle (+ 2 motor-r))
                                  (->> (m/square (+ 4 (* 2 motor-r)) 32)
                                       (m/translate [0 5])))

                  (m/difference
                   (m/intersection
                    (->> (cosine-hill (+ 10 (* 6 motor-r))
                                      (+ 10 (* 2 motor-r))
                                      50)
                         (m/minkowski (m/circle 2.5)))
                    (->> (m/square 200 32)
                         (m/translate [0 5])))))


         :name :body
         :order 0)

  (model :shape (m/circle motor-r)
         :mask? true
         :name :mask
         :order 1)

  (rotate :x (- (/ pi 2)))
  (translate :y (- 21) :z (- 3/2))

  (branch
   :from :body
   (segment mount-bracket))

  (branch
   :from :body
   (rotate :y pi)
   (translate :z (- 3))
   (segment mount-bracket))

  (branch
   :from :body
   (translate :y (- motor-r 12))
   (rotate :y pi)
   (model :shape (m/circle 6) :name :mask)
   (backward :length 3.01 :to [:mask]))

  (translate :y 3/2 :to [:mask])
  (backward :length motor-l :to [:mask])
  (forward :length 3 :to [:body]))

(defmodel spin-joint
  (model :shape (cosine-hill (* 5 motor-axle-w)
                             (* 2.7 motor-axle-r)
                             50)
         :name :body
         :mask? false
         :order 0)

  (model :shape (m/intersection
                 (m/circle motor-axle-r)
                 (m/square motor-axle-w (* 2 motor-axle-r)))
         :name :mask
         :mask? true
         :order 1)

  (rotate :y (/ pi 2))
  (rotate :axis [1 0 0] :angle (- pi))
  (translate :z (- 4) :y (- 4))
  (forward :length 8))

(defmodel rotater
  (model :shape (m/union (m/square 8 3)
                         (->> (m/square 3 6)
                              (m/translate [-2.5 3/2])))
         :curve-radius 4
         :name :body)
  (set :fn 50)
  (forward :length 80)

  #_(branch
   :from :body
   (rotate :z (/ pi 2))
   (rotate :y (/ pi 2))
   (translate :y 2.5)
   (segment motor-mount))

  (set :shape nil :to [:mask])
  (forward :length 80)
  (left :curve-radius 4)
  (forward :length (/ 248 2))
  (branch
   :from :body
   (segment spin-joint))
  (set :shape nil :to [:mask])
  (forward :length (/ 248 2))
  (left :curve-radius 4)
  (forward :length 160)
  (left :curve-radius 4)
  (forward :length 248)
  (left :curve-radius 4))


(def suction-generator
  (path
   (body :shape (m/square (+ 40 1.6)
                          (+ 40 1.6))
         :name :body
         :fn 100)
   (forward :length 30)
   (forward :length 0.1)
   (set :shape (m/circle 5))
   (translate :z 60)
   (forward :length 0.1)
   (hull)
   (forward :length 30)))

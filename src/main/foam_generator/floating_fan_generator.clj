(ns foam-generator.floating-fan-generator
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [foam-generator.math :refer [cos sin pi pi|2 pi|3 pi|4 pi|5 pi|6 atan acos asin sqr sqrt tan |2 |3 |4]]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path
            model branch segment set arc result union difference intersection
            body mask spin lookup-transform rotate transform]]))

(def wall-thickness 1.6)

(def fan-w 40)
(def fan-h 40)
(def fan-l 20)
(def fan-r 18)

(def float-radius 60)
(def output-radius (u/in->mm 1/2))
(def output-angle pi|4)

(def fan-mount-angle 0)


(def foam-generator
  (path
   (body :name :origin :fn 60)

   (result (difference :fan-body :fan-mask :fan-input-mask))

   (segment
    (for [[side i] (map list [:left :right] (range 2))]
      (branch
       :from :origin
       (rotate :z (* i pi))
       (body :shape (m/circle float-radius)
             :name :fan-body)
       (forward :length 5)
       (set :shape (m/square (+ fan-w 1.6)
                             (+ fan-h 1.6))
            :to [:fan-body])
       (body :shape (m/square fan-w fan-h)
             :name :fan-mask)
       (translate :y 10)
       (rotate :x (- pi|2 fan-mount-angle))
       (forward :length fan-l)
       (translate :y 11)
       (set :shape (m/square (* 4 output-radius)
                             (* 2 output-radius))
            :to [:fan-body])
       (set :shape (m/circle (- output-radius wall-thickness))
            :to [:fan-mask])
       (forward :length 24.5)
       (hull :to [:fan-mask])
       (down :angle pi|2 :curve-radius 13)
       (backward :length 29 :to [:fan-body])
       (forward :length 29 :to [:fan-body])
       (hull :to [:fan-body] :n-segments 6)
       (when (= side :left)
         (segment
          (set :shape (m/circle output-radius)
               :to [:fan-body])
          (forward :length 15)))
       (forward :length 10 :to [:fan-mask]))))

   (branch
    :from :origin
    (body :shape (m/square fan-w fan-l)
          :name :fan-input-mask)
    (forward :length 20)
    (backward :length 36)
    (rotate :x (- pi|2))
    (forward :length 100))))

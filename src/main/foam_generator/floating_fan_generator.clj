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

(def wall-thickness 0.8)

(def fan-w 40)
(def fan-h 40)
(def fan-l 25)
(def fan-r 18)

(def float-radius 60)
(def output-radius (u/in->mm 1/2))
(def output-angle pi|4)

(def fan-mount-angle 0)


(def foam-generator
  (path
   (body :name :origin :fn 60)

   (result (difference :fan-body :fan-mask))

   (branch
    :from :origin
    (body :shape (m/circle float-radius)
          :name :fan-body)
    (forward :length 5)
    (set :shape (m/square (+ fan-w 1.6)
                          (+ fan-h 1.6))
         :to [:fan-body])
    (body :shape (m/circle fan-r)
          :name :fan-mask)
    (translate :y 15)
    (rotate :x (- pi|2 fan-mount-angle))
    (forward :length 20)
    (down :angle (- output-angle fan-mount-angle)
          :curve-radius 30 :gap true)
    (set :shape (m/circle output-radius)
         :to [:fan-body])
    (set :shape (m/circle (- output-radius wall-thickness))
         :to [:fan-mask])
    (forward :length 20)
    (hull :to [:fan-mask])
    (hull :to [:fan-body] :n-segments 3)
    (forward :length 20)
    (forward :length 1 :to [:fan-mask]))

   (branch
    :from :origin
    (body :shape (m/circle float-radius)
          :name :float-mask)
    (backward :length 20))))

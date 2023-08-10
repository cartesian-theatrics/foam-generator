(ns main.foam-generator.straw-generator
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [foam-generator.math :refer [cos sin pi pi|2 pi|3 pi|4 pi|5 pi|6 atan acos asin sqr sqrt tan |2 |3 |4]]
   [foam-generator.core :refer [tube-connector]]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [plexus.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path offset points
            save-transform to model branch segment set arc result union difference intersection
            body mask spin lookup-transform rotate transform]]))

(def wall-thickness (* 0.4 2))

(def straw-outer-radius 5)
(def straw-height)

(def straw-air-hole-radius 0.4)
(def straw-grap-radius 1/3)

(def straw-air-holes
  (path
   (result :name :straw-air-holes :expr :straw-air-hole-body)
   (body :shape (m/circle straw-air-hole-radius)
         :name :straw-air-hole-body)
   (segment
    (for [x (range )]))))

(def straw-generator
  (path
   (body :name :origin :fn 10)
   (branch
    :from :origin
    (body :name :outer-body :shape ))))

(def straw-generator-test
  (path
   (result :name :straw-generator
           :expr (difference :straw-body :straw-mask :air-hole-body))
   (body :name :straw-body
         :shape (m/circle (* 0.4 3)))
   (body :name :straw-mask
         :shape (m/circle 0.4))
   (forward :length 5)
   (branch
    :from :straw-body
    :with []
    (body :shape (m/circle straw-air-hole-radius)
          :name :air-hole-body)
    (rotate :x pi|2)
    (forward :length 50))
   (forward :length 20)))

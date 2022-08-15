(ns foam-generator.vacuum-generator
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [foam-generator.math :refer [cos sin pi pi|2 pi|3 pi|4 pi|5 pi|6 atan acos asin sqr sqrt tan |2 |3 |4]]
   [foam-generator.core :refer [tube-connector]]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path offset
            save-transform to model branch segment set arc result union difference intersection
            body mask spin lookup-transform rotate transform]]))

(def wall-thickness 0.8)

(def chamber-coupling-inner-radius 58/2)
(def chamber-outer-radius 50/2)
(def chamber-inner-radius (- chamber-outer-radius wall-thickness))
(def chamber-inner-offset 0)

(def top-cap-thread-offset 1)
(def chamber-threads-bottom-length 15)
(def chamber-threads-top-length 10)

(def twist-valve-outer-radius (+ chamber-outer-radius
                                 top-cap-thread-offset
                                 1.6))

(defmethod s/write-expr :screw-by-pitch
  [depth [_ pitch d0 dr length flat]]
  (list (apply str (repeat depth " "))
        "screwByPitch(" pitch ",d0=" d0 ",dr=" dr ",length=" length, ",flat=" flat ");\n"))

(defn screw-by-pitch
  ([& {:keys [pitch d0 dr length flat starts]
       :or {pitch 3.6
            d0 12
            dr 1.5
            length 12
            flat 0.6
            starts 1}}]
   `(:screw-by-pitch ~pitch ~d0 ~dr ~length ~flat ~starts)))

(defn slice-model
  [model start end]
  (m/difference
   (m/translate [0 0 (- start)] model)
   (m/union
    (->> (m/cube 1000 1000 1000)
         (m/translate [0 0 (+ 500 (- end start))]))
    (->> (m/cube 1000 1000 1000)
         (m/translate [0 0 (- 500)])))))

(def chamber-bottom-threads-outer
  (screw-by-pitch
   :flat 1
   :d0 (+ 2.2 (- (* 2 chamber-inner-radius) (* 2 top-cap-thread-offset)))
   :length chamber-threads-bottom-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-top-threads-inner
  (screw-by-pitch
   :flat 1
   :d0 (* 2 chamber-inner-radius)
   :length chamber-threads-top-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-top-threads-inner-bottom-slice
  (slice-model chamber-top-threads-inner 0 0.01))

(def chamber-top-threads-outer-bottom-slice
  (slice-model chamber-bottom-threads-outer 0.0 0.02))

(def chamber-tube-connector-top
  (path
   (body :shape (m/circle chamber-inner-radius)
         :name :body
         :fn 100)
   (mask :shape (m/circle 1.75) :name :mask)
   (forward :length 1.2)
   (segment tube-connector)))

(def chamber-tube-connector-bottom
  (path
   (body :shape (m/circle (- chamber-inner-radius top-cap-thread-offset))
         :name :body
         :fn 100)
   (mask :shape (m/circle 3)
         :name :mask)
   (forward :length 1.2)
   (to
    :models [:body]
    (forward :length 0.01)
    (translate :z 2)
    (forward :length 0.01 :model chamber-top-threads-outer-bottom-slice)
    (hull)
    (forward :length chamber-threads-bottom-length :model chamber-bottom-threads-outer))))

(def main-chamber-threads
  (screw-by-pitch
   :flat 1
   :d0 (- (* 2 chamber-inner-radius) (* 2 top-cap-thread-offset))
   :length chamber-threads-bottom-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def main-chamber-threads-top
  (slice-model main-chamber-threads (- chamber-threads-bottom-length 0.01) chamber-threads-bottom-length))

(def tube-connection
  (path
   (body :shape (m/circle (/ 9.2 2)) :name :tube-mask
         :fn 100)
   (segment
    (for [_ (range 3)]
      (segment
       (forward :length 1)
       (offset :offset -0.25)
       (forward :length 3)
       (hull)
       (offset :offset 0.25))))
   (offset :offset -1)
   (forward :length 3.01)))

(* 4 3)

(def main-chamber
  (path
   (body :shape (m/circle (- chamber-inner-radius
                             top-cap-thread-offset))
         :name :body
         :fn 100)
   (mask :shape (m/circle (- chamber-inner-radius
                             top-cap-thread-offset
                             2.5))
         :name :mask)
   (result (difference :body :mask :tube-mask))
   (forward :length chamber-threads-bottom-length :to [:mask]
            :model tube-connection)
   (forward :length chamber-threads-bottom-length :to [:body]
            :model main-chamber-threads
            )

   (forward :length 0.01 :model main-chamber-threads-top :to [:body])
   (translate :z 1.98 :to [:body])
   (forward :length 0.01 :to [:body])
   (hull :to [:body])
   (save-transform :model :body :name ::twist-valve-start)
   (u/curve-segment-2
      :bottom-radius (- chamber-inner-radius top-cap-thread-offset)
      :height 4
      :offset top-cap-thread-offset
      :to [:body])
   (u/curve-segment-2
      :bottom-radius (- chamber-inner-radius top-cap-thread-offset 1.6)
      :height 6
      :offset (+ top-cap-thread-offset 0.9)
      :to [:mask])
   (backward :length 0.02 :to [:mask])
   (forward :length 0.02 :to [:mask])
   (segment
      (forward :length 6)
      (segment
       (for [i (range 4)]
         (branch
          :from :body
          (to
           :models [:mask]
           (set :shape (m/hull (->> (m/circle 1)
                                    (m/translate [0 2.5]))
                               (->> (m/circle 1)
                                    (m/translate [0 -2.5]))) )
           (rotate :z (* i 1/4 pi))
           (rotate :x pi|2)
           (forward :length 200 :center true)))))

      (forward :length 6)
      (to
       :models [:body]
       (forward :length 0.01)
       (translate :z 2.98)
       (forward :length 0.01 :to [:body]
                :model chamber-top-threads-inner-bottom-slice)
       (hull)
       (forward :length chamber-threads-top-length :to [:body]
                :model chamber-top-threads-inner))
      (u/curve-segment-2
       :bottom-radius (- chamber-inner-radius 1.6)
       :offset -2
       :height 4
       :to [:mask])
      (forward :length chamber-threads-top-length :to [:mask])
      (save-transform :model :body :name ::screen-start))))

(def twist-valve
  (path
   (body :shape (m/circle))))

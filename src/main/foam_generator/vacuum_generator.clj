(ns foam-generator.vacuum-generator
  (:require
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [foam-generator.math :refer [cos sin pi pi|2 pi|3 pi|4 pi|5 pi|6 atan acos asin sqr sqrt tan |2 |3 |4]]
   [foam-generator.core :refer [tube-connector]]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path offset points
            save-transform to model branch segment set arc result union difference intersection
            body mask spin lookup-transform rotate transform]]))

(def tmp
  (path
   (body :name :a :shape (m/circle 10) :fn 80)
   (mask :name :b :shape (m/circle 8))
   #_(result (difference :a :b))
   (forward :length 20)))

(def wall-thickness 0.8)

(def chamber-coupling-inner-radius 58/2)
(def chamber-coupling-outer-radius
  (+ chamber-coupling-inner-radius 1.2))
(def chamber-outer-radius 20/2)
(def chamber-inner-radius (- chamber-outer-radius wall-thickness))
(def chamber-inner-offset 0)

(def top-cap-thread-offset 1)
(def chamber-threads-bottom-length 15)
(def chamber-threads-top-length 10)

(def twist-valve-outer-radius (+ chamber-inner-radius
                                 1.6))
(def twist-valve-thread-length 7)
(def twist-valve-bottom-flang-length 5)
(def twist-valve-top-flang-length 20)

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

(def twist-valve-thread-mask
  (screw-by-pitch
   :flat 1
   :d0 (+ 3/4 (- (* 2 chamber-inner-radius) (* 2 top-cap-thread-offset)))
   :length twist-valve-thread-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-top-threads
  (screw-by-pitch
   :flat 1
   :d0 (* 2 chamber-inner-radius)
   :length chamber-threads-top-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-top-threads-bottom-slice
  (slice-model chamber-top-threads 0 0.01))

(def twist-valve-thread-mask-bottom-slice
  (slice-model twist-valve-thread-mask 0.0 0.02))

(def twist-valve-thread-mask-top-slice
  (slice-model twist-valve-thread-mask
               (- twist-valve-thread-length 0.01)
               twist-valve-thread-length))

(def chamber-bottom-threads
  (screw-by-pitch
   :flat 1
   :d0 (- (* 2 chamber-inner-radius) (* 2 top-cap-thread-offset))
   :length chamber-threads-bottom-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-bottom-threads-top-slice
  (slice-model chamber-bottom-threads (- chamber-threads-bottom-length 0.01) chamber-threads-bottom-length))

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
            :model chamber-bottom-threads)

   (forward :length 0.01 :model chamber-bottom-threads-top-slice :to [:body])
   (translate :z 1.98 :to [:body])
   (forward :length 0.01 :to [:body])
   (hull :to [:body])
   (save-transform :model :body :name ::twist-valve-start)
   (u/curve-segment-2
    :bottom-radius (- chamber-inner-radius top-cap-thread-offset)
    :height 4
    :offset top-cap-thread-offset
    :to [:body])
   (forward :length 6 :to [:mask])
   #_(u/curve-segment-2
      :bottom-radius (- chamber-inner-radius top-cap-thread-offset 1.6)
      :height 6
      :offset (+ top-cap-thread-offset 0.9)
      :to [:mask])
   (backward :length 0.02 :to [:mask])
   (forward :length 0.02 :to [:mask])
   (segment
    (forward :length 6)
    (segment
     (for [i (range 8)]
       (branch
        :from :body
        (to
         :models [:mask]
         (set :shape (m/hull (->> (m/circle 1/4)
                                  (m/translate [0 3.25]))
                             (->> (m/circle 1/4)
                                  (m/translate [0 -3.25]))) )
         (rotate :z (* i 1/8 pi))
         (rotate :x pi|2)
         (forward :length 200 :center true)))))

    (forward :length 6)
    (to
     :models [:body]
     (forward :length 0.01)
     (translate :z 2.98)
     (forward :length 0.01 :to [:body]
              :model chamber-top-threads-bottom-slice)
     (hull)
     (forward :length chamber-threads-top-length :to [:body]
              :model chamber-top-threads))
    (forward :length 4 :to [:mask])
    #_(u/curve-segment-2
     :bottom-radius (- chamber-inner-radius 1.6)
     :offset -2
     :height 4
     :to [:mask])
    (forward :length chamber-threads-top-length :to [:mask])
    (save-transform :model :body :name ::screen-start))))

(def twist-valve
  (path
   (body :shape (m/circle twist-valve-outer-radius)
         :name :body
         :fn 100)
   (mask :shape (m/circle (- twist-valve-outer-radius 1.6))
         :name :mask)
   (result (difference :body :mask))
   (forward :length (- twist-valve-bottom-flang-length 1.5))
   (forward :length 0.01)
   (translate :z 1.48)
   (forward :length 0.01 :to [:mask] :model twist-valve-thread-mask-bottom-slice)
   (forward :length 0.01 :to [:body])
   (hull)
   (forward :length twist-valve-thread-length :to [:body])
   (forward :length twist-valve-thread-length :to [:mask] :model twist-valve-thread-mask)
   (forward :length 0.01 :to [:mask] :model twist-valve-thread-mask-top-slice)
   (forward :length 0.01 :to [:body])
   (translate :z 1.48)
   (forward :length 0.01)
   (hull)
   (forward :length (- twist-valve-top-flang-length 1.5))))

(def screen-top-tpu
  (path
   (body :shape (m/circle (- chamber-inner-radius 1.6))
         :name :body
         :fn 100)
   #_(segment
      (for [i (range 3)
            :let [d 6
                  r (+ 4 (* i (/ (- chamber-inner-radius 5) 3)))
                  c (* 2 pi r)
                  n-holes (quot c d)]
            j (range  n-holes)]
        (branch
         :from :body
         (rotate :z (* j (/ (* 2 pi) n-holes)))
         (translate :x r)
         (set :shape (m/circle 2))
         (forward :length 2.1)
         (set :mask? true))))
   (forward :length 1.2)
   (mask :shape (m/circle (- chamber-inner-radius 1.6 1.2))
         :name :mask)
   (forward :length 6.4)))

(def screen-top
  (path
   (body :shape (m/circle (- chamber-inner-radius 1.6))
         :name :body
         :fn 100)
   #_(segment
      (for [i (range 3)
            :let [d 6
                  r (+ 4 (* i (/ (- chamber-inner-radius 5) 3)))
                  c (* 2 pi r)
                  n-holes (quot c d)]
            j (range  n-holes)]
        (branch
         :from :body
         (rotate :z (* j (/ (* 2 pi) n-holes)))
         (translate :x r)
         (set :shape (m/circle 2))
         (forward :length 2.1)
         (set :mask? true))))
   (mask :shape (m/circle (- chamber-inner-radius 5))
         :name :mask)
   (forward :length 1.2)
   (set :shape (m/circle (- chamber-inner-radius 1.6 1.2))
        :to [:mask])

   (forward :length 7)))

(def inner-screen-ring
  (path
   (body :shape (m/circle (- chamber-inner-radius 1.6 1.2 0.15))
         :name :body
         :fn 100)
   (mask :shape (m/circle (- chamber-inner-radius 1.6 1.2 0.15 1.2))
         :name :mask)
   (forward :length (- 7 1.2))))

(def chamber-top-threads-mask-length
  (- chamber-threads-top-length 1/2))

(def chamber-top-threads-mask
  (screw-by-pitch
   :flat 1
   :d0 (+ 1/2 (* 2 chamber-inner-radius))
   :length chamber-top-threads-mask-length
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def chamber-top-threads-mask-top-slice
  (slice-model chamber-top-threads-mask
               (- chamber-top-threads-mask-length 0.01)
               chamber-top-threads-mask-length))

(def vacuum-coupling
  (path
   (body :name :origin :fn 60)

   (result (union
            (difference :thread-body :thread-mask)
            (difference :coupling-outer :coupling-inner :thread-mask)))

   (branch
    :from :origin
    (body :shape (m/circle (+ chamber-coupling-inner-radius 1.6))
          :name :coupling-outer)
    (forward :length 2)
    (body :shape (m/circle chamber-coupling-inner-radius)
          :name :coupling-inner)
    (forward :length 30))

   (branch
    :from :origin
    (body :shape (m/circle (- chamber-inner-radius 1.5))
          :name :thread-mask)
    (body :shape (m/circle (+ 3 chamber-inner-radius))
          :name :thread-body)
    (forward :length chamber-top-threads-mask-length
             :model chamber-top-threads-mask
             :to [:thread-mask])
    (forward :length chamber-top-threads-mask-length
             :to [:thread-body])
    (forward :length 0.01
             :to [:thread-mask]
             :model chamber-top-threads-mask-top-slice)
    (forward :length 0.01
             :to [:thread-body])
    (translate :z 3)
    (set :shape (m/circle (+ chamber-inner-radius 1/2))
         :to [:thread-body])
    (forward :length 0.01)
    (hull)
    (forward :length 4)
    (forward :length 0.01)
    (offset :offset -1.5)
    (translate :z 2.98)
    (forward :length 0.01)
    (hull))))

(def distance-between-walls 1/2)
(def wall-thickness 1.2)

(def vacuum-coupling-v2
  (path
   (body :name :origin :fn 30)

   (result
    (union
     (difference :outer_wall_body
                 :outer_wall_mask)
     (difference :inner_wall_body
                 :inner_wall_mask)))

   (branch
    :from :origin
    (body :shape (m/circle 12) :name :outer_wall_body)
    (body :shape (m/circle (- 12 wall-thickness)) :name :outer_wall_mask)
    (body :shape (m/circle (- 12 wall-thickness distance-between-walls)) :name :inner_wall_body)
    (body :shape (m/circle (- 12 wall-thickness distance-between-walls wall-thickness)) :name :inner_wall_mask)
    (forward :length 15)
    (segment
     (for [[model offset] [[:outer_wall_body 0]
                           [:outer_wall_mask (- wall-thickness)]
                           [:inner_wall_body (- (+ wall-thickness distance-between-walls))]
                           [:inner_wall_mask (- (+ (* 2 wall-thickness) distance-between-walls))]]]
       (segment
        (u/curve-segment-2
         :bottom-radius (+ 12 offset)
         :offset (- chamber-coupling-outer-radius 12)
         :height 45
         :curve-offset offset
         :to [model])
        (forward :length 20 :to [model])
        (cond
          (or (= model :inner_wall_body)
              (= model :inner_wall_mask))
          (to
           :models [model]
           (forward :length 0.01)
           (scad-paths.core/offset :offset -1)
           (translate :z 4)
           (forward :length 0.01)
           (hull)
           (forward :length 4))

          (or (= model :outer_wall_mask)
              (= model :outer_wall_body))
          (to
           :models [model]
           (forward :length 30)))
        (forward :length 10 :to [model])))))))

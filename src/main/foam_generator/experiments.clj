(ns main.foam-generator.experiments
  (:require
   [foam-generator.core :as c]
   [foam-generator.utils :as u]
   [foam-generator.params :as p]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]
   [scad-paths.core :as paths
    :refer [forward hull left right up down roll backward defmodel translate path
            body mask
            model branch segment set arc
            spin lookup-transform rotate transform]]))


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

(def reservoir-threads
  (screw-by-pitch
   :flat 1
   :d0 101
   :length 12
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def reservoir-threads-mask
  (screw-by-pitch
   :flat 1
   :d0 101.5
   :length 12
   :dr 1.5
   :pitch 3.8
   :starts 1))

(def reservoir-threads-bottom
  (slice-model reservoir-threads 0.0 0.02))

(def siphon-reservoir
  (path
   (body :name :origin :fn 80)

   (branch
    :from :origin
    (body :shape (m/circle 50) :name :outer-reservoir-body)
    (forward :length 1.2)
    (mask :shape (m/circle 48.4) :name :outer-reservoir-mask)
    (forward :length 50)
    (forward :length 0.1)
    (translate :z 2)
    (forward :length 0.02 :model reservoir-threads-bottom :to [:outer-reservoir-body])
    (forward :length 0.02 :to [:outer-reservoir-mask])
    (hull)
    (forward :length 12
             :model reservoir-threads
             :to [:outer-reservoir-body])
    (forward :length 12 :to [:outer-reservoir-mask]))))

(def siphon-reservoir-lid
  (path
   (body :name :origin :fn 80)

   (segment
    (for [x [-30 30]]
      (branch
       :from :origin
       (mask :shape (m/circle 6) :name :tube-hole)
       (translate :x x)
       (forward :length 5))))

   (branch
    :from :origin
    (body :shape (m/circle 54) :name :lid-outer)
    (forward :length 3)
    (mask :name :thread-mask)
    (forward :length 10
             :model reservoir-threads-mask
             :to [:thread-mask])
    (forward :length 10 :to [:lid-outer]))))

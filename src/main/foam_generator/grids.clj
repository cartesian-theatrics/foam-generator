(ns foam-generator.grids
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

;; Approaches:
;; 1. blow holes in a square
;; 2. Define grid as single polygon
;;
;; Problems:
;; - Both are painfully slow to render
;; - Even caching a single grid layer and translating the
;;   copies of the cached layer is desperately slow.
;;
;; Questions:
;; - Are there any other approaches to try?
;; - Can we use a different shape to accomplish the same goal?
;;
;; New Approaches?
;; 1. blow holes in cube at angles.
;; 2. Exploit rotational symmetry? The structure has insane symmetry, but so far we are
;;    unable to exploit this.
;; 3. Extrude up and from the side?


(defn grid-2d
  ([rows cols hole-size center? offset?]
   (let [w (* 2 hole-size rows)
         h (* 2 hole-size cols)]
     (m/difference
      (m/square w h :center center?)
      (for [x (range (if offset? hole-size 0) (+ 0.01 w (if offset? (- hole-size) 0)) (* 2 hole-size))
            y (range (if offset? hole-size 0) (+ 0.01 h (if offset? (- hole-size) 0)) (* 2 hole-size))]
        (->> (m/square hole-size hole-size :center true)
             (m/translate [(if center? (- x (/ w 2)) x)

                           (if center? (- y (/ h 2)) y)]))))))
  ([rows cols hole-size center?]
   (grid-2d rows cols hole-size center? false))
  ([rows cols hole-size]
   (grid-2d rows cols hole-size true)))

(defn grid-3d
  ([levels rows cols hole-size center?]
   (let [support (m/cube hole-size hole-size (- (* 2 hole-size levels) hole-size) :center false)
         w (* 2 hole-size rows)
         h (* 2 hole-size cols)
         z (* 2 hole-size levels)]
     (m/union
      (m/difference
       (->> (m/square w h :center true)
            (m/extrude-linear {:height z :center false}))
       (->> (m/square (- w 2) (- h 2) :center true)
            (m/extrude-linear {:height z :center false})
            (m/translate [0 0 1])))
      (concat
       (for [x (range (* 3/2 hole-size) (+ 0.01 w (- (* 2 hole-size))) (* 2 hole-size))
             y (range (* 1/2 hole-size) (+ 0.01 h (- (* 1 hole-size))) (* 2 hole-size))]
         (->> support
              (m/translate [(if center? (- x (/ w 2)) x)
                            (if center? (- y (/ h 2)) y)])))
       (for [l (range levels)]
         (->> (grid-2d rows cols hole-size center? (odd? l))
              (m/extrude-linear {:height hole-size :center false})
              (m/translate [0 0 (* 2 hole-size l)])))))))
  ([levels rows cols hole-size]
   (grid-3d levels rows cols hole-size true)))

(->> (grid-3d 6 10 10 0.8)
     (s/write-scad)
     (spit "test.scad"))

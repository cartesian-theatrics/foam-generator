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
  ([rows cols hole-size density center? offset?]
   (let [w (* 2 hole-size rows)
         h (* 2 hole-size cols)
         step (/ hole-size density)]
     (m/difference
      (m/square w h :center center?)
      (for [x (range (if offset? (/ step 2) 0) (+ 0.01 w (if offset? (- hole-size) 0)) step)
            y (range (if offset? (/ step 2) 0) (+ 0.01 h (if offset? (- hole-size) 0)) step)]
        (->> (m/square hole-size hole-size :center center?)
             (m/translate [(if center? (- x (/ w 2)) x)
                           (if center? (- y (/ h 2)) y)])))))))


(defn grid-3d
  ([levels rows cols hole-size density center?]
   (let [support (m/cube hole-size hole-size (- (* 2 hole-size levels) hole-size) :center false)
         w (* 2 hole-size rows)
         h (* 2 hole-size cols)
         z (* 2 hole-size levels)]
     (m/union
      (concat
       #_(for [x (range (* 3/2 hole-size) (+ 0.01 w (- (* 2 hole-size))) (* 2 hole-size))
             y (range (* 1/2 hole-size) (+ 0.01 h (- (* 1 hole-size))) (* 2 hole-size))]
         (->> support
              (m/translate [(if center? (- x (/ w 2)) x)
                            (if center? (- y (/ h 2)) y)])))
       (for [l (range levels)]
         (->> (grid-2d rows cols hole-size density center? (odd? l))
              (m/extrude-linear {:height hole-size :center false})
              (m/translate [0 0 (* 2 hole-size l)]))))))))

(defn fast-grid-3d
  [levels rows cols cell-size]
  (let [line (->> (u/polyline [[0 0] [0 (* 2 cell-size cols)]] (/ cell-size 2))
                  (m/extrude-linear {:height cell-size :center false}))]
    (m/union
     (for [z (range 0 levels)
           y (range 0 rows)]
       (if (odd? z)
         (->> line
              (m/rotatec [0 0 (/ Math/PI 2)])
              (m/translate [(* 2 rows cell-size) (* 2 cell-size y) (* 1 cell-size z)]))
         (->> line
              (m/translate [(* 2 cell-size y) 0 (* 1 cell-size z)])))))))

(comment
  (->> (fast-grid-3d 6 10 15 0.8)
       (s/write-scad)
       (spit "test.scad"))

  )

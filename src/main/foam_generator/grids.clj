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


(defn z-support-sides [cell-size center?]
  (let [side (->> (m/square cell-size (* 4 cell-size) :center true)
                  (m/extrude-linear {:height  (* 2 cell-size)  :center false})
                  (m/translate [cell-size 0 (- (* 2 cell-size))]))]
    (m/union side (m/rotatec [0 0 Math/PI] side))))

(->> (z-support-sides 2 true)
     (s/write-scad)
     (spit "test.scad"))

(defn grid-layer
  ([rows cols hole-size density center? offset?]
   (let [w (* 2 hole-size rows)
         h (* 2 hole-size cols)
         step (/ hole-size density)
         holes-and-supports (for [[rotate? x]
                                  (map list
                                       (cycle [false true])
                                       (range (if offset? (/ step 2) 0) (+ 0.01 w (if offset? (- hole-size) 0)) step))
                                  y (range (if offset? (/ step 2) 0) (+ 0.01 h (if offset? (- hole-size) 0)) step)]
                              (let [tr (partial m/translate [(if center? (- x (/ w 2)) x)
                                                             (if center? (- y (/ h 2)) y)])]
                                [(tr (cond->> (z-support-sides hole-size center?)
                                       rotate? (m/rotatec [0 0 (/ Math/PI 2)])))
                                 (tr (->> (m/square hole-size hole-size :center center?)
                                          (m/extrude-linear {:height hole-size :center false})))]))
         holes (map second holes-and-supports)
         supports (map first holes-and-supports)]
     (-> (->> (m/square w h :center center?)
              (m/extrude-linear {:height hole-size :center false}))
         (m/difference holes)
         (m/union supports)))))

(defn grid-3d
  ([levels rows cols hole-size density center?]
   (let [support (m/cube hole-size hole-size (- (* 2 hole-size levels) hole-size) :center false)
         w (* 2 hole-size rows)
         h (* 2 hole-size cols)
         z (* 2 hole-size levels)]
     (m/union
      (for [l (range levels)]
        (->> (grid-layer rows cols hole-size density center? (odd? l))
             (m/translate [0 0 (* 2 hole-size l)])))))))

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

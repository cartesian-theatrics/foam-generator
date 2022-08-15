(ns foam-generator.utils
  (:require
   [scad-clj.scad :as s]
   [scad-clj.model :as m]
   [malli.core :as malli]
   [foam-generator.math :refer [cos sin pi pi|2 pi|3 pi|4 pi|5 pi|6 atan acos asin sqr sqrt tan |2 |3 |4]]
   [scad-paths.core :as paths
    :refer [model forward hull left right up down roll backward defmodel defpoly poly
            translate set segment lookup-transform pattern transform branch offset
            minkowski rotate body mask save-transform spin
            union intersection difference result ignore points path]]
   [clojure.pprint :refer [pprint]]))

(defmethod s/write-expr :shell
  [depth [_ d center? block]]
  (concat
   (repeat depth " ")
   (list "shell(" d ", center=" center? ") {\n")
   (mapcat #(s/write-expr (inc depth) %) block)
   (repeat depth " ")
   (list "}")))

(defn shell [d center? & block]
  `(:shell ~d ~center? ~block))

(defn belt-circumference
  "Length of a belt with large wheel (`dl`) and a smaller wheel (`ds`), separated by
distance `l`."
  [dl ds l]
  {:pre [(>= dl ds)]}
  (+ (* (+ dl ds) (/ Math/PI 2))
     (* (- dl ds) (Math/asin (/ (- dl ds) (* 2 l))))
     (* 2 (Math/sqrt (- (Math/pow l 2)
                        (* 0.25 (Math/pow (- dl ds) 2)))))))

(defn ovol
  [rx ry]
  (let [n-steps (or m/*fn* 100)]
    (m/polygon
     (for [x (range n-steps)]
       (let [d (* x (/ (* 2 Math/PI) n-steps))]
         [(* rx (Math/cos d))
          (* ry (Math/sin d))])))))


(defn radius-from-circumference [c]
  (/ c (* Math/PI 2)))

(defn loft-shell
  [square circle hole-pattern length thickness]
  (let [x (:x square)
        y (:y square)
        r (:r circle)
        shape-thickness 1
        hole-radius (:radius hole-pattern)
        hole-rows (:rows hole-pattern)
        initial-hole-offset (:initial-offset hole-pattern)
        hole-offset (:offset hole-pattern)
        hole-cylinder (->> (m/cylinder hole-radius (+ 100 (max x y (* 2 r))))
                           (m/rotatec [0 (/ Math/PI 2) 0]))
        outer (m/hull (->> (m/cylinder r 1 :center true))
                      (->> (m/cube x y 1 :center true)
                           (m/translate [0 0 (+ length shape-thickness)])))]
    (m/difference
     (m/union
      outer
      (->> (m/difference (m/cylinder r 15 :center false)
                         (m/cylinder (- r shape-thickness) 15 :center false))
           (m/translate [0 0 -15])))
     (m/union
      (m/hull (->> (m/cylinder (- r thickness) 1 :center true))
              (->> (m/cube (- x thickness) (- y thickness) 1 :center true)
                   (m/translate [0 0 (+ length shape-thickness)])))
      (for [row (range hole-rows)
            column (range (- initial-hole-offset 1.5) y hole-offset)]
        (let [y-pos (+ (- (/ y 2)) column)]
          (->> hole-cylinder
               (m/translate [0
                             y-pos
                             (- length
                                initial-hole-offset
                                (* row hole-offset))]))))
      (for [row (range hole-rows)
            column (range (- initial-hole-offset 1) x hole-offset)]
        (let [x-pos (+ (- (/ x 2)) column)]
          (->> hole-cylinder
               (m/rotatec [0 0 (/ Math/PI 2)])
               (m/translate [x-pos
                             0
                             (- length
                                initial-hole-offset
                                (* row hole-offset))]))))))))

(defn loft
  [square circle length thickness]
  (let [x (:x square)
        y (:y square)
        r (:r circle)
        shape-thickness 1]
    (m/union
     (m/hull (->> (m/cylinder (- r thickness) 15 :center true)
                  (m/translate [0 0 -7.5]))
             (->> (m/cube (- x thickness) (- y thickness) 1 :center true)
                  (m/translate [0 0 (+ length shape-thickness)]))))))

(defn cylinder-shell
  [inner-r outer-r h & {:keys [center] :or {center false}}]
  (m/difference
   (m/cylinder outer-r h :center center)
   (m/cylinder inner-r h :center center)))

(defn line [p1 p2 thickness]
  (m/hull (m/translate p1 (m/square (* 2 thickness) (* 2 thickness) :center true))
          (m/translate p2 (m/square (* 2 thickness) (* 2 thickness) :center true))))

(defn polyfill [[p1 p2 & pts] thickness]
  (when p2
    (m/hull (line p1 p2 thickness)
            (polyfill (cons p2 pts) thickness))))

(defn polyline [[p1 p2 & pts] thickness]
  (when p2
    (m/union (line p1 p2 thickness)
             (polyline (cons p2 pts) thickness))))

(defn curve [r angle thickness]
  (m/polygon
   (concat
    (reverse
     (for [x (range 0 angle (/ angle 50))]
       [(* (- r thickness) (Math/cos x)) (* (- r thickness) (Math/sin x))]))
    (for [x (range 0 angle (/ angle 50))]
      [(* r (Math/cos x)) (* r (Math/sin x))]))))

(defn curve-points [r angle]
  (for [x (range (- (/ angle 2)) (/ angle 2) (/ angle 50))]
    [(* r (Math/cos x)) (* r (Math/sin x)) ]))

(defn semi-circle [r angle]
  (m/hull
   (m/polygon (cons [0 0]
                    (for [x (range 0 angle (/ angle 100))]
                      [(* r (Math/cos x)) (* r (Math/sin x)) ])))))

(defn translatev [angle offset block]
  (let [tx (* offset (Math/cos angle))
        ty (* offset (Math/sin angle))]
    (->> block
         (m/rotatev angle [0 0 1])
         (m/translate [tx ty]))))

(defn slope
  [[[x1 y1] [x2 y2]]]
  (/ (- y2 y1)
     (- x2 x1)))

#_(defmacro
  ^{:style/indet [1 [[:defn]] :form]}
  defmodule
  [name args body]
  `(do (defn ~name ~args
         (~(keyword name) ~@args))
       (defmethod s/write-scad ~(keyword name)
         [indent# [_ args# body#]]
         (concat (repeat indent# " ")
                 ~(str name)
                 "(" ~a ")"))
       (m/define-module ~(str name) )))

(defn irange
  ([start end step]
   (take-while (if (pos? step) #(<= % end) #(>= % end)) (iterate #(+ % step) start)))
  ([start end]
   (irange start end 1))
  ([end]
   (irange 0 end))
  ([] (range)))

(defn extrude-linear-stepwise
  [{:keys [step-size step-offset twist height] :as args} & block]
  (let [e (m/union block)
        n-steps (/ (* 1 height) step-size)
        twist-step-size (/ twist n-steps)
        ]
    (for [step (next (range n-steps))
          :when (even? step)]
      (let [rot (- (* step twist-step-size))
            z (* step step-size)]
        (->> e
             (m/rotatec [0 0 rot])
             (m/extrude-linear {:height step-size :twist twist-step-size :center false})
             (m/translate [0 0 z]))))))

(defn rotate-extrude
  [{:keys [twist height step-size]} & block]
  (let [b (m/extrude-linear {:height step-size :center false} (m/union block))
        n-steps (/ (* 1 height) step-size)
        twist-step-size (/ twist n-steps)]
    (m/union
     (for [step (range n-steps)]
       (let [rot (- (* step twist-step-size))
             z (* step step-size)]
         (->> b
              (m/rotatec [0 0 rot])
              (m/translate [0 0 z])))))))


(defn in->mm [inches]
  (* inches 25.4))

(defn ft->mm [ft]
  (* ft 12 25.4))

(defn mm->in [mm]
  (/ mm 25.4))



(defn curve-points [& {:keys [bottom-radius top-radius offset curve-angle curve-radius]}]
  (let [bottom-radius (+ bottom-radius offset)
        top-radius (+ top-radius offset)
        base-top-offset (- bottom-radius top-radius)
        curves-width (* 2 (- curve-radius (* (cos curve-angle) curve-radius)))
        width-between-curves (- base-top-offset curves-width)
        extrude-length (/ width-between-curves (sin curve-angle))]
    (points
     (body :name :body :fn 100)
     (rotate :z (- pi|2))
     (rotate :y (- pi|2))
     (rotate :x (- pi|2))
     (forward :length bottom-radius)
     (rotate :x pi|2)
     (up :angle curve-angle :curve-radius (+ curve-radius offset))
     (forward :length extrude-length)
     (down :angle curve-angle :curve-radius (- curve-radius offset))
     (rotate :x pi|2)
     (forward :length top-radius))))

(defn curve-segment
  [& {:keys [offset
             height
             curve-offset]
      :or {curve-offset 0}}]
  (let [h (/ height 2)
        w (/ offset 2)
        b (atan (/ h w))
        c (atan (/ w h))
        d (- b c)
        r (/ h (cos d))
        a (cond-> (asin (/ h r))
            (pos? offset) -)]
    (segment
     (left :angle a :curve-radius (+ r curve-offset))
     (right :angle a :curve-radius (- r curve-offset)))))

(defn one-curve
  [& {:keys [offset
             height
             curve-offset]
      :or {curve-offset 0}}]
  (let [h height
        w offset
        b (atan (/ h w))
        c (atan (/ w h))
        d (- b c)
        r (/ h (cos d))
        a (cond-> (asin (/ h r))
            (pos? offset) -)]
    (segment
     (rotate :y a)
     (left :angle a :curve-radius (+ r curve-offset)))))


(defn curve-segment-2 [& {:keys [bottom-radius offset curve-offset height to]
                          :or {curve-offset 0}}]
  (segment
   (paths/offset :offset offset :to to)
   (forward :length height
            :to to
            :model (m/with-fn 100
                     (m/extrude-rotate
                      {:angle 360}
                      (m/polygon
                       (points
                        :axes [:x :y]
                        (body :name :body :fn 30)
                        (rotate :x (- pi|2))
                        (forward :x bottom-radius)
                        (curve-segment :offset offset :height height :curve-offset curve-offset)
                        (forward :x (- (+ bottom-radius offset))))))))))

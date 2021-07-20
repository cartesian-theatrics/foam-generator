(ns main.foam-generator.core
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(Math/round 1.1)
(comment

  (let [x-length 36
        y-length 36
        z-length 5
        hole-x 1
        hole-y 1
        hole-z 1
        line-thickness 1
        x-line (u/polyline [[0 0] [x-length 0]] (/ line-thickness 2))
        y-line (u/polyline [[0 0] [0 y-length]] (/ line-thickness 2))
        grid (->> (m/union
                   (concat
                    (for [x (range 0 x-length (+ line-thickness hole-x))]
                      (->> y-line (m/translate [x 0])))
                    (for [y (range 0 x-length (+ line-thickness hole-y))]
                      (->> x-line (m/translate [0 y])))))
                  (m/extrude-linear {:height 1 :center false}))

        all-grids (m/union
                   (for [z (range 0 z-length (+ line-thickness hole-z))]
                     (cond->> grid
                       true (m/translate [0 0 z])
                       (even? (Math/round (float (/ z 2)))) (m/translate [hole-x hole-y 0]))))
        cylinder (m/union
                  (m/intersection
                   (m/translate [(- (/ x-length 2)) (- (/ y-length 2)) 0] all-grids)
                   (m/cylinder 15 z-length :center false))
                  #_(m/difference
                     (m/cylinder 15 z-length :center false)
                     (m/cylinder 14 z-length :center false)))]
    (->> cylinder
         (s/write-scad)
         (spit "test.scad")))

  )


(defn flat-wedge [o r angle]
  (m/difference
   (u/semi-circle r angle)
   (m/polygon [[0 0] [0 o]  [o 0]])))

(defn reservoir-side [o r h]
  (->> (u/polyline [[o 0]
                    [r 0] [(+ r (* h (Math/cos (/ Math/PI 4)))) (* h (Math/sin (/ Math/PI 4)))]
                    [(* h (Math/cos (/ Math/PI 4))) (+ r (* h (Math/sin (/ Math/PI 4))))]
                    [0 r]
                    [0 o]
                    [o 0]]
                   1/2)))

(defn reservoir-walls [o r h]
  (m/union (u/polyline [[o 0]
                        [r 0]
                        [(+ r (* h (Math/cos (/ Math/PI 4)))) (* h (Math/sin (/ Math/PI 4)))]]
                       1/2)
           (u/polyline [[o 0]
                        [0 o]
                        [0 r]
                        [(* h (Math/cos (/ Math/PI 4))) (+ r (* h (Math/sin (/ Math/PI 4))))]]
                       1/2)))

(Math/atan (/ Math/PI 20))

(defn foam-chamber [bottom-width radius height offset]
  (let [line (m/cube 1 0.5 0.5 :center true)
        horizontal-hole-pattern
        (m/union
         (for [r (range (* offset (Math/atan (/ 1 radius))) (/ Math/PI 2) (/ Math/PI 3 radius))]
           (->> line (u/translatev r radius))))

        vertical-hole-pattern
        (m/union
         (for [z (range (* offset 1) height 1)]
           (->> horizontal-hole-pattern
                (m/translate [0 0 z]))))

        fluid-cylinder
        (m/difference
         (->> (flat-wedge bottom-width radius (/ Math/PI 2))
              (u/shell 0.5 false)
              (m/extrude-linear {:height height :center false}))
         vertical-hole-pattern)]
    (m/difference (m/union fluid-cylinder #_bottom-surface) #_bottom-cut)))

(defn foam-liquid-holder [r h l]
  (->> (u/polyline [[r 0] [(+ r (* h (Math/cos (/ Math/PI 4)))) (* h (Math/sin (/ Math/PI 4)))]
                    [(* h (Math/cos (/ Math/PI 4))) (+ r (* h (Math/sin (/ Math/PI 4))))]
                    [0 r]]
                   1/2)
       (m/extrude-linear {:height l :center false})))

(defn output-hull [o r0 r1 h]
  (m/hull (->> (flat-wedge o r0 (/ Math/PI 2))
               (m/translate [(- (/ r0 2)) (- (/ r0 2)) 0])
               (m/extrude-linear {:height 1 :center false}))
          (->> (m/cylinder r1 2 :center false)
               (m/translate [0 0 h]))))

(defn output-nozzle [o r0 r1 h]
  (->> (m/difference
        (output-hull o r0 r1  h)
        (output-hull o (- r0 1) (- r1 1) h))
       (m/translate [(/ r0 2) (/ r0 2) 0])))

(defn foam-mesh [bottom-width n-layers reservoir-length starting-radius]
  (m/union
   (for [x (range starting-radius (+ starting-radius n-layers))]
     (if (odd? x)
       (foam-chamber bottom-width x reservoir-length 1)
       (foam-chamber bottom-width x reservoir-length 5/3)))))

(let [bottom-width 10
      out-nozzle (output-nozzle bottom-width 32 10 -25)
      reservoir-length 2
      top-side (->> (reservoir-side bottom-width 38 40))
      bottom-side (->> (reservoir-side bottom-width 38 40)
                       (m/hull)
                       (m/extrude-linear {:height 5 :center false})
                       (m/translate [0 0 reservoir-length]))]
  (->> (m/union
        out-nozzle
        bottom-side
        (->> (reservoir-walls bottom-width 38 40)
             (m/extrude-linear {:height reservoir-length :center false}))
        (m/difference (->> top-side
                           (m/hull)
                           (m/extrude-linear {:height 1 :center false}))
                      (->> (flat-wedge bottom-width 31 (/ Math/PI 2))
                           (m/extrude-linear {:height 2 :center false})))
        (foam-mesh bottom-width 20 reservoir-length 12))
       (s/write-scad (m/use "scad-utils/morphology.scad"))
       (spit "test.scad")))

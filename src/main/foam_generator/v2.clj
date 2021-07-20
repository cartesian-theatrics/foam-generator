(ns foam-generator.v2
  (:require
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(defn dove-tail [n hole-size]
  (m/union
   (->> (m/square (* n (* 2 hole-size)) hole-size :center false)
        (m/translate [0 hole-size]))
   (for [x (range 0 n)]
     (let [x (* x 2 hole-size)]
       (m/polygon [[x 0] [(+ x hole-size) 0] [(+ x hole-size) hole-size] [x hole-size] [x 0]])))))

(defn grid-2d [n-columns n-rows hole-size offset?]
  (cond->> (->> (for [c (range n-columns)]
                  (->> (dove-tail n-rows hole-size)
                       (m/translate [0 (* c 2 hole-size)])))
                (m/union)
                (m/translate [(- (* hole-size n-rows)) (- (* hole-size n-columns))])
                (m/extrude-linear {:height hole-size :center false}))
    offset? (m/rotatec [Math/PI Math/PI 0])))

(defn grid-3d [n-levels n-columns n-rows hole-size]
  (let [grid (grid-2d n-columns n-rows hole-size false)
        offset-grid (grid-2d n-columns n-rows hole-size true)]
    (m/union
     (concat
      (for [l (range (- (/ n-levels 2)) (/ n-levels 2))]
        (->> (if (even? (int l)) grid offset-grid) (m/translate [0 0 (* l 2 hole-size)])))
      #_(list (m/cylinder 2 3 :center true))
      #_(for [l (range (- (/ n-rows 2)) (/ n-rows 2) 2)]
          (->> (m/cube hole-size (* n-columns 2 hole-size) (* n-levels 2 hole-size))
               (m/translate [(+ 1 (/ hole-size 2) (* l 2 hole-size)) 0 0])))))))

(binding [m/*fn* 100]
  (let [grid (grid-3d 1 20 20 1/2)
        shell (->> (m/square 20 20 :center true)
                   (u/shell 1 true)
                   (m/extrude-linear {:height 55 :center false}))]
    (->> #_(m/union grid
                    (->> grid (m/rotate [(/ Math/PI 2) 0 0])))
         #_(m/union shell grid)
         (m/union #_(->> grid (m/translate [0 0 (* 0.5 20)])) grid #_shell)
         (s/write-scad (m/use "scad-utils/morphology.scad"))
         (spit "test.scad"))))

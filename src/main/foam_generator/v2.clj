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

#_(binding [m/*fn* 100]
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

(let [x 35
      y 60
      z 15
      square (m/square x y :center true)
      hole-size 1/2
      hole (m/square hole-size hole-size)
      hole-pattern-1 (m/extrude-linear
                      {:height hole-size :center false}
                      (m/difference
                       square
                       (for [dx (range 0 (+ x 0.01) (* 2 hole-size))
                             dy (range 0 (+ y 0.01) (* 2 hole-size))]
                         (->> hole (m/translate [(- dx (/ x 2)) (- dy (/ y 2))])))))

      hole-pattern-2 (m/extrude-linear
                      {:height hole-size :center false}
                      (m/difference
                       square
                       (for [dx (range hole-size (+ x 0.01) (* 2 hole-size))
                             dy (range hole-size (+ y 0.01) (* 2 hole-size))]
                         (->> hole (m/translate [(- dx (/ x 2)) (- dy (/ y 2))])))))
      layers (m/union
              (for [z (range 0 z  (* 2 hole-size))]
                (m/translate [0 0 z] (if (odd? z) hole-pattern-1 hole-pattern-2))))

      shell-thickness 1
      shell-height (+ z 40)
      shell (m/difference
             (m/translate [0 0 (/ shell-height 2)] (m/cube x y shell-height :center true))
             (->> (m/cube (- x (* 2 shell-thickness)) (- y (* 2 shell-thickness)) shell-height :center true)
                  (m/translate [0 0 (+ (/ shell-height 2) shell-thickness)])))

      intake-nozzle-thickness 3
      intake-nozzle-radius 7
      intake-nozzle-length 15
      intake-hole-size 2
      nozzle-interior-length (- y 10)
      t-x 12
      t-y 8
      hole-pattern (->> (m/union
                         (for [x (range 0 5 (* 2 intake-hole-size))
                               y (range 0 nozzle-interior-length (* 2 intake-hole-size))]
                           (m/translate [x y] (m/square 1 1))))
                        (m/extrude-linear {:height 2 :center true})
                        (m/translate [(- intake-hole-size) 0 0])
                        (m/rotatec [(/ Math/PI 2) 0 0])
                        (m/translate [0 (- (/ t-y 2)) intake-nozzle-length]))
      offset 2
      outer-intake
      (->> (m/union
            (->> (m/polygon [[(- (/ t-x 2)) (- (/ t-y 2))]
                             [0 (/ t-y 2)]
                             [(/ t-x 2) (- (/ t-y 2))]])
                 (m/extrude-linear {:height (- y 10) :center false})
                 (m/translate [0 0 intake-nozzle-length]))
            (m/hull
             (m/cylinder intake-nozzle-radius 5 :center false)
             (->>
              (m/polygon [[(- (/ t-x 2)) (- (/ t-y 2))]
                          [0 (/ t-y 2)]
                          [(/ t-x 2) (- (/ t-y 2))]])
              (m/extrude-linear {:height 1 :center false})
              (m/translate [0 0 intake-nozzle-length]))))
           (m/rotatec [(/ Math/PI 2) 0 0])
           (m/translate [0 (+ intake-nozzle-length (/ nozzle-interior-length 2)) (+ 10 (/ t-y 2))]))
      inner-intake
      (->> (m/union
            hole-pattern
            (->> (m/polygon [[(- (/ (- t-x 2) 2)) (- (/ (- t-y 2) 2))]
                             [0 (/ (- t-y 2) 2)]
                             [(/ (- t-x 2) 2) (- (/ (- t-y 2) 2))]])
                 (m/extrude-linear {:height (- y 10) :center false})
                 (m/translate [0 0 intake-nozzle-length]))
            (m/hull
             (m/cylinder (dec intake-nozzle-radius) 5 :center false)
             (->>
              (m/polygon [[(- (/ (- t-x 2) 2)) (- (/ (- t-y 2) 2))]
                          [0 (/ (- t-y 2) 2)]
                          [(/ (- t-x 2) 2) (- (/ (- t-y 2) 2))]])
              (m/extrude-linear {:height 1 :center false})
              (m/translate [0 0 intake-nozzle-length]))))
           (m/rotatec [(/ Math/PI 2) 0 0])
           (m/translate [0 (+ intake-nozzle-length (/ nozzle-interior-length 2)) (+ 10 (/ t-y 2))]))
      #_(m/hull
         (m/cylinder (dec intake-nozzle-radius) 5 :center false)
         (->>
          (m/polygon[[(- (/ (- t-x 2) 2)) (- (/ (- t-y 2) 2))]
                     [0 (/ (- t-y 2) 2)]
                     [(/ (- t-x 2) 2) (- (/ (- t-y 2) 2))]])
          (m/extrude-linear {:height 1 :center false})
          (m/translate [0 0 intake-nozzle-length])))]
  (->> #_(m/union layers shell)
       (m/union (m/difference (m/union layers shell)
                              (m/union outer-intake
                                       (->> (m/cylinder 7 1 :center false)
                                            (m/rotatec [(/ Math/PI 2) 0 0])
                                            (m/translate [0 (- (/ y 2)) (+ 5 7/2)]))))
                (m/difference outer-intake inner-intake))
       (s/write-scad (m/use "scad-utils/morphology.scad"))
       (spit "test.scad")))

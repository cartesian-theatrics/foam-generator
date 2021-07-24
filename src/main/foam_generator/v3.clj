(ns foam-generator.v3
  (:require
   [foam-generator.grids :as g]
   [foam-generator.utils :as u]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(defn air-assembly-block
  [{:keys [intake-length
           intake-radius
           inlet-hull-length
           outlet-length
           outlet-height
           outlet-width]
    :or {intake-length 20
         intake-radius 8
         outlet-length 100
         outlet-height 8
         outlet-width 15
         inlet-hull-length 5}}]
  (let [intake (->> (m/cylinder intake-radius 1 :center false)
                    (m/translate [0 0 (- inlet-hull-length)]))
        outlet-triangle (let [dx (/ outlet-width 2)
                              dy (/ outlet-height 2)]
                          (m/polygon [[(- dx) (- dy)]
                                      [0 dy]
                                      [dx (- dy)]]))
        outlet (->> outlet-triangle
                    (m/extrude-linear {:height outlet-length :center false}))]
    (->> (m/union (m/hull intake (m/extrude-linear {:height 1 :center false} outlet-triangle))
                  (->> (m/cylinder intake-radius intake-length :center false)
                       (m/translate [0 0 (- (+ inlet-hull-length intake-length))]))
                  outlet)
         (m/translate [0 0 (+ intake-length inlet-hull-length)]))))

(defn air-assembly
  [& {:keys [intake-length
             intake-radius
             inlet-hull-length
             outlet-length
             outlet-height
             outlet-width]
    :or {intake-length 20
         intake-radius 8
         outlet-length 100
         outlet-height 8
         outlet-width 15
         inlet-hull-length 10}}]
  (let [block-args {:intake-length intake-length
                    :intake-radius intake-radius
                    :outlet-width outlet-width
                    :outlet-length outlet-length
                    :outlet-height outlet-height
                    :inlet-hull-length inlet-hull-length}
        outer-block (air-assembly-block block-args)
        inner-block (air-assembly-block
                     (-> block-args
                         (update :intake-radius - 1)
                         (update :outlet-length - 1)
                         (update :outlet-width - 1)
                         (update :outlet-height - 1)))
        hole-size 2
        hole-pattern (for [z (range (* 2 hole-size) outlet-length (* 2 hole-size))
                           x (range (* 5/3 hole-size) outlet-width (* 2 hole-size))]
                       (->> (m/cube hole-size hole-size hole-size)
                            (m/rotatec [(/ Math/PI 2) 0 0])
                            (m/translate [(- (- x (/ outlet-width 2)))
                                          (- (/ outlet-height 2))
                                          z])))]
    (m/difference outer-block (m/union inner-block hole-pattern))))

(defn output-nozzle
  [{:keys []}]
  (let [nozzle
        (m/difference
         (m/hull
          (->> (m/circle 11)
               (m/extrude-linear {:height 5 :center false}))
          (->> (m/circle 6)
               (m/extrude-linear {:height 1 :center false})
               (m/translate [0 0 30])))
         (m/hull
          (->> (m/circle 10)
               (m/extrude-linear {:height 5 :center false}))
          (->> (m/circle 5)
               (m/extrude-linear {:height 1 :center false})
               (m/translate [0 0 30]))))]
    nozzle))

(defn foam-gun
  [& {:keys [mixing-chamber-rows
             mixing-chamber-columns
             mixing-chamber-levels
             mixing-chamber-hole-size
             nozzle-intake-length
             nozzle-intake-radius]}]
  (let [grid (g/grid-3d mixing-chamber-levels
                        mixing-chamber-rows
                        mixing-chamber-columns
                        mixing-chamber-hole-size
                        true)
        chamber-length (* 2 mixing-chamber-hole-size mixing-chamber-columns)
        chamber-width (* 2 mixing-chamber-hole-size mixing-chamber-rows)
        chamber-height (* 2 mixing-chamber-hole-size mixing-chamber-levels)
        intake-assembly-args {:intake-length nozzle-intake-length
                              :intake-radius nozzle-intake-radius
                              :outlet-length (- chamber-length 15 8)}
        assembly-transform (comp
                            (partial m/translate [0
                                                  (- (+ nozzle-intake-length (/ chamber-length 2)))
                                                  (+ (/ nozzle-intake-radius 2) 5)])
                            (partial m/rotatec [(/ Math/PI 2) 0 Math/PI]))
        air-intake (assembly-transform (air-assembly intake-assembly-args))
        air-intake-block (assembly-transform (air-assembly-block intake-assembly-args))
        nozzle-outlet-radius 11
        cap-height 5
        output-nozzle (binding [m/*fn* 100]
                        (->> (m/difference
                              (m/hull
                               (->> (m/circle nozzle-outlet-radius)
                                    (m/extrude-linear {:height 5 :center false}))
                               (->> (m/circle 6)
                                    (m/extrude-linear {:height 1 :center false})
                                    (m/translate [0 0 30])))
                              (m/hull
                               (->> (m/circle (dec nozzle-outlet-radius))
                                    (m/extrude-linear {:height 5 :center false}))
                               (->> (m/circle 5)
                                    (m/extrude-linear {:height 1 :center false})
                                    (m/translate [0 0 30]))))
                             (m/union (->> (m/difference (m/circle 6)
                                                         (m/circle 5))
                                           (m/extrude-linear {:height cap-height :center false})
                                           (m/translate [0 0 31])))
                             (m/rotatec [(- (/ Math/PI 2)) 0 0])
                             (m/translate [0 (dec (/ chamber-length 2)) (+ 7 (/ nozzle-outlet-radius 2))])))
        reservoir-height 30
        cap-radius 10
        cap-thickness 1
        top-reservoir (binding [m/*fn* 100]
                        (->> (m/difference
                              (m/hull (->> (m/square chamber-width chamber-length :center true)
                                           (m/extrude-linear {:height reservoir-height :center false}))
                                      (->> (m/square (+ chamber-width 20) (- chamber-length 2) :center true)
                                           (m/extrude-linear {:height 1 :center false})
                                           (m/translate [0 0 reservoir-height])))
                              (m/union (m/hull (->> (m/square (- chamber-width 2) (- chamber-length 2) :center true)
                                                    (m/extrude-linear {:height (dec reservoir-height) :center false}))
                                               (->> (m/square (+ chamber-width 18) (- chamber-length 4) :center true)
                                                    (m/extrude-linear {:height 1 :center false})
                                                    (m/translate [0 0 (dec reservoir-height)])))
                                       (->> (m/circle cap-radius)
                                            (m/extrude-linear {:height cap-height :center false})
                                            (m/translate [0 0 reservoir-height]))))
                             (m/union (->> (m/circle cap-radius)
                                           (u/shell 1 true)
                                           (m/extrude-linear {:height cap-height :center false})
                                           (m/translate [0 0 chamber-height])))
                             (m/translate [0 0 chamber-height])))]
    (m/union (m/difference grid air-intake-block) air-intake output-nozzle top-reservoir)))

(comment
  (->> (foam-gun :mixing-chamber-rows 30
                 :mixing-chamber-columns 50
                 :mixing-chamber-levels 15
                 :mixing-chamber-hole-size 1
                 :nozzle-intake-length 10
                 :nozzle-intake-radius 4)
       (s/write-scad (m/use "scad-utils/morphology.scad"))
       (spit "test.scad"))

  (->> (air-assembly-block {})
       (s/write-scad)
       (spit "test.scad"))

  (->> (air-assembly {:intake-length 10
                      :intake-radius 6})
       (s/write-scad)
       (spit "test.scad"))

  (->> (g/grid-3d 30 50 30 1/2)
       (s/write-scad)
       (spit "test.scad"))

  )

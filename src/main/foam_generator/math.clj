(ns foam-generator.math)

(defmacro cos [x] `(Math/cos ~x))
(defmacro acos [x] `(Math/acos ~x))
(defmacro sin [x] `(Math/sin ~x))
(defmacro asin [x] `(Math/asin ~x))
(defmacro tan [x] `(Math/tan ~x))
(defmacro atan [x] `(Math/atan ~x))
(defmacro sqr [x] `(Math/pow ~x 2))
(defmacro sqrt [x] `(Math/sqrt ~x))
(defmacro pow [x exp] `(Math/pow ~x ~exp))

(defmacro |2 [arg] `(/ ~arg 2))
(defmacro |3 [arg] `(/ ~arg 3))
(defmacro |4 [arg]  `(/ ~arg 4))

(def pi Math/PI)
(def pi|2 (/ pi 2))
(def pi|3 (/ pi 3))
(def pi|4 (/ pi 4))
(def pi|5 (/ pi 5))
(def pi|6 (/ pi 6))

(defn nearest-multiple [x m]
  (* (quot x m) m))

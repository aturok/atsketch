(ns atsketch.distort
  (:require [quil.core :as q]))

(defn distort-g [d v]
  (+ v (* d (q/random-gaussian))))

(defn distort-point [[xd yd] [x y]]
  [(distort-g xd x) (distort-g yd y)])

(defn distorter [x y]
  (partial distort-point [x y]))

(defn off-center-growing-distort-x [[x y] center max-off max-d]
  [(distort-g (* max-d (float (min (/ (Math/abs (- y center)) max-off) 1))) x) y])

(defn off-center-growing-distort-y [[x y] center max-off max-d]
  (let [[y x] (off-center-growing-distort-x [y x] center max-off max-d)]
    [x y]))

(defn off-xy-center-growing-distort [[x y] center max-off max-d]
  (let [measure (max (float (min (/ (Math/abs (- y center)) max-off) 1))
                     (float (min (/ (Math/abs (- x center)) max-off) 1)))]
    [(distort-g (* max-d measure) x) (distort-g (* max-d measure) y)]))


            
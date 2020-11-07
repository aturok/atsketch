(ns atsketch.uber-shapes
  (:require [quil.core :as q]
            [atsketch.util :refer [map-but-edges]]
            [atsketch.shapes :as sh]
            [atsketch.distort :as dst]
            [atsketch.util :as util :refer [random-c random-cl]]))

(defn rects-rect [n rrand rect gen-dims gen-color]
  (vec (repeatedly n (fn []
                       (let [[sw sh] (gen-dims)
                             coords {:x (+ (:x rect) (* (rrand) (:w rect)))
                                     :y (+ (:y rect) (* (rrand) (:h rect)))
                                     :w sw
                                     :h sh}
                             color (gen-color coords)
                             ]
                         {:coords coords
                          :color color})))))

(defn line-y [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        a (/ (- (float y1) y2) dx)
        b (- y1 (* a x1))]
    (if (zero? dx)
      nil
      (fn [x] (+ (* a x) b)))))

(defn rects-triangle [n rrand [p1 p2 p3 :as points] gen-dims gen-color]
  (let [ylimits (filter some? [(line-y p1 p2)
                               (line-y p1 p3)
                               (line-y p3 p2)])
        [x1 x2 x3] (map first points)
        xmin (min x1 x2 x3)
        xmax (max x1 x2 x3)
        gen-x #(+ xmin (* (rrand) (- xmax xmin)))
        gen-y (fn [x]
                (let [ys (map #(% x) ylimits)
                      ymin (apply min ys)
                      ymax (apply max ys)]
                  (+ ymin (* (rrand) (- ymax ymin)))))]
    (vec (repeatedly n (fn []
                         (let [[sw sh] (gen-dims)
                               x (gen-x)
                               coords {:x x
                                       :y (gen-y x)
                                       :w sw
                                       :h sh}]
                           {:coords coords
                            :color (gen-color coords)}))))))

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
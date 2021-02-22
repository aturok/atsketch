(ns atsketch.curves
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.uber-shapes :as ush]
            [atsketch.distort :as dst]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]))

(defn some-curves [w h]
  (let [base-color {:h 20 :s 255 :b 255 :a 255}
        base-x 500
        offset-base 200]
    (-> (repeatedly 10 (fn [] (let [x (random-c base-x 50)]
                               {:color (update base-color :h (partial + (random-c 0 10)))
                                :weight (max 1 (random-c 2 2))
                                :detail 100
                                :points [{:x x :y 0}
                                         {:x (+ x (random-c 0 offset-base)) :y (* 0.25 h)}
                                         {:x (+ x (random-c 0 offset-base)) :y (* 0.75 h)}
                                         {:x x :y h}]})))
        vec)))

(defn fat-curves [w h]
  (let [base-color {:h 20 :s 255 :b 255 :a 20}
        base-x 500
        offset-base 100]
    (-> (repeatedly 4 (fn [] (let [x (random-c base-x 50)]
                                {:color (update base-color :h (partial + (random-c 0 10)))
                                 :weight (+ 200 (random-c 10 20))
                                 :detail 400
                                 :points [{:x x :y 0}
                                          {:x (+ x (random-c 0 offset-base)) :y (* 0.25 h)}
                                          {:x (+ x (random-c 0 offset-base)) :y (* 0.75 h)}
                                          {:x x :y h}]})))
        vec)))

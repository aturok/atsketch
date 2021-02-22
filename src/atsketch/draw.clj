(ns atsketch.draw
  (:require [quil.core :as q]
            [atsketch.shapes :as sh]))

(defn set-color! [which {:keys [h s b a] :or {h 0 s 255 b 255 a 255}}]
  (which h s b a))

(defn draw-line [{:keys [points color]}]
  (set-color! q/stroke color)
  (doseq [[[x1 y1] [x2 y2]] (map vector points (next points))]
    (q/line x1 y1 x2 y2)))

(defn draw-rect [{:keys [coords color]}]
  (set-color! q/stroke color)
  (set-color! q/fill color)
  (q/rect (:x coords) (:y coords) (:w coords) (:h coords)))


(defn draw-rect-with-shadow [rect shadow-displacer]
  (let [drop-shadow (fn [intensity]
                      {:coords (sh/shadow-rect (sh/constant-growth intensity) shadow-displacer rect)
                       :color {:h 0 :s 0 :b 0 :a 70}})]
    (draw-rect (drop-shadow 1.10))
    (draw-rect (drop-shadow 1.15))
    (draw-rect (drop-shadow 1.20))
    (draw-rect (drop-shadow 1.25))
    (draw-rect rect)))

(defn draw-bezier [{:keys [color weight detail]
                    [{x1 :x y1 :y}
                     {x2 :x y2 :y}
                     {x3 :x y3 :y}
                     {x4 :x y4 :y}] :points}]
  (when color
    (set-color! q/stroke color))
  (set-color! q/fill {:h 0 :s 0 :b 0 :a 0})
  (when weight
    (q/stroke-weight weight))
  (q/bezier-detail (or detail 20))
  (q/bezier x1 y1 x2 y2 x3 y3 x4 y4))

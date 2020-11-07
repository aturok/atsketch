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

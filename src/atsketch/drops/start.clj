(ns atsketch.drops.start
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.util :as util]))

(defn draw-pixel [color]
  (let [size 20]
    (apply q/fill color)
    (q/rect 0 0 size size)))

(def w 1000)
(def h w)

(defn draw-state [{:keys [background go pixels]}]
  (when go
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)
    (doseq [{[x y] :origin :keys [color]} pixels]
      (q/push-matrix)
      (q/translate (+ (/ w 2) x) (+ (/ h 2) y))
      (draw-pixel color)
      (q/pop-matrix))))

(defn upd-state [{:keys [w h]}]
  {:w w
   :h h
   :go true
   :background [0 0 0 255]
   :color [140 250 250 230]
   :pixels (do (q/random-seed 19)
               (->> (fn []
                      {:color [(q/random 120 180) 250 250 (q/random 150 250)]
                       :origin (let [y (q/random (- (* 0.2 h)) (* 0.2 h))]
                                 [(q/random (- (* 0.2 w)) (* 0.2 w))
                                  y])})
                    (repeatedly 1000)
                    (filter (fn [{[x y] :origin}]
                              (< (* x x) (* y y) (* 0.2 w 0.2 w))))
                    vec))})

(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  :setup (fn setup []
           (q/frame-rate 30)
           (q/color-mode :hsb)
           {:w w
            :h h})
  :settings (fn settings []
              (q/smooth))
  :update upd-state
  :draw draw-state
  :middleware [m/fun-mode])

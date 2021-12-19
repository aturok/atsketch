(ns atsketch.drops.start
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.util :as util]))

(defn draw-pixel [{:keys [color size]}]
  (apply q/fill color)
  (q/rect 0 0 size size))

(def w 1000)
(def h w)

(defn draw-state [{:keys [background go pixels]}]
  (when go
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)
    (doseq [{[x y] :origin :as pixel} pixels]
      (q/push-matrix)
      (q/translate (+ (* 0.5 w) x) (+ (* 0.6 h) y))
      (draw-pixel pixel)
      (q/pop-matrix))))

(defn upd-state [{:keys [w h]}]
  (let [displ (* 0.1 h)
        dh (* 0.36 h)
        r (* 0.2 h)
        yfn #(- (* %1 (/ (- dh displ) (* %2 (Math/sqrt (- (* r r) (* displ displ)))))) dh)]
    {:w w
     :h h
     :go true
     :background [35 20 255 50]
     :color [140 250 250 230]
     :pixels (do (q/random-seed 20)
                 (->> (fn []
                        {:color [(q/random 5 15) 255 0 (q/random 100 230)]
                         :origin (let [y (q/random (- 0 r dh) r)]
                                   [(q/random (- r) r)
                                    y])
                         :size 8})
                      (repeatedly 10000)
                      (filter (fn [{[x y] :origin}]
                                (or (< (+ (* x x) (* y y)) (* r r))
                                    (and (< y (- displ))
                                         (> y (yfn x 1))
                                         (> y (yfn x -1))))
                                ))
                      vec))}))

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

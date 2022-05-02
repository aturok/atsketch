(ns atsketch.drops.start
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.util :as util]))

(defn draw-pixel [{:keys [color size]}]
  (apply q/fill color)
  (q/rect 0 0 size size))

(def w 1000)
(def h w)
(def screen-w w)
(def screen-h h)

(defn draw-state [{:keys [background go parts]}]
  (when go
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (- (* 0.5 screen-w) (* 0.5 (:w (first parts))) 20) (+ (* 0.4 screen-h) 20))
    (q/rotate (q/radians 60))
    (apply q/fill [10 255 200 240])
    (q/rect 0 0 150 105)
    (q/pop-matrix)

    (doseq [{:keys [y w h color] :as part} parts]
      (q/push-matrix)
      (q/translate (* 0.5 screen-w) y)
      (apply q/fill color)
      (q/rect 0 0 w h)
      (q/pop-matrix))
    
    (q/rect-mode :corner)
    (q/push-matrix)
    (apply q/fill [0 0 100 100])
    (q/rect (- (* 0.5 screen-w) (* 0.5 (:w (first parts))) 10) (* 0.4 screen-h) 10 (* 0.35 screen-h))
    (q/rect (+ (* 0.5 screen-w) (* 0.5 (:w (first parts))) 10) (* 0.4 screen-h) -10 (* 0.35 screen-h))

    (apply q/fill [0 0 100 50])
    (q/rect (- (* 0.5 screen-w) (* 0.5 (:w (first parts))))
            (* 0.4 screen-h)
            (:w (first parts))
            (* 0.35 screen-h))

    (apply q/fill [0 0 100 120])
    (q/rect (- (* 0.5 screen-w) (* 0.5 (:w (first parts))))
            ;; (- (+ (* 0.4 screen-h) (* 5.5 (:h (first parts)))))
            ;; (- (+ (* 0.4 screen-h) (* 0.35 screen-h)) (* 0.5 (:h (first parts))))
            (- (+ (* 0.4 screen-h) (* 3.5 (:h (first parts)))) (* 0.5 (:h (first parts))))
            (:w (first parts))
            (* 0.5 (:h (first parts))))
    (q/pop-matrix)))

(defn upd-state [{:keys [w h]}]
  (let [displ (* 0.1 h)
        dh (* 0.36 h)
        r (* 0.2 h)
        yfn #(- (* %1 (/ (- dh displ) (* %2 (Math/sqrt (- (* r r) (* displ displ)))))) dh)
        layer-h (* 0.1 h)
        layer-w (* 0.2 w)
        layer-displ (- (* 0.01 layer-h))
        layer-y #(+ (* 0.5 h) (* % (+ layer-h layer-displ)))]
    {:w w
     :h h
     :go true
     :background [0 0 0 250]
     :color [140 250 250 230]
     :parts [{:y (+ (* 0.5 h) (* 0 (+ layer-h layer-displ)))
              :w layer-w
              :h layer-h
              :color [110 0 255 20]}
             {:y (+ (* 0.5 h) (* 1 (+ layer-h layer-displ)))
              :w layer-w
              :h layer-h
              :color [155 255 255 140]}
             {:y (+ (* 0.5 h) (+ layer-h layer-displ) (+ (* 0.75 layer-h) layer-displ))
              :w layer-w
              :h (* 0.5 layer-h)
              :color [44 255 255 160]}]
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

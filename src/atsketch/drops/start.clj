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

(defn- draw-strawbery []
  (q/rotate (q/radians 60))
  (apply q/fill [10 255 200 240])
  (q/rect-mode :corner)
  (q/rect 0 0 150 105)
  
  (apply q/fill [10 100 255 200])
  (q/rect-mode :corner)
  (doseq [_ (range 20)]
    (q/rect (q/random 5 145) (q/random 5 100) (q/random 2 4) (q/random 2 4)))
  (q/rect-mode :center))

(defn- draw-glass [{:keys [w h bottom-h side-w rim-w]
                    :or {side-w 10
                         rim-w 2.5}}]
  (q/rect-mode :corner)
  (q/push-matrix)
  (q/stroke-weight 0)
  (apply q/fill [0 0 250 40])
  (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w) 0 side-w (- h bottom-h))
  (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w) 0 (- side-w) (- h bottom-h))

  (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w)
          (- h bottom-h 0)
          (+ w side-w side-w)
          bottom-h)
  (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w)
          (- rim-w)
          (+ w side-w side-w)
          rim-w)

  (apply q/fill [0 0 100 50])
  (q/rect (- (* 0.5 screen-w) (* 0.5 w))
          0
          w
          (- h bottom-h))

  (q/pop-matrix))

(defn- draw-parts [parts]
  (doseq [{:keys [y w h color]} parts]
    (q/push-matrix)
    (q/translate (* 0.5 screen-w) y)
    (apply q/fill color)
    (q/rect 0 0 w h)
    (q/pop-matrix)))

(defn draw-description [& {:keys [lines size line-height]}]
  (q/push-matrix)
  (let [main-text-size (or size 16)
        main-line-height (or 1.25 line-height)]
    (doall
     (map-indexed (fn [idx {:keys [text size color offset line-height]}]
                    (q/text-size (or size main-text-size))
                    (apply q/fill color)
                    (q/text text (or offset 0) 0)
                    (q/translate 0 (* (or line-height main-line-height) (or size main-text-size))))
                  lines))
    (q/pop-matrix)))

(defn draw-state [{:keys [background go parts]}]
  (when go
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (- (* 0.5 screen-w) (* 0.5 (:w (first parts))) 20) (+ (* 0.3 screen-h) 20))
    (draw-strawbery)
    (q/pop-matrix)
    
    (draw-parts parts)
    
    (q/push-matrix)
    (q/translate 0 (* 0.4 screen-h))
    (draw-glass {:w (:w (first parts))
                 :h (* 0.347 screen-h)
                 :bottom-h (* 0.5 (:h (first parts)))})
    (q/pop-matrix)
    
    (q/push-matrix)
    (q/translate (* 0.1 screen-w) (* 0.25 screen-h))
    (draw-description :lines [{:text "Sand Jinn" :color [155 255 255 220] :size 22 :offset -10 :line-height 1.1}
                              {:text "Limoncello, 1pt" :color [0 0 255 200] :size 14}
                              {:text "Blue curacao, 2pts" :color [0 0 255 200] :size 14}
                              {:text "Gin, 2pts" :color [0 0 255 200] :size 14}
                              {:text "Strawberry" :color [0 0 255 200] :size 14}]
                      :offset 20)
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

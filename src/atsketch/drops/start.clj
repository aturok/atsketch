(ns atsketch.drops.start
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [atsketch.util :as util]
            [atsketch.signature :refer [draw-signature]]))

(defn draw-pixel [{:keys [color size]}]
  (apply q/fill color)
  (q/rect 0 0 size size))

(def w 1000)
(def h w)
(def screen-w w)
(def screen-h h)

(defn- draw-strawbery []
  (let [tx 62 ty 140.0
        lline #(+ (- ty 10) (* (/ (- ty 10) (- tx 5)) %))
        rline #(- (- ty 10) (* (/ (- ty 10) (- tx 5)) %))]
    (q/rotate (q/radians -20))
    (apply q/fill [10 255 200 240])
    (q/rect-mode :corner)
    (q/triangle (- tx) 0 tx 0 0 ty)

    (apply q/fill [10 100 255 200])
    (q/rect-mode :corner)
    (doseq [_ (range 100)]
      (let [[x y] [(q/random (- tx) tx) (q/random 0 ty)]]
        (when (and (<= y (lline x)) (<= y (rline x)))
          (q/rect x y (q/random 2 4) (q/random 2 4)))))
    (q/rect-mode :center)))

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

  (let [flare-h (+ h rim-w)
        flare-off -8
        flare-sw 8]
    (apply q/fill [0 0 255 20])
    (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w flare-off) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 255 30])
    (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w (+ 2 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 255 35])
    (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w (+ 4 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 255 25])
    (q/rect (- (* 0.5 screen-w) (* 0.5 w) side-w (+ 6 flare-off)) (- rim-w) flare-sw flare-h))
  
  (let [flare-h (+ h rim-w)
        flare-off -10
        flare-sw 4]
    (apply q/fill [0 0 0 40])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w flare-off) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 0 50])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 2 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 255 15])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 2 flare-off)) (- rim-w) 2 flare-h)
    (apply q/fill [0 0 0 45])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 4 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 0 25])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 6 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 0 15])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 8 flare-off)) (- rim-w) flare-sw flare-h)
    (apply q/fill [0 0 0 10])
    (q/rect (+ (* 0.5 screen-w) (* 0.5 w) side-w (+ 10 flare-off)) (- rim-w) flare-sw flare-h))

  (q/pop-matrix))

(defn- draw-parts [parts]
  (doseq [{:keys [y w h color]} parts]
    (q/push-matrix)
    (q/translate (* 0.5 screen-w) y)
    (apply q/fill color)
    (q/rect 0 0 w h)
    (doseq [_ (range 2000)]
      (q/stroke-weight 0)
      (q/fill 0 0 255 (q/random 0 2))
      (q/rect (q/random (* -0.5 w) (* 0.5 w)) (q/random (* -0.5 h) (* 0.5 h)) (q/random 2 8) (q/random 2 8)))
    (q/pop-matrix)))

(defn draw-description [& {:keys [lines size line-height font]}]
  (q/push-matrix)
  (let [main-text-size (or size 16)
        main-line-height (or 1.25 line-height)]
    (doall
     (map-indexed (fn [idx {:keys [text size color offset line-height]}]
                    (if font
                      (q/text-font (q/create-font font (or size main-text-size)))
                      (q/text-size (or size main-text-size)))
                    (apply q/fill color)
                    (q/text text (or offset 0) 0)
                    (q/translate 0 (* (or line-height main-line-height) (or size main-text-size))))
                  lines))
    (q/pop-matrix)))

(defn draw-state [{:keys [background go parts fonts]}]
  (when go
    (q/no-stroke)
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (* -0.15 screen-w) 0)

    (q/push-matrix)
    (q/translate (- (* 0.5 screen-w) (* 0.5 (:w (first parts))) 20) (+ (* 0.35 screen-h) 0))
    (draw-strawbery)
    (q/pop-matrix)

    (draw-parts parts)

    (q/push-matrix)
    (q/translate 0 (* 0.4 screen-h))
    (draw-glass {:w (:w (first parts))
                 :h (* 0.347 screen-h)
                 :bottom-h (* 0.5 (:h (first parts)))})
    (q/pop-matrix)
    (q/pop-matrix)

    (q/push-matrix)
    (q/translate (* 0.65 screen-w) (* 0.48 screen-h))
    (draw-description :lines [{:text "Sand Jinn" :color [155 255 255 220] :size 22 :offset -10 :line-height 1.1}
                              {:text "Limoncello, 1pt" :color [0 0 255 200] :size 14}
                              {:text "Blue curacao, 2pts" :color [0 0 255 200] :size 14}
                              {:text "Gin, 2pts" :color [0 0 255 200] :size 14}
                              {:text "Strawberry" :color [0 0 255 200] :size 14}]
                      :offset 20
                      :font "Montserrat Regular")
    (q/pop-matrix)

    (q/push-matrix)
    (q/translate (* 0.7 screen-w) (* 0.7 screen-h))
    (q/rotate (q/radians -12))
    (q/scale 2)
    (draw-signature :color [162 200 255])
    (q/pop-matrix)))

(defn upd-state [{:keys [w h] :as original}]
  (let [displ (* 0.1 h)
        dh (* 0.36 h)
        r (* 0.2 h)
        yfn #(- (* %1 (/ (- dh displ) (* %2 (Math/sqrt (- (* r r) (* displ displ)))))) dh)
        layer-h (* 0.1 h)
        layer-w (* 0.2 w)
        layer-displ (- (* 0.01 layer-h))
        layer-y #(+ (* 0.5 h) (* % (+ layer-h layer-displ)))]
    (merge
     original
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
               :color [175 255 255 140]}
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
                                          (> y (yfn x -1))))))
                       vec))})))

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

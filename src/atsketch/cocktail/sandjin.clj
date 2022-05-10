(ns atsketch.cocktails.sandjin
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [atsketch.util :as util]
            [atsketch.signature :refer [draw-signature]]))

(def w 1000)
(def h w)
(def screen-w w)
(def screen-h h)

(defn- randomc [c w]
  (cond 
    (not c) c
    (zero? w) c
    :else (q/random (Math/max 0 (- c w)) (Math/min 255 (+ c w)))))

(defn- randomize-color [[h s l a] & {:keys [hw sw lw aw]
                                     :or {hw 0 sw 0 lw 0 aw 0}}]
  (filterv some? (map randomc [h s l a] [hw sw lw aw])))

(defn- draw-glitter-piece [{[h s l a] :color :keys [x y spread rotate]
                            :or {spread 6
                                 rotate 0}}]
  (q/push-matrix)
  (q/rect-mode :corner)
  (q/translate x y)
  (q/rotate rotate)
  (doseq [x? [true false]
          p (range (- spread) spread)]
    (apply q/fill [h s l (* a (/ (- spread (Math/abs p)) 10))])
    (q/rect (if x? p 0) (if x? 0 p) 1 1))
  (q/pop-matrix))

(defn- draw-glitter [pieces]
  (doseq [p pieces]
    (draw-glitter-piece p)))

(defn- draw-strawbery [{:keys [color peckles tx ty]
                        :or {color [10 255 200 240]
                             peckles []
                             tx 62.0
                             ty 140.0}}]
  (q/rotate (q/radians -20))
  (apply q/fill color)
  (q/rect-mode :corner)
  (q/triangle (- tx) 0 tx 0 0 ty)

  (apply q/fill [10 100 255 200])
  (q/rect-mode :corner)
  (doseq [{:keys [x y w h]} peckles]
    (when x
      (q/rect x y w h)))
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
  (doseq [{:keys [y w h color noise]} parts]
    (q/push-matrix)
    (q/translate (* 0.5 screen-w) y)
    (apply q/fill color)
    (q/rect 0 0 w h)
    (doseq [{:keys [x y w h a]} noise]
      (q/stroke-weight 0)
      (q/fill 0 0 255 a)
      (q/rect x y w h))
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

(defn draw-state [{:keys [background go parts fonts strawberry glitter signature-alpha]}]
  (when go
    (q/no-stroke)
    (apply q/background background)
    (q/fill 0 0 0 0)
    (q/stroke-weight 0)
    (q/rect-mode :center)

    (draw-glitter (or glitter []))

    (q/push-matrix)
    (q/translate (* -0.15 screen-w) 0)

    (q/push-matrix)
    (q/translate (- (* 0.5 screen-w) (* 0.5 (:w (first parts))) 20) (+ (* 0.35 screen-h) 0))
    (draw-strawbery strawberry)
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
    (draw-description :lines [{:text "Sand Jinn" :color [155 255 255 220] :size 26 :offset -10 :line-height 1.1}
                              {:text "Limoncello, 1pt" :color [0 0 255 200] :size 20}
                              {:text "Blue curacao, 2pts" :color [0 0 255 200] :size 20}
                              {:text "Gin, 2pts" :color [0 0 255 200] :size 20}
                              {:text "Strawberry" :color [0 0 255 200] :size 20}]
                      :offset 20
                      :font "Space Mono")
    (q/pop-matrix)

    (q/push-matrix)
    (q/translate (* 0.7 screen-w) (* 0.7 screen-h))
    (q/rotate (q/radians -12))
    (q/scale 3)
    (draw-signature :color [162 0 255] :alpha-feed signature-alpha)
    (q/pop-matrix)))

(defn- randomize-hue [[h & other] & {:keys [width]
                                     :or {width 10}}]
  (into [(q/random (Math/max 0 (- h width)) (Math/min 255 (+ h width)))] other))


(defn- gen-strawberry []
  (let [tx (+ 72.0 (* 10 (q/random-gaussian))) ty 140.0]
    {:tx tx
     :ty ty
     :color (randomize-hue [10 255 200 240])
     :peckles (->> (fn peckle [] (let [lline #(+ (- ty 10) (* (/ (- ty 10) (- tx 5)) %))
                                       rline #(- (- ty 10) (* (/ (- ty 10) (- tx 5)) %))
                                       [x y] [(q/random (- tx) tx) (q/random 0 ty)]]
                                   (when (and (<= y (lline x)) (<= y (rline x)))
                                     {:x x :y y :w (q/random 2 4) :h (q/random 2 4)})))
                   repeatedly
                   (take 100)
                   (filterv some?))}))

(defn- gen-noise-in-rect [n w h]
  (->> (fn []
         {:x (q/random (* -0.5 w) (* 0.5 w))
          :y (q/random (* -0.5 h) (* 0.5 h))
          :w (q/random 2 8)
          :h (q/random 2 8)
          :a (q/random 0 2)})
       repeatedly
       (take n)
       vec))

(defn upd-state [{:keys [w h step n-steps seed-basis] :as original
                  :or {n-steps 1
                       seed-basis 200}}]
  (if (and (some? step) (>= step n-steps))
    original
    (do
      (q/random-seed (+ seed-basis (or step 0)))

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
         {:step (cond
                  (not step) 0
                  (< step n-steps) (inc step)
                  :else step)
          :w w
          :h h
          :go true
          :background [0 0 0 250]
          :color [140 250 250 230]
          :strawberry (gen-strawberry)
          :parts [{:y (+ (* 0.5 h) (* 0 (+ layer-h layer-displ)))
                   :w layer-w
                   :h layer-h
                   :color (randomize-hue [110 0 255 20])
                   :noise (gen-noise-in-rect 2000 layer-w layer-h)}
                  {:y (+ (* 0.5 h) (* 1 (+ layer-h layer-displ)))
                   :w layer-w
                   :h layer-h
                   :color (randomize-hue [175 255 255 140])
                   :noise (gen-noise-in-rect 2000 layer-w layer-h)}
                  {:y (+ (* 0.5 h) (+ layer-h layer-displ) (+ (* 0.75 layer-h) layer-displ))
                   :w layer-w
                   :h (* 0.5 layer-h)
                   :color (randomize-hue [44 255 255 160])
                   :noise (gen-noise-in-rect 1000 layer-w (* 0.5 layer-h))}]
          :pixels (->> (fn []
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
                       vec)

          :glitter (->> #(assoc {}
                                :x (q/random 0 w)
                                :y (q/random 0 h)
                                :color [(q/random 0 255) 50 250 200]
                                :spread (q/random 3 16)
                                :rotate (q/radians (q/random -10 10)))
                        repeatedly
                        (take (q/random 75 150))
                        vec)
          :signature-alpha (->> #(q/random 80 110)
                                repeatedly
                                (take 500)
                                vec)})))))

(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  :setup (fn setup []
           (q/frame-rate 30)
           (q/color-mode :hsb)
           {:w w
            :h h
            :n-steps 30})
  :settings (fn settings []
              (q/smooth))
  :update upd-state
  :draw draw-state
  :middleware [m/fun-mode])

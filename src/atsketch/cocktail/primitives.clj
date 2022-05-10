(ns atsketch.cocktails.primitives
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [atsketch.util :as util]))

(defn- randomc [c w]
  (cond
    (not c) c
    (zero? w) c
    :else (q/random (Math/max 0 (- c w)) (Math/min 255 (+ c w)))))

(defn randomize-color [[h s l a] & {:keys [hw sw lw aw]
                                    :or {hw 0 sw 0 lw 0 aw 0}}]
  (filterv some? (map randomc [h s l a] [hw sw lw aw])))



(defn draw-glitter-piece [{[h s l a] :color :keys [x y spread rotate]
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

(defn draw-glitter [pieces]
  (doseq [p pieces]
    (draw-glitter-piece p)))


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


(defn gen-noise-in-rect [n w h]
  (->> (fn []
         {:x (q/random (* -0.5 w) (* 0.5 w))
          :y (q/random (* -0.5 h) (* 0.5 h))
          :w (q/random 2 8)
          :h (q/random 2 8)
          :a (q/random 0 2)})
       repeatedly
       (take n)
       vec))
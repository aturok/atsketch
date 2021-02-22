(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.squares2 :as sq2]
            [atsketch.curves :as crv]
            [atsketch.draw :as d]))

(def w 1000)
(def h 1000)

(defn update-state [state] state)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color {:h 4 :s 0 :b 255}
   :rects (sq2/tris1 w h)
   :curves (do
             (q/random-seed 10)
             (concat (crv/fat-curves w h)
                     (crv/some-curves w h)))})

(defn settings []
  (q/smooth 100))



(defn draw-state [{:keys [curves]}]
  (q/background 25 10 255)
  (doall (map d/draw-bezier curves)))

;; 400 0 300 200 540 500 400 1000

(defn mouse-press [& _]
  (q/save-frame "out/pretty-pic-#####.tiff"))


(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :settings settings
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :mouse-clicked mouse-press
;   :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
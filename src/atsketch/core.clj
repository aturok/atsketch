(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.squares2 :as sq2]
            [atsketch.curves :as crv]
            [atsketch.draw :as d]))

(def w 1000)
(def h 4000)

(defn go-next-frame [{:keys [frame max-frame w h] :as state}]
  (if (>= frame max-frame)
    (assoc state
           :fram max-frame
           :done true
           :curves [])
    (assoc state
           :frame (inc frame)
           :curves (do
                     (q/random-seed frame)
                     (concat (crv/fat-curves w h)
                             (crv/some-curves w h))))))

(defn update-state [state] state)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:frame 0
   :max-frame 100
   :done false
   :w w
   :h h
   :color {:h 4 :s 0 :b 255}
   :curves []})

(defn settings []
  (q/smooth 100))

(defn draw-state [{:keys [curves done frame w h]}]
  (if done
    (q/background 0)
    (let [g (q/create-graphics w h)]
      (q/background 25 10 255)
      (doall (map d/draw-bezier curves))
      (q/save (str "/out/livingroom/" (format "%04d" frame) ".png")))))

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
  :update go-next-frame
  :draw draw-state
  :mouse-clicked mouse-press
;   :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

;; create 5 renders with different seeds
;; save to files
;; in the specified folder
;; 
;; 
;; start small:
;; just use create graphics, with graphics and save to save
(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.squares2 :as sq2]
            [atsketch.curves :as crv]
            [atsketch.sketch-util :refer [save-frame]]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]))

(def w 8000)
(def h 8000)

(defn rainbow-color [ncols col row]
  {:h (* 255 (/ col ncols)) :s 200 :b 255 :a (- 250 (* 10 row))})

(defn gen-circle-squares []
  (let [n 550
        size 14
        displ (* 1.25 size)
        nits 30
        alpha-degrade 5
        r 1600
        
        get-color #(cond (and (> % (+ (/ n 4) 4)) (< % (- (/ n 2) 7))) :red
                         (and (> % (+ (/ n 2) 11)) (< % (- (+ (/ n 2) (/ n 4)) 3))) :blue
                         :else :black)
        skip? #(and (> % (- (/ n 2) 5)) (< % (+ (/ n 2) 9)))
        red-h 253
        blue-h 141]
    (vec
     (for [i (range n)]
       (let [c (get-color i)
             basecolor {:h (+ (rand-int 6) (random-c 0 2)
                              (cond (= :red c) red-h
                                    (= :blue c) blue-h
                                    :else 0))
                        :s 210
                        :b (if (= :black c) 0 255)
                        :a 250}]
         (vec (for [j (when (not (skip? i))
                        (range (random-cl (* nits 0.5) (* nits 0.25) 1 (* 1.5 nits))))]
                {:coords {:x 0
                          :y (+ r (* j displ))
                          :w size
                          :h size}
                 :color  (update basecolor :a #(- % (+ (* alpha-degrade j) (random-c 0 30))))})))))))

(defn go-next-frame [{:keys [frame max-frame] :as state}]
  (if (>= frame max-frame)
    (assoc state
           :frame max-frame
           :done true
           :circle-squares [])
    (assoc state
           :frame (inc frame)
           :circle-squares (gen-circle-squares))))


(defn update-state [state] go-next-frame)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:frame 0
   :max-frame 0
   :done false
   :circle-squares (gen-circle-squares)
   :w w
   :h h
   :background {:h 0 :s 0 :b 255 :a 255}
   :color {:h 14 :s 15 :b 15}
   :curves []})

(defn settings []
  (q/smooth 100))

(defn draw-state [{:keys [circle-squares background frame done w h]}]
  (if done
    (q/background 0)
    (let [g (q/create-graphics w h)]
      (q/background (:h background) (:s background) (:b background) (:a background))
      (let [cx (/ w 2) cy (/ h 2)
            da (/ (* 2 q/PI) (count circle-squares))]
        (q/translate cx cy)
        (doseq [row circle-squares]
          (doseq [s row]
            (d/draw-rect s))
          (q/rotate da)))
      (q/save (str "/out/sun/" (format "%04d" frame) ".png")))))

;; 400 0 300 200 540 500 400 1000


(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :settings settings
  ; update-state is called on each iteration before draw-state.
  :update go-next-frame
  :draw draw-state
  :mouse-clicked save-frame
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
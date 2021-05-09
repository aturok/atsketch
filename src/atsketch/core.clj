(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.squares2 :as sq2]
            [atsketch.curves :as crv]
            [atsketch.genesq :as genesq]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]))

(def w 1000)
(def h 1000)

(defn rainbow-color [ncols col row]
  {:h (* 255 (/ col ncols)) :s 200 :b 255 :a (- 250 (* 10 row))})

(defn gen-circle-squares []
  (let [n 550
        size 14
        displ (* 1.25 size)
        skip 14
        nits 30
        alpha-degrade 5
        r 1600]
    (vec
     (for [i (range n)]
       (let [basecolor {:h (+ 18 (random-c 0 8)) :s 180 :b 255 :a 250}]
         (vec (for [j (when (not= (int (/ skip 2)) (mod i skip))
                        (range (random-cl (* nits 0.5) (* nits 0.25) 1 (* 1.5 nits))))]
                {:coords {:x 0
                          :y (+ r (* j displ))
                          :w size
                          :h size}
                 :color  (update basecolor :a #(- % (+ (* alpha-degrade j) (random-c 0 20))))})))))))

(defn go-next-frame [{:keys [frame max-frame] :as state}]
  (if (>= frame max-frame)
    (assoc state
           :frame max-frame
           :done true
           :circle-squares [])
    (assoc state
           :frame (inc frame)
           :circle-squares (gen-circle-squares))))


(defn update-state [state] state)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (let [genes (vec (take (* 200 200) (repeatedly genesq/x-gene)))
        fene-gen (genesq/fene-generator {:rows 200
                                         :cols 200
                                         :w 4
                                         :h 4
                                         :d-row 5
                                         :d-col 5})]
    {:frame 0
     :max-frame 0
     :done false
     :fene-gen fene-gen
     :genes genes
     :fenes (fene-gen genes)
     :w w
     :h h
     :color {:h 4 :s 0 :b 255}
     :curves []}))

(defn settings []
  (q/smooth 100))

(defn draw-state [{:keys [fenes frame w h]}]
  (if false
    (q/background 0)
    (let [g (q/create-graphics w h)]
      (q/background -0 0 0)
      (doseq [f fenes]
        (d/draw-rect f))
      (comment (q/save (str "/out/genesq/" (format "%04d" frame) ".png"))))))

;; 400 0 300 200 540 500 400 1000

(defn mouse-press [& _]
  (q/save-frame "out/pretty-pic-#####.tiff"))


(defn key-pressed [{:keys [fene-gen] :as state} {:keys [key]}]
  (if (= :right key)
    (let [new-g (vec (take (* 200 200) (repeatedly genesq/x-gene)))]
      (assoc state :genes new-g :fenes (fene-gen new-g)))
    state))

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
  :key-pressed key-pressed
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
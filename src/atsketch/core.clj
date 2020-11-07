(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh :refer [symmetric line offset-line]]
            [atsketch.distort :as dst :refer [distort-point]]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]
            [genartlib.curves :as curves]))

(def w 1000)
(def h 1000)


(mapv #(- % 30) [65 70 75 80 85 90])
;; => [35 40 45 50 55 60]

(def lines
  (let [from-center 100
        step 10
        center-line-step 10
        distortion 4
        center-line-distortion 3
        wc (/ w 2)
        hc (/ h 2)
        center-offsets (concat [0] (symmetric [5]))
        satelite-offsets (symmetric (flatten (repeat 1 [35 40 45 50 55 60 65 70 75 80 85 90])))
        distorter (fn [x y] (partial distort-point [x y]))
        ;; dist-x (distorter distortion 0)
        ;; dist-y (distorter 0 0)
        c-dist-x (distorter center-line-distortion 0)
        c-dist-y (distorter 0 center-line-distortion)
        dist-x #(dst/off-center-growing-distort-x % hc hc distortion)
        dist-y #(dst/off-center-growing-distort-y % wc wc distortion)

        center-colors (mapv (fn [h] {:h h :s 200 :b 255}) [230 237])
        satelite-colors (mapv (fn [h] {:h h :s 200 :b 255}) [128 128 135 140 156 172])
        ;; make less distortion closer to center


        center-h-lines (for [v-offset center-offsets
                             [start end step] [[(- wc from-center) 0 (- center-line-step)]
                                               [(+ wc from-center) w center-line-step]]]
                         (line :start [start (+ hc v-offset)] :step [step 0] :end [end (+ hc v-offset)]
                               :distorter c-dist-y :smooth? true :color (rand-nth center-colors)))
        center-v-lines (for [h-offset center-offsets
                             [start end step] [[(- hc from-center) 0 (- center-line-step)]
                                               [(+ hc from-center) w center-line-step]]]
                         (line :start [(+ wc h-offset) start] :step [0 step] :end [(+ wc h-offset) end]
                               :distorter c-dist-x :smooth? true  :color (rand-nth center-colors)))
        satelite-h-lines (for [v-offset satelite-offsets
                               [start end step] [[(- wc from-center) 0 (- step)]
                                                 [(+ wc from-center) w step]]]
                           (line :start [start (+ hc v-offset)] :step [step 0] :end [end (+ hc v-offset)]
                                 :distorter dist-y :smooth? true  :color (rand-nth satelite-colors)))

        satelite-v-lines (for [h-offset satelite-offsets
                               [start end step] [[(- hc from-center) 0 (- step)]
                                                 [(+ hc from-center) w step]]]
                           (line :start [(+ wc h-offset) start] :step [0 step] :end [(+ wc h-offset) end]
                                 :distorter dist-x :smooth? true  :color (rand-nth satelite-colors)))]
    (concat center-h-lines
            center-v-lines
            (map #(offset-line % [0 1] {:h 0 :s -40 :b -80}) satelite-h-lines)
            (map #(offset-line % [0 -1] {:h 0 :s -40 :b -80}) satelite-h-lines)
            (map #(offset-line % [0 2] {:h 0 :s -50 :b -150}) satelite-h-lines)
            (map #(offset-line % [0 -2] {:h 0 :s -50 :b -150}) satelite-h-lines)
            (map #(offset-line % [0 3] {:h 0 :s -100 :b -180}) satelite-h-lines)
            (map #(offset-line % [0 -3] {:h 0 :s -100 :b -180}) satelite-h-lines)
            satelite-h-lines
            (map #(offset-line % [1 0] {:h 0 :s -40 :b -80}) satelite-v-lines)
            (map #(offset-line % [-1 0] {:h 0 :s -40 :b -80}) satelite-v-lines)
            (map #(offset-line % [2 0] {:h 0 :s -50 :b -150}) satelite-v-lines)
            (map #(offset-line % [-2 0] {:h 0 :s -50 :b -150}) satelite-v-lines)
            (map #(offset-line % [3 0] {:h 0 :s -100 :b -180}) satelite-v-lines)
            (map #(offset-line % [-3 0] {:h 0 :s -100 :b -180}) satelite-v-lines)
            satelite-v-lines)))

(def lines2
  (let [step 50
        line-step 50
        distortion 10
        satelite-colors (mapv (fn [h] {:h h :s 200 :b 220}) [128 128 135 140 156 172])
        dist-x #(dst/off-xy-center-growing-distort % 1000 h distortion)

        starts (concat (for [start-x (range 0 (dec w) line-step)]
                         [start-x 0])
                       (for [start-y (range line-step (dec h) line-step)]
                         [0 start-y]))


        lines (for [start starts]
                (line :start start :step [step step] :end [(inc w) (inc h)]
                      :color (rand-nth satelite-colors)
                      :distorter dist-x :smooth? true))]
    (concat []
            (map #(offset-line % [1 0] {:h 0 :s -80 :b -10}) lines)
            (map #(offset-line % [-1 0] {:h 0 :s -80 :b -10}) lines)
            (map #(offset-line % [2 0] {:h 0 :s -90 :b -20}) lines)
            (map #(offset-line % [-2 0] {:h 0 :s -90 :b -20}) lines)
            (map #(offset-line % [3 0] {:h 0 :s -100 :b -70}) lines)
            (map #(offset-line % [-3 0] {:h 0 :s -100 :b -70}) lines)
            (map #(offset-line % [3 0] {:h 0 :s -110 :b -80}) lines)
            (map #(offset-line % [-3 0] {:h 0 :s -110 :b -80}) lines)
            lines
            [])))

(defn circle-stuff []
  (let [distortion 25
        dist-x #(dst/off-xy-center-growing-distort % 1000 h distortion)
        produce-circle (fn [radius]
                         (->> (sh/circle-points [(/ w 2) (/ h 2)] radius 50)
                              (map dist-x)
                              curves/chaikin-curve
                              sh/close-line))
        satelite-colors (mapv (fn [h] {:h h :s 240 :b 220}) [140 128 128 135 156 172])]
    (for [r (take 5 (iterate #(- % 25) (- (/ w 2) 75)))]
      {:points (produce-circle r) :color (first satelite-colors)})))

(defn squares []
  (let [_ (q/random-seed 13)
        my-rand-root (java.util.Random. 13)
        rand (fn [] (.nextDouble my-rand-root))
        gen-size #(random-cl 20 10 4 30)
        gen-dims #(sort [(gen-size) (gen-size)])

        backs (repeatedly (* 10 w) (fn []
                                     {:coords {:x (* (rand) w)
                                               :y (* (rand) h)
                                               :w 2
                                               :h 2}
                                      :color {:h (random-cl 154 10 0 255)
                                              :s 150
                                              :b 100
                                              :a (random-cl 20 30 0 255)}}))

        gen-y (fn [x-around x-value y-center y-range]
                (let [proximity (- 1 (/ (Math/abs (- x-value x-around)) w))]
                  (random-c (* y-center proximity) (* y-range (+ proximity 0.05)))))

        band-h (* (float (/ h 3)))
        top-band-center 50
        n-tops (int (* 0.7 w))
        tops (vec (repeatedly n-tops (fn []
                                       (let [[sw sh] (gen-dims)
                                             x (random-c (* w 0.30) (/ w 4))]
                                         {:coords {:x x
                                                   :y (gen-y (* w 0.30) x top-band-center (/ band-h 2))
                                                   :w sw
                                                   :h sh}
                                          :color {:h (random-cl 235 10 0 255) :s 255 :b 255
                                                  :a (random-cl 150 50 0 255)}}))))

        tops-backs (vec (repeatedly (* 3 n-tops) (fn []
                                                   {:coords {:x (random-c (* w 0.30) (/ w 4))
                                                             :y (random-c top-band-center band-h)
                                                             :w 1
                                                             :h 1}
                                                    :color {:h (random-cl 235 10 0 255) :s 255 :b 255
                                                            :a (random-cl 50 50 0 255)}})))

        low-band-center 200
        n-bots (int (* 1 w))
        bottoms (vec (repeatedly n-bots (fn []
                                          (let [[sw sh] (gen-dims)
                                                x (random-c (* w 0.70) (/ w 4))]
                                            {:coords {:x x
                                                      :y (- h (gen-y (* w 0.70) x low-band-center (/ band-h 2)))
                                                      :w sw
                                                      :h sh}
                                             :color {:h (random-cl 154 10 0 255) :s 255 :b 255
                                                     :a (random-cl 150 75 0 255)}}))))
        bots-backs (vec (repeatedly (* 4 n-tops) (fn []
                                                   {:coords {:x (random-c (* w 0.70) (/ w 4))
                                                             :y (- h (random-c low-band-center band-h))
                                                             :w 1
                                                             :h 1}
                                                    :color {:h (random-cl 154 10 0 255) :s 255 :b 255
                                                            :a (random-cl 50 50 0 255)}})))
        xxx :xxx]
    (concat ;backs
            tops-backs
            bots-backs
            bottoms
            tops)))

(defn squares2 []
  (let [_ (q/random-seed 13)
        gen-size #(random-cl 20 10 4 30)
        gen-dims (fn [] [(gen-size) (gen-size)])
        
        n (* 2 w)
        tops (vec (repeatedly n (fn []
                                  (let [[sw sh] (gen-dims)
                                        x (random-c (* 0.5 w) (* 0.1 w))
                                        y (random-c (* 0.5 h) (* 0.1 h))
                                        xc (- x (* 0.5 w))
                                        yc (- y (* 0.5 h))
                                        sins (/ yc (Math/sqrt (+ (* yc yc) (* xc xc))))]
                                    {:coords {:x x
                                              :y y
                                              :w sw
                                              :h sh}
                                     :color {:h (+ 200 (* sins 50))
                                             :s 255 :b 255
                                             :a (random-cl 180 30 0 255)}}))))
        xxx :xxx]
    (concat []
            tops)))

(defn update-state [state] (assoc state :rects (squares2)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color {:h 4 :s 0 :b 255}
   :rects (squares2)})

(defn settings []
  (q/smooth 0))



(defn draw-state [{:keys [rects]}]
  (q/background 0)  
  (doall (map #(d/draw-rect-with-shadow % (sh/from-point-displacer 5 (* 0.5 w) (* 0.5 h))) rects)))

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
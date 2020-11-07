(ns atsketch.firsttries
  (:require [atsketch.shapes :as sh :refer [symmetric line offset-line]]
            [atsketch.distort :as dst :refer [distort-point]]
            [genartlib.curves :as curves]))

(def w 1000)
(def h 1000)

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
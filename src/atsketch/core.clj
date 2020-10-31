(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [genartlib.curves :as curves]))

(def w 1000)
(def h 1000)

(defn h-line-points [y x-from x-to step]
  (map (fn [x] [x y])
       (range x-from x-to step)))

(defn v-line-points [x y-from y-to step]
  (map (fn [y] [x y])
       (range y-from y-to step)))

(defn distort-g [d v]
  (+ v (* d (q/random-gaussian))))

(defn distort-point [[xd yd] [x y]]
  [(distort-g xd x) (distort-g yd y)])

(defn points []
  (->> (h-line-points 250 0 500 10)
       (map distort-point)))


(defn symmetric [ns]
  (concat ns
          (map - ns)))

(defn map-but-last [f s]
  (concat (map f (butlast s))
          [(last s)]))
(defn map-but-first [f s]
  (concat [(first s)]
          (map f (next s))))
(defn map-but-edges [f s]
  (concat [(first s)]
          (map f (butlast (next s)))
          [(last s)]))

(defn distorter [x y]
  (partial distort-point [x y]))

(defn line [& {:keys [start step end color distorter smooth?] :or {smooth? false color {:h 0 :s 0 :b 0}}}]
  {:pre [(some? start) (some? step) (some? end) (let [[x y] step] (or (not (zero? x)) (not (zero? y))))]
   :post [(some? (:points %)) (some? (:color %))]}
  (let [[start-x start-y] start
        [step-x step-y] step
        [end-x end-y] end
        do-distort (if distorter #(map-but-edges distorter %) identity)
        do-smooth (if smooth? curves/chaikin-curve-retain-ends identity)
        points (->> (map vector
                         (if (zero? step-x) (repeat start-x) (range start-x end-x step-x))
                         (if (zero? step-y) (repeat start-y) (range start-y end-y step-y)))
                    do-distort
                    do-smooth)]
    {:points (vec points)
     :color color}))

(float (* 255 (/ 334 360)))

(def lines
  (let [from-center 60
        step 5
        center-line-step 10
        distortion 4
        center-line-distortion 3
        wc (/ w 2)
        hc (/ h 2)
        center-offsets (concat [0] (symmetric [5]))
        satelite-offsets (symmetric (flatten (repeat 2 [25 30 35 40 45 50])))
        distorter (fn [x y] (partial distort-point [x y]))
        dist-x (distorter distortion 0)
        dist-y (distorter 0 distortion)
        c-dist-x (distorter center-line-distortion 0)
        c-dist-y (distorter 0 center-line-distortion)
        
        center-colors (mapv (fn [h] {:h h :s 200 :b 255}) [ 230 237])
        satelite-colors (mapv (fn [h] {:h h :s 200 :b 255}) [128 156 172])
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
    (->> (concat center-h-lines
                 center-v-lines
                 satelite-h-lines
                 satelite-v-lines))))

(defn update-state [state] (assoc state :lines lines))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color {:h 4 :s 0 :b 255}
   :lines lines})

(defn settings []
  (q/smooth 2))


(defn draw-line [{:keys [points color]}]
  (q/stroke (:h color) (:s color) (:b color))
  (doseq [[[x1 y1] [x2 y2]] (map vector points (next points))]
    (q/line x1 y1 x2 y2)))

(defn draw-state [{:keys [lines]}]
  (q/background 0)
  (doall (map draw-line lines)))



(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :settings settings
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
;   :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

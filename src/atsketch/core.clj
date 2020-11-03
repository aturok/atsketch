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


(defn off-center-growing-distort-x [[x y] center max-off max-d]
  [(distort-g (* max-d (float (min (/ (Math/abs (- y center)) max-off) 1))) x) y])
(defn off-center-growing-distort-y [[x y] center max-off max-d]
  (let [[y x] (off-center-growing-distort-x [y x] center max-off max-d)]
    [x y]))

(defn off-xy-center-growing-distort [[x y] center max-off max-d]
  (let [measure (max (float (min (/ (Math/abs (- y center)) max-off) 1))
                     (float (min (/ (Math/abs (- x center)) max-off) 1)))]
    [(distort-g (* max-d measure) x) (distort-g (* max-d measure) y)]))

(mapv #(- % 30) [65 70 75 80 85 90])
;; => [35 40 45 50 55 60]

(defn offset-line [{:keys [points color]} [x-offset y-offset] color-offset]
  {:points (map (fn [[x y]] [(+ x x-offset) (+ y y-offset)]) points)
   :color {:h (+ (:h color) (:h color-offset))
           :s (+ (:s color) (:s color-offset))
           :b (+ (:b color) (:b color-offset))}})

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
        dist-x #(off-center-growing-distort-x % hc hc distortion)
        dist-y #(off-center-growing-distort-y % wc wc distortion)

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
        dist-x #(off-xy-center-growing-distort % 1000 h distortion)

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

(defn circle-points-0 [radius step]
  (let [rsq (* radius radius)
        other #(Math/sqrt (- rsq (* % %)))
        points (concat (for [y (range 0 radius step)]
                         [(- (other y)) y])
                       (for [y (range radius 0 (- step))]
                         [(other y) y])
                       (for [y (range 0 (- radius) (- step))]
                         [(other y) y])
                       (for [y (range (- radius) 0 step)]
                         [(- (other y)) y]))]
    points))


(defn circle-points [[cx cy] radius step]
  (map (fn [[x y]] [(+ x cx) (+ y cy)]) (circle-points-0 radius step)))

(defn close-line [line]
  (concat line [(first line)]))

(defn circle-stuff []
  (let [distortion 25
        dist-x #(off-xy-center-growing-distort % 1000 h distortion)
        produce-circle (fn [radius]
                         (->> (circle-points [(/ w 2) (/ h 2)] radius 50)
                              (map dist-x)
                              curves/chaikin-curve
                              close-line))
        satelite-colors (mapv (fn [h] {:h h :s 240 :b 220}) [140 128 128 135 156 172])]
    (for [r (take 5 (iterate #(- % 25) (- (/ w 2) 75)))]
      {:points (produce-circle r) :color (first satelite-colors)})))

(defn random-c [center range]
  (-> (q/random-gaussian)
      (* range)
      (+ center)))

(defn random-cl [center range mmin mmax]
  (-> (random-c center range)
      (max mmin)
      (min mmax)))

(defn squares []
  (let [gen-size #(random-cl 20 10 4 30)
        gen-dims #(sort [(gen-size) (gen-size)])
        
        top-band-h (* (float (/ h 3)))
        top-band-center 50
        tops (vec (repeatedly 700 (fn []
                                    (let [[sw sh] (gen-dims)]
                                      {:coords {:x (random-c (* w 0.30) (/ w 4))
                                                :y (random-c top-band-center (/ top-band-h 2))
                                                :w sw
                                                :h sh}
                                       :color {:h (random-cl 235 10 0 255) :s 255 :b 255
                                               :a (random-cl 150 50 0 255)}}))))
        
        low-band-h (* (float (/ h 3)))
        low-band-center (- h 80)
        bottoms (vec (repeatedly 1000 (fn []
                                       (let [[sw sh] (gen-dims)]
                                         {:coords {:x (random-c (* w 0.70) (/ w 4))
                                                   :y (random-c low-band-center (/ low-band-h 2))
                                                   :w sw
                                                   :h sh}
                                          :color {:h (random-cl 154 10 0 255) :s 255 :b 255
                                                  :a (random-cl 150 75 0 255)}}))))
        xxx :xxx]
    (concat bottoms
            tops)))

(defn update-state [state] state) ;; (assoc state :lines (circle-stuff)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color {:h 4 :s 0 :b 255}
   :rects (squares)
   :lines (circle-stuff)})

(defn settings []
  (q/smooth 0))

(defn set-color! [which {:keys [h s b a] :or {h 0 s 255 b 255 a 255}}]
  (which h s b a))

(defn draw-line [{:keys [points color]}]
  (set-color! q/stroke color)
  (doseq [[[x1 y1] [x2 y2]] (map vector points (next points))]
    (q/line x1 y1 x2 y2)))

(defn draw-rect [{:keys [coords color]}]
  (set-color! q/stroke color)
  (set-color! q/fill color)
  (q/rect (:x coords) (:y coords) (:w coords) (:h coords)))

(defn draw-state [{:keys [rects]}]
  (q/background 0)  
  (doall (map draw-rect rects)))

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
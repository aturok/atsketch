(ns atsketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def w 500)
(def h 500)

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

(def distortion 3)

(defn symmetric [ns]
  (concat ns
          (map - ns)))

(def from-center 36)

(defn map-but-last [f s]
  (concat (map f (butlast s))
          [(last s)]))
(defn map-but-first [f s]
  (concat [(first s)]
          (map f (next s))))

(defn distorter [x y]
  (partial distort-point [x y]))

(def lines
  (let [offsets (concat [0] (symmetric [5 25 30 35]))
        dist-x (partial distort-point [5 0])
        dist-y (partial distort-point [0 5])]
    (concat (mapcat  (fn [offset]
                       (let [l1 (h-line-points (+ (/ h 2) offset) 0 (- (/ w 2) from-center) 4)
                             l2 (h-line-points (+ (/ h 2) offset) (+ (/ w 2) from-center) w 4)]
                         [(map-but-last dist-y l1)
                          (map-but-first dist-y l2)]))
                     offsets)
            (mapcat  (fn [offset]
                       (let [l1 (v-line-points (+ (/ w 2) offset) 0 (- (/ h 2) from-center) 4)
                             l2 (v-line-points (+ (/ w 2) offset) (+ (/ h 2) from-center) h 4)]
                         [(map-but-last dist-x l1)
                          (map-but-first dist-x l2)]))
                     offsets))))

(defn update-state [state] (assoc state :lines lines))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color {:h 0 :s 0 :b 0}
   :lines lines})


(defn draw-line [points]
  (doseq [[[x1 y1] [x2 y2]] (map vector points (next points))]
    (q/line x1 y1 x2 y2)))

(defn draw-state [{:keys [lines]}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 255)
  (doall (map draw-line lines)))



(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
;   :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

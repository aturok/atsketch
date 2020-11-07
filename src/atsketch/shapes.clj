(ns atsketch.shapes
  (:require [quil.core :as q]
            [atsketch.util :refer [map-but-edges]]
            [genartlib.curves :as curves]))

(defn symmetric [ns]
  (concat ns
          (map - ns)))

(defn h-line-points [y x-from x-to step]
  (map (fn [x] [x y])
       (range x-from x-to step)))

(defn v-line-points [x y-from y-to step]
  (map (fn [y] [x y])
       (range y-from y-to step)))


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

(defn offset-line [{:keys [points color]} [x-offset y-offset] color-offset]
  {:points (map (fn [[x y]] [(+ x x-offset) (+ y y-offset)]) points)
   :color {:h (+ (:h color) (:h color-offset))
           :s (+ (:s color) (:s color-offset))
           :b (+ (:b color) (:b color-offset))}})

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

(defn rect-center [{:keys [x y w h]}]
  {:x (+ x (* 0.5 w))
   :y (+ y (* 0.5 h))})

(defn shadow-rect [grow-factor displacementfn {:keys [coords]}]
  (let [[displacement-x displacement-y] (displacementfn coords)]
    {:x (+ (:x coords) displacement-x)
     :y (+ (:y coords) displacement-y)
     :w (* grow-factor (:w coords))
     :h (* grow-factor (:h coords))}))

(defn from-point-displacer [factor center-x center-y]
  (fn [coords]
    (let [center (rect-center coords)]
      [(* factor (/ (- (:x center) center-x) center-x))
       (* factor (/ (- (:y center) center-y) center-y))])))

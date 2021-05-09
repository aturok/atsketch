(ns atsketch.genesq
  (:require [atsketch.draw :as d]))

(def _r_ (java.util.Random. 1))
(defn rint
  ([max] (.nextInt _r_ max))
  ([min max-inc] (+ min (.nextInt _r_ (- (inc max-inc) min)))))

(def gene-template [{:title :dx :min -5 :max 5 :default 0}
                    {:title :dy :min -5 :max 5 :default 0}
                    {:title :dw :min -5 :max 5 :default 0}
                    {:title :dh :min -5 :max 5 :default 0}
                    {:title :h :min 0 :max 255 :default 0}
                    {:title :s :min 0 :max 255 :default 0}
                    {:title :b :min 0 :max 255 :default 0}
                    {:title :a :min 0 :max 255 :default 0}])

(defn default-gene []
  [0 0 0 0 0 0 0 0])

(defn random-gene []
  [(rint -5 5)
   (rint -5 5)
   (rint -5 5)
   (rint -5 5)
   (rint 0 255)
   (rint 0 255)
   (rint 0 255)
   (rint 0 255)])

(defn x-gene []
  [(rint -1 1)
   (rint -1 1)
   0
   0
   (rint 0 255)
   200
   (rint 0 255)
   (rint 150 255)])


(defn draw-genes [{:keys [rows cols
                          w h
                          d-row d-col]}
                  genes]
  (doseq [i (range rows)]
    (doseq [j (range cols)]
      (let [[dx dy dw dh hue s b a] (nth genes (+ j (* cols i)))
            y (* d-row i)
            x (* d-col j)]
        (d/draw-rect {:coords {:x (+ x dx)
                               :y (+ y dy)
                               :w (+ w dw)
                               :h (+ h dh)}
                      :color {:h hue :s s :b b :a a}})))))

(defn fene-generator [{:keys [rows cols
                              w h
                              d-row d-col]}]
  (fn [genes]
    (vec
     (flatten
      (for [i (range rows)]
        (for [j (range cols)]
          (let [[dx dy dw dh hue s b a] (nth genes (+ j (* cols i)))
                y (* d-row i)
                x (* d-col j)]
            {:coords {:x (+ x dx)
                      :y (+ y dy)
                      :w (+ w dw)
                      :h (+ h dh)}
             :color {:h hue :s s :b b :a a}})))))))

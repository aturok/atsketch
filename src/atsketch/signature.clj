(ns atsketch.signature
  (:require [quil.core :as q]))

(defn draw-signature [& {:keys [color]}]
  (let [empty? #{[1 3] [1 13] [1 19] [1 20] [1 21] [1 22] [1 23]
                 [2 3] [2 11] [2 13] [2 19]
                 [3 3] [3 7] [3 11] [3 13] [3 14] [3 15] [3 16] [3 19] [3 20] [3 21]
                 [4 3] [4 7] [4 11] [4 13] [4 17] [4 19]
                 [5 4] [5 5] [5 6] [5 8] [5 9] [5 10] [5 13] [5 17] [5 20] [5 21] [5 22] [5 23]}]
    (doall
     (doseq [y (range 7)]
       (doseq [x (range 27)]
         (when (not (empty? [y x]))
           (apply q/fill (conj color (Math/floor (q/random 80 110))))
           (q/rect x y 1 1)))))))
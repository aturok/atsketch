(ns atsketch.util
  (:require [quil.core :as q]))

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

(defn random-c [center range]
  (-> (q/random-gaussian)
      (* range)
      (+ center)))

(defn random-cl [center range mmin mmax]
  (-> (random-c center range)
      (max mmin)
      (min mmax)))

(defn rrand-uniform [seed]
  (let [my-rand-root (java.util.Random. seed)]
    (fn [] (.nextDouble my-rand-root))))

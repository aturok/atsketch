(ns atsketch.squares1
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.distort :as dst]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]))

(defn squares [w h]
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

(defn squares2 [w h]
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
(ns atsketch.squares2
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.shapes :as sh]
            [atsketch.uber-shapes :as ush]
            [atsketch.distort :as dst]
            [atsketch.util :as util :refer [random-c random-cl]]
            [atsketch.draw :as d]))


(defn squares3 [w h]
  (let [_ (q/random-seed 13)
        rrand (util/rrand-uniform 13)

        block-size-rel 0.3
        block-displ 0.15
        block-cnt (* 0.4 w)

        gen-size #(random-cl 20 15 4 30)
        gen-dims (fn [] (sort [(gen-size) (gen-size)]))

        blue-gen (fn [_] {:h (random-cl 160 8 0 255)
                          :s 255 :b 255
                          :a (random-cl 180 30 0 255)})

        red-gen (fn [_] {:h (random-cl 0 4 0 255)
                         :s 255 :b 255
                         :a (random-cl 180 30 0 255)})

        tl (ush/rects-rect block-cnt
                           rrand
                           {:x (* block-displ w)
                            :y (* block-displ h)
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           blue-gen)

        tr (ush/rects-rect block-cnt
                           rrand
                           {:x (- w (* block-displ w) (* block-size-rel w))
                            :y (* block-displ h)
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           red-gen)

        bl (ush/rects-rect block-cnt
                           rrand
                           {:x (* block-displ w)
                            :y (- h (* block-displ h) (* block-size-rel h))
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           red-gen)

        br1 (ush/rects-rect (* 0.5 block-cnt)
                            rrand
                            {:x (- w (* block-displ w) (* block-size-rel w))
                             :y (- h (* block-displ h) (* block-size-rel h))
                             :w (* block-size-rel w)
                             :h (* block-size-rel h)}
                            gen-dims
                            blue-gen)
        br2 (ush/rects-rect (* 0.5 block-cnt)
                            rrand
                            {:x (- w (* block-displ w) (* block-size-rel w))
                             :y (- h (* block-displ h) (* block-size-rel h))
                             :w (* block-size-rel w)
                             :h (* block-size-rel h)}
                            gen-dims
                            red-gen)
        br (util/shuffle rrand (concat br1 br2))


        _ :_]
    (concat []
            tl
            tr
            bl
            br
            [])))
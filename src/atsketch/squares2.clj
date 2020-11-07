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
        
        gray-gen (fn [_] {:h 0
                          :s 0 :b (random-cl 80 10 0 255)
                          :a (random-cl 180 30 0 255)})

        tl (ush/rects-rect block-cnt
                           rrand
                           {:x (* block-displ w)
                            :y (* block-displ h)
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           red-gen)

        tr (ush/rects-rect block-cnt
                           rrand
                           {:x (- w (* block-displ w) (* block-size-rel w))
                            :y (* block-displ h)
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           blue-gen)

        bl (ush/rects-rect block-cnt
                           rrand
                           {:x (* block-displ w)
                            :y (- h (* block-displ h) (* block-size-rel h))
                            :w (* block-size-rel w)
                            :h (* block-size-rel h)}
                           gen-dims
                           gray-gen)


        _ :_]
    (concat []
            tl
            tr
            bl
            [])))


(defn tris1 [w h]
  (let [_ (q/random-seed 17)
        rrand (util/rrand-uniform 13)

        block-size-rel 0.3
        bw (* block-size-rel w)
        bh (* block-size-rel h)
        block-displ 0.175
        blb (* block-displ w)
        brb (- w (* block-displ w))
        btb (* block-displ h)
        bbb (- h (* block-displ h))
        block-cnt (* 1 w)
        
        

        gen-size #(random-cl 10 5 2 15)
        gen-dims (fn [] (sort [(gen-size) (gen-size)]))

        blue-gen (fn [_] {:h (random-cl 160 8 0 255)
                          :s 255 :b 255
                          :a (random-cl 180 30 0 255)})

        red-gen (fn [_] {:h (random-cl 0 4 0 255)
                         :s 255 :b 255
                         :a (random-cl 180 30 0 255)})

        gray-gen (fn [_] {:h 0
                          :s 0 :b (random-cl 250 10 0 255)
                          :a (random-cl 180 30 0 255)})

        tl (ush/rects-triangle block-cnt
                               rrand
                               [[blb (+ btb bh)]
                                [(+ blb bw) (+ btb bh)]
                                [(+ blb bw) btb]]
                               gen-dims
                               red-gen)

        tr (ush/rects-triangle block-cnt
                               rrand
                               [[brb (+ btb bh)]
                                [(- brb bw) (+ btb bh)]
                                [(- brb bw) btb]]
                               gen-dims
                               blue-gen)
        
        bl (ush/rects-triangle block-cnt
                               rrand
                               [[blb (- bbb bh)]
                                [(+ blb bw) (- bbb bh)]
                                [(+ blb bw) bbb]]
                               gen-dims
                               gray-gen)


        _ :_]
    (concat []
            tl
            tr
            bl
            [])))


(-> (let [_ :_;(q/random-seed 13)
          rrand (util/rrand-uniform 13)

          w 1000
          h 1000
          block-size-rel 0.3
          bw (* block-size-rel w)
          bh (* block-size-rel h)
          block-displ 0.15
          blb (* block-displ w)
          brb (- w (* block-displ w))
          btb (* block-displ h)
          bbb (- h (* block-displ h))
          block-cnt (* 0.4 w)

          gen-dims (constantly [5 5])
          red-gen (constantly {})

          tl (ush/rects-triangle block-cnt
                                 rrand
                                 [[blb (+ btb bh)]
                                  [(+ blb bw) (+ btb bh)]
                                  [(+ blb bw) btb]]
                                 gen-dims
                                 red-gen)


          _ :_]
      (concat []
              tl
              []))
first)

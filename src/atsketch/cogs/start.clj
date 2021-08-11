(ns atsketch.cogs.start
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [atsketch.util :as util]))

(defn draw-state [{:keys [w h]}]
  (q/background 0)
  (q/translate (/ w 2) (/ h 2))
  (q/stroke 200 100 200 230)
  (q/fill 0 0 0 0)
  (q/arc -30 -100 160 200 0 (/ q/PI 1)))

;; got circle
;; need to place arcs so that they fill
;; need to know the width from vertexes of arcs
;; in: circle radius
;; in: number of arcs
;; the width can be calced as 2*r*sin(2pi/n/2)
;; to draw an arc we want start angle, end angle, boundaries
;; let's start with half-circles - then width is 2rm

(defn draw-teeth [r n color background]
  (let [angle-step (-> q/PI (* 2) (/ n))
        tooth-r (* r (q/sin (/ angle-step 2)))
        displacement 0 ;(- (* 0.005 r))
        angle-offset (* 0.1 q/PI)
        hole-r (* 0.25 r)
        hole-r2 (* 0.4 r)
        cover-d (* 2 (- r (* 0.8 tooth-r)))
        [w1 w2 w3] [1 3 7 20]
        [c1 c2 c3] [color (assoc color 3 150) (assoc color 3 50) (assoc color 3 30)]
        
        rep-variating (fn [what]
                        (doseq [[w c] [[w1 c1] [w2 c2] [w3 c3]]]
                          (apply q/stroke c)
                          (q/stroke-weight w)
                          (what)))]
    
    (apply q/fill background)
    (apply q/stroke [0 0 0 0])
    (q/arc 0 0 cover-d cover-d 0 (* 2 q/PI))
    (apply q/fill [0 0 0 0])
    
    (rep-variating #(do (q/arc 0 0 hole-r hole-r 0 (* 2 q/PI))
                        (q/arc 0 0 hole-r2 hole-r2 0 (* 2 q/PI))))
    
    (doseq [_ (range n)]
      (rep-variating
       #(q/arc 0 (- 0 r displacement) (* 2 tooth-r) (* 2 tooth-r) angle-offset (- q/PI angle-offset)))
      (q/rotate angle-step))))

(def w 1000)
(def h w)

(defn draw-state [{:keys [background go teeth]}]
  (when go
    (apply q/background background)
    (q/fill 0 0 0 0)
    (doseq [{[x y] :origin :keys [color r n]} teeth]
      (q/push-matrix)
      (q/translate (+ (/ w 2) x) (+ (/ h 2) y))
      (draw-teeth r n color background)
      (q/pop-matrix))))

(defn upd-state [{:keys [w h]}]
  {:w w
   :h h
   :go true
   :background [0 0 0 255]
   :color [140 250 250 230]
   :teeth (do (q/random-seed 19)
              (vec (repeatedly 300
                               (fn []
                                 (let [size (q/random 100 160)
                                       n (int (Math/floor (q/random 9 15)))]
                                   {:color [(q/random 120 180) 250 250 230]
                                    :origin [(util/random-c 0 (* 0.25 w))
                                             (util/random-c 0 (* 0.25 h))]
                                    :r size
                                    :n n})))))})

(q/defsketch atsketch
  :title "You spin my circle right round"
  :size [w h]
  :setup (fn setup []
           (q/frame-rate 30)
           (q/color-mode :hsb)
           {:w w
            :h h})
  :settings (fn settings []
              (q/smooth))
  :update upd-state
  :draw draw-state
  :middleware [m/fun-mode])

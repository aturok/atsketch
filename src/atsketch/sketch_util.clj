(ns atsketch.sketch-util
  (:require [quil.core :as q]))

(defn save-frame [& _]
  (q/save-frame "out/pretty-pic-#####.tiff"))
(ns main
  (:require [clojure.string :as str :refer [split]]))

(defn slurp-n-split [f]
  "pulls in the input and splits it on commas then in direction/number pairs"
  (as-> (slurp "example1") it 
             (split it #"\n")
             (map #(split % #",") it)
             (map (fn [wire] 
                    (map (fn [seg] 
                           (let [[_ a n] (re-matches #"([UDLR])(.*)" seg)]
                           [a (Integer/parseInt n)])) wire)) it)))


(def ex1 (slurp-n-split "example1"))

    
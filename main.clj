(ns main
  (:require [clojure.string :as str :refer [split]]))

;; https://adventofcode.com/2019/day/3

(defn slurp-n-split [f]
  "pulls in the input and splits it on commas then in direction/number pairs"
  (as-> (slurp "example1") it 
         (split it #"\n")
         (mapv #(split % #",") it)
         (mapv (fn [wire] 
                (mapv (fn [seg] 
                       (let [[_ a n] (re-matches #"([UDLR])(.*)" seg)]
                       [a (Integer/parseInt n)])) wire)) it)))


(def ex1 (slurp-n-split "example1"))

(defmulti coord+vec (fn [[x y :as coord] [dir mag :as v]]
  dir))
(defmethod coord+vec "U" [[x y :as coord] [dir mag :as v]]
  [x (+ y mag)])
(defmethod coord+vec "D" [[x y :as coord] [dir mag :as v]]
  [x (- y mag)])
(defmethod coord+vec "L" [[x y :as coord] [dir mag :as v]]
  [(- x mag) y])
(defmethod coord+vec "R" [[x y :as coord] [dir mag :as v]]
  [(+ x mag) y])

(defn dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))))

(defn make-ranges [wire-desc]
  (let [coords (reductions coord+vec [0,0] (ex1 0))]
  (loop [ result []
          remaining coords]
        (if-let [more (next remaining)]
         (recur (conj result [(first remaining) (second remaining)]) more)
         result))))

(def ex1-0-ranges (make-ranges (ex1 0)))
(defn intersect? [[[x1 y1] [x2 y2] :as range1] 
                  [[x3 y3] [x4 y4] :as range2]]
  "http://www.cs.swan.ac.uk/~cssimon/line_intersection.html"
  (let [form (fn [ya yb xb xa]
                  (/ (+ (* (- ya yb) (- x1 x3)) 
                        (* (- xb xa) (- y1 y3)))
                     (- (* (- x4 x3) (- y1 y2))
                        (* (- x1 x2) (- y4 y3)))))
        ta (try (form y3 y4 x4 x3) 
              (catch ArithmeticException e nil))
        tb (try (form y1 y2 x2 x1)
              (catch ArithmeticException e nil))]
        (if (and (and ta (<= ta 1) (>= ta 0))
                 (and tb (<= tb 1) (>= ta 0)))
            (let 
                 [x (+ x3 (* tb (- x4 x3)))
                  y (+ y3 (* tb (- y4 y3)))]
                  [x, y]))))

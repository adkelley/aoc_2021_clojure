(ns aoc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [split-lines]]))

(def example
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def input (slurp (io/resource "input.txt")))

(defn data->lines
  "Return collection of integers representing line
  segments x1, y1, x2, y2 . . ."
  [data]
  (->> data
       split-lines
       (mapcat #(re-seq #"\d+" %))
       (map #(Integer/parseInt %))
       (partition 4)))


(defn vertical?
  ""
  [[x1 _ x2 _]]
  (= x1 x2))

(vertical? '(0 1 0 3)) ;; => true

(defn horizontal?
  ""
  [[_ y1 _ y2]]
  (= y1 y2))

(horizontal? '(0 1 3 1)) ;; => true

(defn inclusive-range
  [a b]
  (if (<= a b) (range a (inc b)) (range a (dec b) -1)))

(defn make-points
  ""
  [line]
  (let [[x1 y1 x2 y2] line]
    (cond
      (vertical? line)   (map #(vector x1 %) (inclusive-range y1 y2))
      (horizontal? line) (map #(vector % y1) (inclusive-range x1 x2))
      :else              (map vector (inclusive-range x1 x2) (inclusive-range y1 y2)))))


(defn dangerous-areas
  ""
  [lines diagonal]
  (let [line-filter-fn (if diagonal some? (some-fn horizontal? vertical?))]
    (->> lines
         (filter line-filter-fn)
         (mapcat #(make-points %))
         frequencies
         vals
         (filter (partial <= 2))
         count)))

(dangerous-areas (data->lines example) false) ;; => 5
(dangerous-areas (data->lines example) true) ;; => 12

(dangerous-areas (data->lines input) false) ;; => 6687
(dangerous-areas (data->lines input) true) ;; => 19851

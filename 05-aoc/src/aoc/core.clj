(ns aoc.core
  (:require
   [clojure.string :as str]))

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

(def input (slurp "resources/input.txt"))

(defn data->lines
  "Return collection of integers representing line
  segments x1, y1, x2, y2 . . ."
  [data]
  (->> data
       str/split-lines
       (mapcat #(re-seq #"\d+" %))
       (map #(Integer/parseInt %))))

(defonce example->lines (data->lines example))
(defonce input->lines (data->lines input))

;; process lines
(defn line-segment
  "Return first 4 integers (line segment) from lines"
  [lines]
  (take 4 lines))

(defn x1
  "Return x1 from line-segment x1, y1, x2, y2"
  [line]
  (first line))

(defn y1
  "Return y1 from line-segment x1, y1, x2, y2"
  [line]
  (second line))

(defn x2
  "Return x2 from line-segment x1, y1, x2, y2"
  [line]
  (nth line 2))

(defn y2
  "Return y2 from line-segment x1, y1, x2, y2"
  [line]
  (nth line 3))


(nth (line-segment example->lines) 2)




(ns aoc.core
  (:require
   [clojure.java.io :as io]))

(def example
  "16,1,2,0,4,2,7,1,2,14")

(def input (slurp (io/resource "input.txt")))

(defn data->vector
  "Return [Long] from input data"
  [data]
  (->> data
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defonce example->vector (data->vector example))
(defonce input->vector (data->vector input))

(defn max-pos
  "Return max pos in collection"
  [ps]
  (reduce max ps))

(defn min-pos
  "Return min number in collection"
  [ps]
  (reduce min ps))

(max-pos (data->vector example)) ;; => 16
(min-pos (data->vector example)) ;; => 0
(max-pos (data->vector input)) ;; => 1954
(min-pos (data->vector input)) ;; => 0

(defn inclusive-range
  [a b]
  (if (<= a b) (range a (inc b)) (range a (dec b) -1)))

(defn distance
  [x1 x2]
  (Math/sqrt (Math/pow (- x2 x1) 2)))

(defn distance2
  [x1 x2]
  (let [length (distance x1 x2)]
    (->> (inclusive-range 1 length)
         (reduce +))))

(defn calculate1
  "Return vector of []'s equal to length of p0pn"
  [ps]
  (let [x->y (fn [x2] (map #(distance % x2) ps))]
    (->>
     (inclusive-range (min-pos ps) (max-pos ps))
     (map x->y)
     (map (partial reduce +))
     (reduce (partial min)))))

(defn calculate2
  "Return vector of []'s equal to length of p0pn"
  [ps]
  (let [x->y (fn [x2] (map #(distance2 % x2) ps))]
    (->>
     (inclusive-range (min-pos ps) (max-pos ps))
     (map x->y)
     (map (partial reduce +))
     (reduce (partial min)))))

(calculate1 example->vector) ;; => 37.0
(calculate1 input->vector) ;; => 356992.0

;; Part 2
(calculate2 example->vector) ;; => 168
(calculate2 input->vector) ;; => 101268111




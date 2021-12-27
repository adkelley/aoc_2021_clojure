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

(defn max-num
  "Return max number in collection"
  [coll]
  (reduce max coll))

(defn min-num
  "Return min number in collection"
  [coll]
  (reduce min coll))

(max-num (data->vector example)) ;; => 16
(min-num (data->vector example)) ;; => 0
(max-num (data->vector input)) ;; => 1954
(min-num (data->vector input)) ;; => 0

(defn initial-vec
  "Return vector of [] equal to length of p0 to pn"
  [coll]
  (let [length (inc (- (max-num coll) (min-num coll)))]
    (vec (take length (repeat [])))))

(initial-vec example->vector)

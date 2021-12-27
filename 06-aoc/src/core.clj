(ns aoc.core
  (:require
   [clojure.java.io :as io]))

(def example "3,4,3,1,2")

(def input (slurp (io/resource "input.txt")))

(defn data->vector
  ""
  [data]
  (->> data
       (re-seq #"\d")
       (mapv #(Integer/parseInt %))))

(data->vector example) ;; => [3 4 3 1 2]
(data->vector input)

(defn initial-timers
  "Returns inital timers of fish"
  [fish]
  (let [state (vec (repeat 9 0))]
    (reduce #(update %1 %2 inc) state fish)))

(initial-timers (data->vector example)) ;; => [0 1 1 2 1 0 0 0 0]


(defn next-day
  "Return state of fish after one-day"
  [fish]
  (let [v8 (get fish 0)
        v6 #(assoc % 6 (+ v8 (get % 6)))]
    (v6 (assoc (vec (drop 1 fish)) 8 v8))))


(next-day (next-day (next-day (next-day [0 1 1 2 1 0 0 0 0]))))
;; => [1 1 2 1 0 0 0 0 0]
;; => [1 2 1 0 0 0 1 0 1]
;; => [2 1 0 0 0 1 1 1 1]
;; => [1 0 0 0 1 1 3 1 2]


(defn calculate
  "Return number of latern fish created after days"
  [days fish] 
  (reduce + (last (take (inc days) (iterate next-day fish)))))

(calculate 80 (initial-timers (data->vector example))) ;; => 5934
(calculate 80 (initial-timers (data->vector input))) ;; => 360610

;; Part 2
(calculate 256 (initial-timers (data->vector example))) ;; => 26984457539
(calculate 256 (initial-timers (data->vector input))) ;; => 1631629590423

(ns aoc.core)
(require '[clojure.string :refer [split]])

;; Advent of Code 2021
;; Day 1: Sonar Sweep
;; See https://adventofcode.com/2021/day/1 for problem description

(def example "199
200
208
210
200
207
240
269
260
263")

(def input (slurp "resources/input.txt"))

(defn string->depths
  "Returns a vector of integers from a string of integers separated by line breaks"
  [s]
  (->> (split s #"\n")
       (map #(Integer/parseInt %))))

(def tuples-example (partition 2 1 (string->depths example)))


(defn measure-tuples
  "Returns the number of times in xs that a depth measurement increases by
   evaluting whether x2 is greater than x1 in the tuple (x1, x2)"
  [xs]
  (reduce (fn [n [x1 x2]]
            (if (> x2 x1) (inc n) n))
          0 xs))

(def tries-example (partition 3 1 (string->depths example)))

(defn measure-tries
  "Returns the number of times in xs that the sum of a trie increases by
   evaluting whether B is greater than A in tries (A, B, C, D, ...)"
  ([xs] (measure-tries xs 0))
  ([xs res]
   (if (empty? xs)
     res
     (let [sum-a (reduce + (first xs))
           sum-b (reduce + (second xs))
           res_ (if (> sum-b sum-a) (inc res) res)]
       (recur (rest xs) res_)))))


(comment
  (measure-tuples tuples-example) ;; => 7
  (measure-tries tries-example) ;; => 5
  (def tuple-input (partition 2 1 (string->depths input)))
  (measure-tuples tuple-input) ;; => 1529
  (def tries-input (partition 3 1 (string->depths input)))
  (measure-tries tries-input) ;; => 1567
  ,)

(ns aoc.day01
  (:require
   [clojure.string :as string]))

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

(def input (slurp "resources/day01.txt"))

:input "200\n208\n..."
:output [200 208 ...]

(defn split
  [re s]
  (string/split s re))

(defn parse-input
  "Returns a vector of integers from a string of integers separated by line breaks"
  [s]
  (->>
   s
   (split #"\n")
   (map #(Integer/parseInt %))))

(defn part1
  "Returns how many measurements are larger than the previous measurement from a
  two measurement sliding window"
  [xs]
  (->>
   xs
   parse-input
   (partition 2 1)
   (filter (fn [[left right]] (> right left)))
   count))

(part1 input) ;; => 1529

(defn part2
  "Consider sums of a three-measurement sliding window. Returns how many sums are larger than the previous sum"
  [xs]
  (->>
   xs
   parse-input
   (partition 3 1)
   (map (fn [[x y z]] (+ x y z)))
   (partition 2 1)
   (filter (fn [[left right]] (> right left)))
   count))

(part2 input) ;; => 1567

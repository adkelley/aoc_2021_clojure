(ns aoc.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

;; Advent of Code 2021
;; Day 2: Dive
;; See https://adventofcode.com/2021/day/2 for problem description

(def example "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn split
  ""
  [re s]
  (string/split s re))

:input "down 5"
:output {:down 5}

(defn parse-line
  ""
  [line]
  (let [[            _  k     v]
        (re-matches #"(\w+) (\d+)" line)]
    [(keyword k) (Integer/parseInt v)]))

:input "down 5\nup 3\n"
:output [{:down 5} {:up 3}]

(defn read-positions
  ""
  [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (->>
     rdr
     line-seq
     (mapv parse-line))))


(defn part1
  "What do you get if you multiply your final horizontal position by your final depth? "
  [ms]
  (let [f (fn [[position depth] [k v]]
            (case k
              :forward [(+ position v) depth]
              :up      [position      (- depth v)]
              :down    [position      (+ depth v)]
              :default [position depth]))]
       (reduce f [0 0] ms)))

(defn part2
  "What do you get if you multiply your final horizontal position by your final depth?"
  [ms]
  (let [f (fn [[position depth aim] [k v]]
            (case k
              :forward [(+ position v) (+ depth (* aim v)) aim]
              :up      [position depth (- aim v)]
              :down    [position depth (+ aim v)]
              :default [position depth aim]))]
    (reduce f [0 0 0] ms)))

(->>
 example
 (split #"\n")
 (mapv parse-line)
 #_part1
 part2
 (take 2)
 (apply *))

(->>
 "day02.txt"
 read-positions
 #_part1
 part2
 (take 2)
 (apply *))

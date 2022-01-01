(ns aoc.core)
(require '[clojure.string :refer [split]])

;; Advent of Code 2021
;; Day 2: Dive
;; See https://adventofcode.com/2021/day/2 for problem description

(def example "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def input (slurp "resources/day02.txt"))

(defn string->xs
  "Return a vector of strings from a string separated by a line breaks"
  [s]
  (split s #"\n"))

(string->xs example)
;; => ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"]

(defn get-command
  "Return the command (i.e., forward, down, up) proceeded by units"
  [s]
  (->> (split s #" ")
       first))

(get-command "forward 5") ;; => "forward"

(defn get-units
  "Return the units from a string preceded by a command"
  [s]
  (->> (split s #" ")
       second
       Integer/parseInt))

(get-units "forward 5");; => 5

(def position
  "Use an atom to store the position state"
  (atom {:depth 0 :horiz 0 :aim 0}))

(defn update-position-p1
  "Part 1 Instructions"
  [x]
  (let [command (get-command x)
         units   (get-units x)]
   (cond
     (= command "forward") (swap! position update :horiz + units)
     (= command "up") (swap! position update :depth - units)
     (= command "down") (swap! position update :depth + units))))

(defn update-position-p2
  "Part 2 Instructions"
  [x]
  (let [command (get-command x)
        units   (get-units x)]
    (cond
      (= command "forward") (do
                              (swap! position update :horiz + units)
                              (swap! position update :depth + (* (get @position :aim) units)))
      (= command "up") (swap! position update :aim - units)
      (= command "down") (swap! position update :aim + units))))

(defn pilot
  "Keep track of the submarine's position.
   Return depth * horizontal for final result"
  [xs update-fn]
  (if (empty? xs)
    (* (get @position :horiz) (get @position :depth))
    (do
      (update-fn (first xs))
      (recur (rest xs) update-fn))))

(pilot (string->xs example) update-position-p1) ;; => 150
(pilot (string->xs input) update-position-p1) ;; => 1882980

(pilot (string->xs example) update-position-p2) ;; => 900
(pilot (string->xs input) update-position-p2) ;; => 1971232560

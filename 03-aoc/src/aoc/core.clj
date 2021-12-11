(ns aoc.core)
(require '[clojure.string :refer [split]])

;; Advent of Code 2021
;; Day 3: Binary Diagnostic
;; See https://adventofcode.com/2021/day/3 for problem description

(def example
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def input (slurp "resources/input.txt"))

(defn str->bits
  ""
  [s]
  (->> (split s #"\n")
       (mapcat #(split % #""))
       (map #(Integer/parseInt %))))

(def bits-example (str->bits example))
(def bits-input (str->bits input))

(defn ones-common? 
  "Return true when 1 is the most common bit in position 'idx' for binary numbers of
   length 'n' from bits 'xs'"
  [idx n xs]
  (let [fifty-percent (/ (/ (count xs) n) 2)]
    (loop [xs xs ones 0]
     (if (empty? xs)
       (>= ones fifty-percent)
       (recur (drop n xs) (+ ones (nth xs idx)))))))

(ones-common? 0 5 bits-example) ;; => true
(ones-common? 1 5 bits-example) ;; => false
(ones-common? 2 5 bits-example) ;; => true
(ones-common? 3 5 bits-example) ;; => true
(ones-common? 4 5 bits-example) ;; => false


(defn gamma-rate
  "Returns the most common bit in the corresponding position of all numbers in 'xs'"
  [n data]
   (loop [x (dec n)
          rate 0]
     (if (< x 0)
       rate
       (recur (dec x)
              (if (ones-common? x n data)
                (bit-set rate x) 
                rate)))))

(def gamma-example (gamma-rate 5 bits-example))
(def gamma-input (gamma-rate 12 bits-input))


(defn epsilon-rate
  "Return epsilon-rate by flipping bits of gamma-rate"
  [n gamma-rate]
  (loop [x 0
         rate gamma-rate]
    (if (= x n)
      rate
      (recur (inc x)
             (bit-flip rate x)))))


(def epsilon-example (epsilon-rate 5 gamma-example))
(def epsilon-input (epsilon-rate 12 gamma-input))

(def power-consumption-example (* gamma-example epsilon-example))
power-consumption-example ;; => 198

(def power-consumption-input (* gamma-input epsilon-input))
power-consumption-input ;; => 3912944

;; ====================
;; Part 2
;; ====================

(defn filter-values
  "Return binary numbers whose bit at position idx is x (1 or 0)"
  [idx n x xs]
  (loop [xs xs
         nums []]
    (if (empty? xs)
      (apply concat nums)
      (recur (drop n xs)
             (if (= x (nth xs idx))
               (conj nums (take n xs))
               nums)))))

(filter-values 0 5 1 bits-example)
;; => (1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 1 0 1 1 1 1 0 0 1 0 0 0 0 1 1 0 0 1)

(defn bits->num
  "Return a number from a sequence of bits in 'xs'"
  [xs]
  (loop [res 0
         pos (dec (count xs))
         xs  xs]
    (if (empty? xs)
      res 
      (recur (if (= (first xs) 1)
               (bit-set res pos)
               res)
             (dec pos)
             (rest xs)))))

(defn rating
  "Return the filtered version of most common or least common bits down to one number"
  [most least n xs]
  (loop [xs xs
         x  0]
    ;; one number left in xs?
    (if (= (count xs) n)
      (bits->num xs)
      (recur (if (ones-common? x n xs)
               (filter-values x n most xs)
               (filter-values x n least xs))
             (inc x)))))


(def oxygen-generator-rating (rating 1 0 5 bits-example))
(def oxygen-generator-rating_ (rating 1 0 12 bits-input))
(def co2-scrubber-rating (rating 0 1 5 bits-example))
(def co2-scrubber-rating_ (rating 0 1 12 bits-input))

;; life support rating = oxygen-generator-rating * c02-scrubber-rating
(* co2-scrubber-rating oxygen-generator-rating) ;; => 230
(* co2-scrubber-rating_ oxygen-generator-rating_) ;; => 4996233

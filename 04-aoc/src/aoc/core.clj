(ns aoc.core)
(require '[clojure.string :as string :refer [split-lines]])

;; Advent of Code 2021
;; Day 4: Giant Squid
;; See https://adventofcode.com/2021/day/4 for problem description

(def example
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19

  3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7")

(def puzzle-input (split-lines (slurp "resources/input.txt")))
(def puzzle-example (split-lines example))

(defn random-numbers
  "Return random-numbers from puzzle input"
  [s]
  (->> (first s)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))


(def random-numbers-example (random-numbers puzzle-example))
(def random-numbers-input (random-numbers puzzle-input))


(defn bingo-cards
  "Return bingo cards from puzzle input"
  [s]
  (->> (drop 1 s)
       (mapcat #(re-seq #"\d+" %))
       (map #(hash-map :number (Integer/parseInt %) :mark 0))
       vec))

(def bingo-cards-example (bingo-cards puzzle-example))
(def bingo-cards-input (bingo-cards puzzle-input))

(defn mark-cards
  "Return marked when 'x' appears in 'cards' by marking index with 1"
  [x cards]
  (map #(if (= x (:number %)) (assoc % :mark 1) %) cards))

(mark-cards 5 bingo-cards-example)

(defn row->col
  "Transpose card from rows first to column first order"
  [card]
  (let [f #(concat %2 (take-nth 5 %1))]
    (loop [xs nil i 0 card card]
     (if (> i 4)
       xs
       (recur (f card xs) (inc i) (rest card))))))

(defn rows->cols
  "Transpose cards from row first to column first order"
  [cards]
  (loop [cards cards
         xs nil]
    (if (empty? cards)
      xs
      (recur (drop 25 cards) (concat xs (row->col (take 25 cards)))))))

(defn drop-card
  ""
  [n cards]
  (concat (take (* n 25) cards) (drop (* (inc n) 25) cards)))

(defn winner?
  "Return card # when a row within a card is fully marked"
  [cards win]
  (let [ones? (fn [r] (= 5 (reduce + (map #(:mark %) (take 5 r)))))
        win? (fn [r c pos w] (if (or (ones? r) (ones? c)) (assoc w :card (quot pos 25)) w))]
    (loop [rows cards
           cols (rows->cols cards)
           x      0
           win    win]
      (if (or (empty? rows) (:card win))
        win
        (recur (drop 5 rows) (drop 5 cols) (+ x 5) (win? rows cols x win))))))

(defn score
  ""
  [cards win]
  (let [f #(if (= (:mark %1) 0) (+ %2 (:number %1)) %2)]
    (loop [x 25
          card (drop (* 25 (:card win)) cards)
          sum 0]
     (if (= x 0)
       (* sum (:number win))
       (recur (dec x) (rest card) (f (first card) sum))))))

(defn play-bingo-first
  "Return card # and winning #"
  [numbers cards]
    (loop [numbers numbers
           cards cards
           win {:card nil :number nil}]
      (if (:card win)
        (score cards win)
        (recur (rest numbers)
               (mark-cards (first numbers) cards)
               (winner? (mark-cards (first numbers) cards) {:card nil :number (first numbers)})))))


(play-bingo-first random-numbers-example bingo-cards-example) ;; => 4512
(play-bingo-first random-numbers-input bingo-cards-input) ;; => 72770

;;==================================
;; Part 2
;;===================================

(defn play-bingo
  "Return card # and winning #"
  [numbers cards]
  (loop [numbers numbers
         cards cards
         win {:card nil :number nil}]
    (if (:card win)
       (if (> 25 (count cards))
         (recur numbers (drop-card (:card win) cards) win)
         (score cards win))
      (recur (rest numbers)
             (mark-cards (first numbers) cards)
             (winner? (mark-cards (first numbers) cards) {:card nil :number (first numbers)})))))

(drop-card 2 bingo-cards-example)
(play-bingo random-numbers-example bingo-cards-example)

;(def marked-input (vec (repeat (count bingo-cards-input) 0)))
;(play-bingo-last random-numbers-input bingo-cards-input marked-input) ;; => 72770

;bingo-cards-example

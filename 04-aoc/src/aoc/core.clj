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
       (map #(Integer/parseInt %))
       vec))


(def bingo-cards-example (bingo-cards puzzle-example))
(def bingo-cards-input (bingo-cards puzzle-input))

(defn mark-cards
  "Return marked when 'x' appears in 'cards' by marking index with 1"
  [x cards marked]
  (let [f #(if (= %1 x) (assoc %2 %3 1) %2)]
    (loop [i 0
           cards cards
           marked marked]
     (if (empty? cards)
       marked
       (recur (inc i) (rest cards) (f (first cards) marked i))))))

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

(rows->cols (vec (range 50)))
;; => (0 5 10 15 20 1 6 11 16 21 2 7 12 17 22 3 8 13 18 23 4 9 14 19 24 25 30 35 40 45 26 31 36 41 46 27 32 37 42 47 28 33 38 43 48 29 34 39 44 49)


(defn winner?
  "Return card # when a row within a card is fully marked"
  [marked win]
  (let [ones? #(= 5 (reduce + (take 5 %)))
        win? (fn [r c pos w] (if (or (ones? r) (ones? c)) (assoc w :card (quot pos 25)) w))]
    (loop [rows marked
           cols (rows->cols marked)
           x      0
           win    win]
      (if (or (empty? rows) (:card win))
        win
        (recur (drop 5 rows) (drop 5 cols) (+ x 5) (win? rows cols x win))))))

(def test-marked (vec (concat (repeat 40 0) (repeat 5 1) (repeat 5 0))))
(winner? test-marked {:card nil :number 5})

(defn score
  ""
  [marked cards win]
  (let [f #(if (= %1 0) (+ %2 %3) %2)]
    (loop [x 25
          card (drop (* 25 (:card win)) cards)
          mark (drop (* 25 (:card win)) marked)
          sum 0]
     (if (= x 0)
       (* sum (:number win))
       (recur (dec x) (rest card) (rest mark) (f (first mark) sum (first card)))))))

(defn play-bingo-first
  "Return card # and winning #"
  [numbers cards]
    (loop [numbers numbers
           marked (vec (repeat (count cards) 0))
           win {:card nil :number nil}]
      (if (:card win)
        (score marked cards win)
        (recur (rest numbers)
               (mark-cards (first numbers) cards marked)
               (winner? (mark-cards (first numbers) cards marked) {:card nil :number (first numbers)})))))


(play-bingo-first random-numbers-example bingo-cards-example) ;; => 4512
(play-bingo-first random-numbers-input bingo-cards-input) ;; => 72770

;;==================================
;; Part 2
;;===================================
(defn play-bingo-last
  "Return card # and winning #"
  [numbers cards marked]
  (loop [numbers numbers
         marked marked
         win {:card nil :number nil}]
    (if (:card win)
      {:marked marked :numbers numbers :card (:card win) :number (:number win)} 
      (recur (rest numbers)
             (mark-cards (first numbers) cards marked)
             (winner? (mark-cards (first numbers) cards marked) {:card nil :number (first numbers)})))))

(def marked-example (vec (repeat (count bingo-cards-example) 0)))
(play-bingo-last random-numbers-example bingo-cards-example marked-example)

(def marked-input (vec (repeat (count bingo-cards-input) 0)))
(play-bingo-last random-numbers-input bingo-cards-input marked-input) ;; => 72770

random-numbers-example

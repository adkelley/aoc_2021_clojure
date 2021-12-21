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

(defn drop-losers
  ""
  [n cards]
  (->> (drop (* n 25) cards)
       (take 25)))

(drop-losers 2 bingo-cards-example)

(defn drop-winner
  ""
  [n cards]
  (concat (take (* n 25) cards) (drop (* (inc n) 25) cards)))

(drop-winner 1 bingo-cards-example)


(defn winner?
  "Return card # when a row or column within a card is fully marked"
  [cards]
  (loop [rows cards
         cols (rows->cols cards)
         pos  0]
    (let [ ones? (fn [row] (= 5 (reduce + (map #(:mark %) (take 5 row)))))
          win?  (when (or (ones? rows) (ones? cols)) (quot pos 25))]
      (if (or (empty? rows) win?)
        win? 
        (recur (drop 5 rows) (drop 5 cols) (+ pos 5))))))

(winner? bingo-cards-example)

(every? true? [true true true])

(defn score
  "Return score by taking the sum of unmarked numbers on the board and multiply it by number"
  [board number]
  (->> (filter #(= (:mark %) 0) board)
       (map #(:number %))
       (reduce +)
       (* number)))


(defn play-bingo
  "Return card # and winning #"
  [numbers cards]
  (loop [numbers numbers
         draw  (first numbers)
         marked  (mark-cards draw cards)
         board   (winner? cards)]
      (if board
        {:cards marked :draw draw :board board :numbers numbers}
        (recur (rest numbers) (first numbers) (mark-cards (first numbers) marked) (winner? (mark-cards (first numbers) marked))))))

(defn first-winner
  "Return score of first bingo card to win"
  [numbers cards]
  (let [winner (play-bingo numbers cards)]
    (score (drop-losers (:board winner) (:cards winner)) (:draw winner))))

(first-winner random-numbers-example bingo-cards-example) ;; => 4512
(first-winner random-numbers-input bingo-cards-input) ;; => 72770


;;==================================
;; Part 2
;;===================================

(defn last-winner
  ""
  [numbers cards]
  (loop [winner (play-bingo numbers cards)]
    (if (= 25 (count (:cards winner)))
      (score (:cards winner) (:draw winner))
      (recur (play-bingo (:numbers winner) (drop-winner (:board winner) (:cards winner)))))))

(last-winner random-numbers-example bingo-cards-example) ;; => 1924
(last-winner random-numbers-input bingo-cards-input) ;; => 13912

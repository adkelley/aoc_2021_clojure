(ns aoc.core
  (:require
   [clojure.string :as str]))

(def example
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def input (slurp "resources/input.txt"))

(defn data->lines
  "Return collection of integers representing line
  segments x1, y1, x2, y2 . . ."
  [data]
  (->> data
       str/split-lines
       (mapcat #(re-seq #"\d+" %))
       (map #(Integer/parseInt %))
       (partition 4)))

(defonce example->lines (data->lines example))
(defonce input->lines (data->lines input))

;; process lines

(defn first-line
  "Return first 4 integers (line segment) from lines"
  [lines]
  (first lines))

(defn first-x
  "Return x1 from line-segment x1, y1, x2, y2"
  [line]
  (first line))

(defn first-y
  "Return y1 from line-segment x1, y1, x2, y2"
  [line]
  (second line))

(defn last-x
  "Return x2 from line-segment x1, y1, x2, y2"
  [line]
  (nth line 2))

(defn last-y
  "Return y2 from line-segment x1, y1, x2, y2"
  [line]
  (nth line 3))


(defn vertical-line?
  ""
  [line]
  (= (first-x line) (last-x line)))

(vertical-line? '(0 1 0 3)) ;; => true

(defn horizontal-line?
  ""
  [line]
  (= (first-y line) (last-y line)))

(horizontal-line? '(0 1 3 1)) ;; => true

(defn diagonal-line?
  ""
  [line]
  (not (or (vertical-line? line) (horizontal-line? line))))

(diagonal-line? '(0 1 3 3)) ;; => true

(defn non-diagnonal-lines
  "Return non-diagnonal lines from lines"
  [lines]
  (filter #(not (diagonal-line? %)) lines))

(non-diagnonal-lines example->lines)

(defn new-point
  "Return map {:x x, :y y, :id hash :danger 0}"
  [x y]
  (let [id (hash (str x y))]
    {:x x :y y :id id :danger 0}))

(map #(new-point % 0) (range 0 (inc 2)))

(defn make-horiz-points
  ""
  [line]
  (let [y  (first-y line)
        x1 (first-x line)
        x2 (last-x  line)] 
    (if (> x1 x2)
      (map #(new-point % y) (range x2 (inc x1)))
      (map #(new-point % y) (range x1 (inc x2))))))

(make-horiz-points '(0 1 3 1))

(defn make-vertical-points
  ""
  [line]
  (let [x  (first-x line)
        y1 (first-y line)
        y2 (last-y  line)] 
    (if (> y1 y2)
      (map #(new-point x %) (range y2 (inc y1)))
      (map #(new-point x %) (range y1 (inc y2))))))

(make-vertical-points '(0 1 0 3)) 

(defn make-points
  ""
  [line]
  (if (vertical-line? line)
    (make-vertical-points line)
    (make-horiz-points line)))

(make-points '(0 1 0 3))

(defn make-grid
  ""
  [lines]
  (reduce #(concat %1 (make-points %2)) {} lines))

(make-grid (non-diagnonal-lines example->lines))

(defn set-dangerous
  ""
  [grid]
  grid)

  
;; Appendix
(defn max-xs
  ""
  [lines]
  (reduce max (take-nth 2 lines)))

(defn max-ys
  ""
  [lines]
  (reduce max (take-nth 2 (drop 1 lines))))

(defn grid-size
  ""
  [lines]
  (* (max-xs lines) (max-ys lines)))

(grid-size example->lines)
(grid-size input->lines)

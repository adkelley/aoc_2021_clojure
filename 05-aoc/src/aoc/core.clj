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

input->lines

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

(def grid (atom {}))

(defn new-point
  "Return map {:x x, :y y, :id hash :danger 0}"
  [x y]
  (let [id  (read-string (str ":" x y))
        danger (get (deref grid) id 0)]
    (swap! grid assoc id (inc danger))))

(defn make-horiz-points
  ""
  [line]
  (let [y  (first-y line)
        x1 (min (first-x line) (last-x line))
        x2 (inc (max (first-x line) (last-x  line)))] 
    (for [x (range x1 x2)]
       (new-point x y))))

(reset! grid {})
(make-horiz-points '(0 9 5 9))
(deref grid)

(defn make-vertical-points
  ""
  [line] 
  (let [x  (first-x line)
        y1 (min (first-y line) (last-y line))
        y2 (inc (max (first-y line ) (last-y line)))]
    (for [y (range y1 y2)]
      (new-point x y))))

(reset! grid nil)
(make-vertical-points '(0 1 0 9))
(deref grid)

(defn make-points
  ""
  [line]
  (if (vertical-line? line)
    (make-vertical-points line)
    (make-horiz-points line)))


(defn make-grid
  ""
  [lines]
  (reset! grid nil)
  (for [line lines]
     (make-points line)))

(make-grid (non-diagnonal-lines example->lines))
(count (filter #(> (second %) 1) (deref grid))) ;; => 5

(make-grid (non-diagnonal-lines input->lines))
(count (filter #(> (second %) 1) (deref grid)))

(deref grid)
(count (deref grid))

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


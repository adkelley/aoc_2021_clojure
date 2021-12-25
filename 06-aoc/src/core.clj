(ns aoc.core
  (:require
   [clojure.java.io :as io]))

(def example "3,4,3,1,2")

(def input (slurp (io/resource "input.txt")))

(defn data->vector
  ""
  [data]
  (->> data
       (re-seq #"\d")
       (map #(Integer/parseInt %))))

(data->vector example) ;; => (3 4 3 1 2)
(data->vector input)

(defn new-fish
  "Return # of new-fish to spawn"
  [lanterns]
  (count
   (for [lantern lanterns
         :when (= 0 lantern)]
     lantern)))

(defn reset-lanterns
  ""
  [lanterns]
  (let [reset-lantern #(if (= 0 %) 6 %)]
    (map reset-lantern lanterns)))

(defn spawn-lanterns
  ""
  [spawns lanterns]
  (loop [i spawns
         lanterns lanterns]
    (if (= 0 i)
      lanterns
      (recur (dec i) (cons 6 (cons 8 lanterns))))))

;; [Int] -> [Int]
(defn after-one-day
  "Return state of lantern fish after one-day" 
  [lanterns]
  (let [p (partial not= 0)
        spawns (count (filter (partial = 0) lanterns))
        ]
    (->> (filter p lanterns) 
         (map dec)
         (spawn-lanterns spawns)
         )))

(after-one-day '(6 3 2 0 1))

;; Int -> [Int] -> Int
(defn calculate
  "Return number of latern fish created after days"
  [days lanterns] 
  (loop [days days
         lanterns lanterns]
    (if (= 0 days)
      lanterns
      (recur (dec days) (after-one-day lanterns)))))

(count (calculate 80 (data->vector example))) ;; => 5934

(count (calculate 80 (data->vector input))) ;; => 360610

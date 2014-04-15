(ns sudoku.sudoku
  (:require [sudoku.puzzle-getter :refer [gen-puzzle empty-map coords]]))

(defn return-column [x puzzle]
  "given an x value, returns a set of coords in that column"
  (set (filter
         #(= (first %) x)
         (keys puzzle))))

(defn return-row [x puzzle]
  "given a y value, returns a set of coords in that row"
  (set (filter
         #(= (last %) x)
         (keys puzzle))))

(defn return-square-set [n]
  "given a number, returns a set to which it belongs"
  (let [i (quot n 3)]
    (case i
      0 #{0 1 2}
      1 #{3 4 5}
      2 #{6 7 8})))

(defn return-x-square [coords puzzle]
  (set (filter
         #(contains?
            (return-square-set (first coords))
            (first %))
         (keys puzzle))))

(defn return-y-square [coords puzzle]
  (set (filter
         #(contains?
            (return-square-set (last coords))
            (last %))
         (keys puzzle))))

(defn return-square [coords]
  "intersects x and y sets to find square"
  (clojure.set/intersection
    (set (return-x-square coords (empty-map)))
    (set (return-y-square coords (empty-map)))))

(defn show-friends [coords]
  "returns a set of all squares that need to be checked against for a given coord ('friends')"
  (vec (set (apply concat [(return-column (first coords) (empty-map))
                           (return-row (last coords) (empty-map))
                           (return-square coords)]))))

(defn show-friend-set [co puzzle]
  "returns a set of used values in friends"
  (set (vals (select-keys
               puzzle
               (show-friends co)))))

(defn print-puzzle [puzzle]
  (print (clojure.string/join
           (map println-str
                (partition 9 (vals (sort-by key puzzle)))))))

(defn deterministic-cell-solve [co puzzle]
  (if (= (puzzle co) 0)
           (clojure.set/difference #{0 1 2 3 4 5 6 7 8 9}
                                   (show-friend-set co puzzle))
    (puzzle co)))


(defn deterministic-solve [puzzle]
  (reduce (fn [acc car]
            (if (and (= (val car) 0) (= (count (deterministic-cell-solve (key car) puzzle)) 1))
              (assoc acc (key car) (first (deterministic-cell-solve (key car) puzzle)))
              (assoc acc (key car) (val car))))
          {}
          puzzle))

(defn solve-all [puzzle]
 (cond
   (= puzzle (deterministic-solve puzzle)) puzzle
   :else (recur (deterministic-solve puzzle))))



(defn possibles [puzzle]
  (reduce (fn [acc cell]
            (if (= 0 (val cell))
              (assoc acc (key cell) (deterministic-cell-solve (key cell) puzzle))
              (assoc acc (key cell) (val cell))))
          {}
          puzzle))



(defn guess [puzzle]
  (let [poss (first (filter #(not (= (type 6) (type(val %)))) (possibles puzzle)))]
    (if (not= nil (first(last poss)))
      (assoc puzzle (first poss) (first (last poss)))))
      puzzle)


(defn solved? [puzzle]
  (= (count (filter set? (map #(deterministic-cell-solve (key %) puzzle) puzzle))) 0))

(solved? puzzle)
(solved? (solve-all puzzle))

(defn complete-solve [puzzle]
  (let [board (solve-all puzzle)]
    (if (solved? board)
      board
      (if (contains? (vec (vals (possibles board))) #{})
        (throw (Exception. "Dead end"))
        (try
          (complete-solve (guess board)))))))


; demo stuff

(def puzzle (gen-puzzle))
(print-puzzle puzzle)
(deterministic-cell-solve [0 4] puzzle)

(print-puzzle (possibles (solve-all puzzle)))

(do 
(print-puzzle (solve-all puzzle))
  (println)
(print-puzzle (guess (solve-all puzzle))))

(print-puzzle (complete-solve puzzle))

(print-puzzle (complete-solve puzzle))


(print-puzzle(possibles (solve-all puzzle)))

((possibles puzzle) [0 0])

(show-friend-set [0 0] puzzle)

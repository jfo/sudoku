(ns sudoku.sudoku
  (:require [sudoku.puzzle-getter :refer [gen-puzzle empty-map coords]]))


(defn return-row [x]
  "given an x value, returns a set of coords in that column"
  (set (filter
         #(= (first %) x)
         (coords))))

(defn return-column [x]
  "given a y value, returns a set of coords in that row"
  (set (filter
         #(= (last %) x)
         (coords))))

(defn return-square-set [n]
  "given a number, returns a set to which it belongs"
  (let [i (quot n 3)]
    (case i
      0 #{0 1 2}
      1 #{3 4 5}
      2 #{6 7 8})))

(defn return-x-square [co]
  (set (filter
         #(contains?
            (return-square-set (first co))
            (first %))
         (coords))))

(defn return-y-square [co]
  (set (filter
         #(contains?
            (return-square-set (last co))
            (last %))
         (coords))))

(defn return-square [coords]
  "intersects x and y sets to find square"
  (clojure.set/intersection
    (return-x-square coords)
    (return-y-square coords)))

(defn show-friends [coords]
  "returns a set of all cells that need to be checked against for a given coord ('friends')"
  (vec (set (apply concat [(return-column (last coords))
                           (return-row (first coords))
                           (return-square coords)]))))


(defn show-friend-set [co puzzle]
  "#cell_poss - returns a set of used values in friends"
  (set (vals (select-keys
               puzzle
               (show-friends co)))))

(defn cell-poss [co puzzle]
  (clojure.set/difference
    (clojure.set/union
      (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                              (show-friend-set co puzzle))
      #{(puzzle co)})
    #{0}))

(defn possibles [puzzle]
  (reduce (fn [acc cell]
              (assoc acc (key cell) (cell-poss (key cell) puzzle)))
          {}
          puzzle))

(defn print-puzzle [puzzle]
  (print (clojure.string/join
           (map println-str
                (partition 9 (vals (sort-by key puzzle)))))))

(defn dead-puzzle? [puzzle]
  (false? (some (set (vals (possibles puzzle))) #{})))


(defn deterministic-solve [puzzle]
  (reduce (fn [acc car]
            (if (and (= (val car) 0) (= (count (cell-poss (key car) puzzle)) 1))
              (assoc acc (key car) (first (cell-poss (key car) puzzle)))
              (assoc acc (key car) (val car))))
          {}
          puzzle))

(defn solve-all [puzzle]
  (if (dead-puzzle? puzzle)
    (throw (Exception. "Dead end"))
   (cond
     (= puzzle (deterministic-solve puzzle)) puzzle
     :else (recur (deterministic-solve puzzle)))))

; =======================================================================

(defn possible-guesses [puzzle]
  (reduce
    (fn [acc el]
      (if (> (count (val el)) 1)
        (assoc acc (key el) (val el))
        acc))
    {}
    (possibles puzzle)))

(defn all-possible-moves [puzzle]
  (mapcat (fn [pair]
            (let [[coords moves] pair]
              (for [move moves]
                [coords move])))
          (possible-guesses puzzle)))

(defn dead-puzzle? [puzzle]
  (if (some #{} (set (vals (possibles puzzle))))
    true
    false))

(defn make-move [puzzle move]
  (print-puzzle puzzle)
  (read-line)
  (if (dead-puzzle? puzzle)
    (throw (Exception. "Dead end"))
    (assoc puzzle (first move) (last move))))

(defn solved? [puzzle]
    (every? (complement zero?) (vals puzzle)))

(declare solve)

(defn try-moves [puzzle moves]
  (let [next-puzzle (make-move puzzle (first moves))]
    (try
      (solve next-puzzle)
      (catch Exception e
        (try-moves puzzle (rest moves))))))

(defn solve [puzzle]
  (let [new-puzzle (solve-all puzzle)]
    (if (solved? new-puzzle)
          new-puzzle
          (try-moves new-puzzle (all-possible-moves new-puzzle)))))

(defn -main []
  (let [puzzle (gen-puzzle)]
    (print-puzzle puzzle)
    (print-puzzle (solve-all puzzle))
    (println "ok")
    (println (read-line))
    (read-line)
    (print-puzzle (solve puzzle))))

(ns sudoku.sudoku
  (:require [sudoku.puzzle-getter :refer [gen-puzzle empty-map coords]]))

(defn return-row [x]
  "given an x value, returns a set of coords in that row"
  (set (filter
         #(= (first %) x)
         (coords))))

(defn return-column [x]
  "given a y value, returns a set of coords in that column"
  (set (filter
         #(= (last %) x)
         (coords))))

(return-row 4)
(return-row 4)

(defn return-square-set [n]
  "given a number, returns a set to which it belongs"
  (let [i (quot n 3)]
    (case i
      0 #{0 1 2}
      1 #{3 4 5}
      2 #{6 7 8})))

(return-row  8)
(return-column 5)

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
  "returns a set of all cells that need to be checked against for a given coord ('friends')"
  (remove
    #(= coords %)
    (vec (set (apply concat [(return-column (first coords) (empty-map))
                           (return-row (last coords) (empty-map))
                           (return-square coords)])))))

(defn show-friend-set [co puzzle]
  "returns a set of used values in friends"
  (set (vals (select-keys
               puzzle
               (show-friends co)))))

(defn print-puzzle [puzzle]
  (print (clojure.string/join
           (map println-str
                (partition 9 (vals (sort-by key puzzle)))))))

(defn cell-poss [co puzzle]
  (if (= (puzzle co) 0)
           (clojure.set/difference #{0 1 2 3 4 5 6 7 8 9}
                                   (show-friend-set co puzzle))
    (puzzle co)))

(defn all-cell-poss [co puzzle]
   (clojure.set/difference #{0 1 2 3 4 5 6 7 8 9}
                           (show-friend-set co puzzle)
                           #{(puzzle co)}))


(defn all-possible-moves [puzzle]
  (mapcat (fn [pair]
            (let [[coords moves] pair]
              (for [move moves]
                [coords move])))
          (possibles puzzle)))

(all-possible-moves puzzle)
(sort-by #(count (val %))(possibles (solve-all puzzle)))

; ==============================================

(defn dead-cell? [puzzle co]
   (contains?
     (clojure.set/difference
       (show-friend-set co puzzle)
       #{0})
     (puzzle co)))

(defn dead-puzzle? [puzzle]
  (if (contains? (set (all-possible-moves puzzle)) #{})
    true
    (not (every? false? (map #(dead-cell? puzzle %) (keys puzzle))))))

(defn deterministic-solve [puzzle]
  (reduce (fn [acc car]
            (if (and (= (val car) 0) (= (count (cell-poss (key car) puzzle)) 1))
              (assoc acc (key car) (first (cell-poss (key car) puzzle)))
              (assoc acc (key car) (val car))))
          {}
          puzzle))

(defn solve-all [puzzle]
 (if (dead-puzzle? puzzle)
   (throw (Exception. "Noooo!"))
   (cond
     (= puzzle (deterministic-solve puzzle)) puzzle
     :else (recur (deterministic-solve puzzle)))))

; ==============================================

(defn make-move [puzzle move]
  (let [new-puzzle (assoc puzzle (first move) (last move))]
    (if (dead-puzzle? new-puzzle)
      (throw (Exception. "Noooo!"))
      new-puzzle)))

(defn try-moves [puzzle moves]
    (let [next-puzzle (make-move puzzle (first moves))]
          (try
            (solve next-puzzle)
            (catch Exception e
              (try-moves puzzle (rest moves))))))

(defn solve [puzzle]
  (let [new-puzzle (solve-all puzzle)]
    (if (not (contains? (set (vals new-puzzle)) 0))
          new-puzzle
          (try-moves new-puzzle (all-possible-moves new-puzzle)))))





(def puzzle (gen-puzzle))
(print-puzzle puzzle)

(all-possible-moves puzzle)
(print-puzzle (solve-all puzzle))
; (print-puzzle (solve puzzle))

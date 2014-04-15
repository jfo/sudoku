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
  "returns a set of all cells that need to be checked against for a given coord ('friends')"
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
              (dissoc acc (key cell))))
          {}
          puzzle))

; =======================================================================


(defn all-possible-moves [puzzle]
  (mapcat (fn [pair]
            (let [[coords moves] pair]
              (for [move moves]
                [coords move])))
          (possibles puzzle)))

(defn make-move [puzzle move]
  (assoc puzzle (first move) (last move)))

(defn solved? [puzzle]
  (every? (complement zero?) (vals puzzle)))


(declare solve)

(defn try-moves [puzzle moves]
    (when (empty? moves)
          (throw (Exception. "no valid moves")))
    (let [next-puzzle (make-move puzzle (first moves))]
          (try
                  (solve next-puzzle)
                  (catch Exception e
                            (try-moves puzzle (rest moves))))))

(defn solve [puzzle]
    (if (solved? puzzle)
          puzzle
          (try-moves puzzle (all-possible-moves puzzle))))

(solve puzzle)



(all-possible-moves puzzle)

(sort (possibles puzzle))
(do
(print-puzzle puzzle)
(println)
(print-puzzle (assoc puzzle (first (guess puzzle))(last (guess puzzle)))))



; everything below this line sucks and doesn't work and will never work.
; =======================================================================

; (defn guess
;   ([puzzle] (guess puzzle {}))
;   ([puzzle tried]
;    (let [g (reduce (fn [acc cell]
;                      (assoc acc
;                             (key cell)
;                             (clojure.set/difference
;                               (val cell)
;                               (tried (key cell)))))
;                    {}
;                    (possibles puzzle))]
;      (let [guess (sort-by #(count (val %)) g)]
;      [(first (first guess)) (first (last (first guess)))]   ))))
; (defn rec-solve
;   ([puzzle] (rec-solve puzzle {}))
;   ([puzzle tried]
;    (let [board (solve-all puzzle)]
;      (if (not (contains? (set (vals board)) 0))
;        board
;        (if (or (contains? (set (vals (possibles board))) #{}) (contains? (set (vals board)) nil))
;          (throw (Exception. "Ughh"))
;          (try
;            (rec-solve
;              (assoc board
;                     (first (guess board tried))
;                     (last (guess board tried)))
;              (assoc tried
;                     (first (guess board tried))
;                     (clojure.set/union
;                       #{(last (guess board tried))}
;                        ((possibles board) (first (guess board tried))))))))))))

; demo stuff

(def puzzle (gen-puzzle))
(do
(print-puzzle puzzle)
  (println)
(print-puzzle (solve-all puzzle))
  (println)
(print-puzzle (rec-solve puzzle))
  )

; (possibles (solve-all puzzle))
(guess puzzle {[3 2] #{3}})


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
         #(some
           #{(first %)}
            (return-square-set (first coords)))
         (keys puzzle))))


(defn return-y-square [coords puzzle]
  (set (filter
         #(some
            #{(last %)}
            (return-square-set (last coords)))
         (keys puzzle))))

; (return-column 0 puzzle)
; (return-row 0 puzzle)
; (return-square-set 0)
; (return-square-set 2)

; (return-x-square [0 0] puzzle)
; (return-y-square [0 0] puzzle)
; (return-square [0 0])
; (return-square [0 8])

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
           (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                   (show-friend-set co puzzle))
    (puzzle co)))

(defn deterministic-cell-solve [co puzzle]
  (if (= (puzzle co) 0)
           (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                   (show-friend-set co puzzle))
    (puzzle co)))

(defn possibles [puzzle]
  (reduce (fn [acc cell]
            (if (= 0 (val cell))
              (assoc acc (key cell) (deterministic-cell-solve (key cell) puzzle))
              (dissoc acc (key cell))))
          {}
          puzzle))

(defn all-possible-moves [puzzle]
  (mapcat (fn [pair]
            (let [[coords moves] pair]
              (for [move moves]
                [coords move])))
          (possibles puzzle)))


(defn dead-cell? [puzzle co]
   (contains?
     (clojure.set/difference
       (show-friend-set co puzzle)
       #{0})
     (puzzle co)))

(defn dead-puzzle? [puzzle]
  (empty? (all-possible-moves puzzle)))

; ==============================================

(defn make-move [puzzle move]
  (assoc puzzle (first move) (last move)))

(defn won? [puzzle]
  (not (some #{0} (vals puzzle))))

(defn depth-solve [puzzle]
  (if (won? puzzle)
    [puzzle]
    (if (dead-puzzle? puzzle)
      []
      (let [all-solns (map (fn [move]
                             (let [new-puzzle (make-move puzzle move)]
                               (depth-solve new-puzzle)))
                           (all-possible-moves puzzle))]
        (apply concat all-solns)))))


(defn graph-solve [puzzles]
  (if (empty? puzzles)
    nil
    (let [p (first puzzles)]
      (if (won? p)
        p
        (graph-solve (concat
                       (map
                         #(make-move p %)
                         (all-possible-moves p)) 
                       (rest puzzles)))))))


(graph-solve puzzle)

(print-puzzle (solve-all puzzle))
; (dead-puzzle? puzzle)
; (won? puzzle)
; (won? (make-move (solve-all puzzle) [[6 0] 6]))
; (print-puzzle (make-move (solve-all puzzle) [[6 0] 6]))
; (depth-solve (solve-all puzzle))
; (take 1 (depth-solve puzzle))
; (all-possible-moves (solve-all puzzle))

; (def puzzle (gen-puzzle))
; (print-puzzle puzzle)
; (all-possible-moves puzzle)
; (print-puzzle (solve-all puzzle))
; (print-puzzle (solve puzzle))

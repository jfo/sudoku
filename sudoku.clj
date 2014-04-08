(defn coords []
  "returns all possible coordinates in order"
  (for [x (range 9) y (range 9)]
    (vector x y)))

(defn empty-map []
  "empty sudoku board for testing naive solution"
  (into {}
        (for [x (coords)]
          {x 0})))

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

(defn show-friends [coords puzzle]
  "returns a set of all squares that need to be checked against for a given coord ('friends')"
  (vec (set (apply concat [(return-column (first coords) (empty-map))
                           (return-row (last coords) (empty-map))
                           (return-square coords)]))))

(show-friend-set [0 0] (assoc tester [0 0] 8))
(show-friends [0 0] tester)

(defn show-friend-set [co puzzle]
  "returns a set of used values in friends"
  (set (vals (select-keys
               puzzle
               (show-friends co puzzle)))))

(defn find-winner [co puzzle]
  "iteratively discovers first available number against friend set for a given coord and puzzle"
  (loop [friend-set (show-friend-set co puzzle)
        n 1]
      (if (not (contains? friend-set n))
        (assoc puzzle co n)
        (recur friend-set (+ n 1)))))


(defn solver [puzzle]
  "recursively finds a winner for each cell. No checking against max val or tree traversal yet"
    (let [co (some #(if (= 0 (puzzle %)) % ) (coords))]
      (if (= nil co)
        puzzle
        (solver (find-winner co puzzle)))))

(solver tester)
(vals (sort-by key (solver tester)))


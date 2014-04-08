(defn coords []
  (for [x (range 9) y (range 9)] (vector x y)))
(defn empty-map [] (into {} (for [x (coords)] {x 0})))

(def tester (empty-map))

(defn return-column [x puzzle]
  (set (filter #(= (first %) x) (keys puzzle))))
(defn return-row [x puzzle]
  (set (filter #(= (last %) x) (keys puzzle))))


(defn return-square-set [n]
  (let [i (quot n 3)]
    (case i
      0 #{0 1 2}
      1 #{3 4 5}
      2 #{6 7 8})))

(defn return-x-square [coords puzzle]
  (set (filter #(contains? (return-square-set (first coords)) (first %)) (keys puzzle))))
(defn return-y-square [coords puzzle]
  (set (filter #(contains? (return-square-set (last coords)) (last %)) (keys puzzle))))
(defn return-square [coords]
  (clojure.set/intersection
    (set (return-x-square coords (empty-map)))
    (set (return-y-square coords (empty-map)))))

(defn show-friends [coords puzzle]
  (vec (set
  (apply concat
  [(return-column (first coords) (empty-map))
  (return-row (last coords) (empty-map))
  (return-square coords)]))))

(show-friend-set [0 0] (assoc tester [0 0] 8))
(show-friends [0 0] tester)

(defn show-friend-set [co puzzle]
  (set (vals (select-keys puzzle (show-friends co puzzle)))))

(show-friend-set [0 0], (assoc (empty-map) [0,0] 1))

(defn find-winner [co puzzle]
  (loop [friend-set (show-friend-set co puzzle)
        n 1]
      (if (not (contains? friend-set n))
        (assoc puzzle co n)
        (recur friend-set (+ n 1)))))

(defn printer [puzzle]
  (vals (sort-by key puzzle)))
(printer tester)

(defn solver [puzzle]
    (let [co (some #(if (= 0 (puzzle %)) % ) (coords))]
      (if (= nil co)
        puzzle
        (solver (find-winner co puzzle)))))

(last (coords))

(printer (solver tester))
(tester [0 0])
(pprint (solver2 tester))

(vals tester)
(find-winner [1 0] (assoc (empty-map) [0, 0] 0) )


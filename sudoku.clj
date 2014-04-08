(defn coords []
  (for [x (range 9) y (range 9)] (vector x y)))
(defn empty-map [] (into {} (for [x (coords)] {x nil})))

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

(show-friends [0 7] (empty-map))


(return-column 0 (empty-map))
(return-row 0 (empty-map))
(return-square [0 8])



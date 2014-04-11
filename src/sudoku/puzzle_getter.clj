(ns sudoku.puzzle-getter
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(def ^:dynamic *base-url* "http://www.free-sudoku.com/sudoku.php")

(defn get-filled []
  (html/select (fetch-url *base-url*) [:div.pred2]))

(defn get-puzzle []
  (reduce (fn [out car] (assoc out
                               (read-string (get-in car [:attrs :id]))
                               (read-string (first (:content car)))))
          {}
          (get-filled)))


(defn coords []
  "returns all possible coordinates in order"
  (for [x (range 9) y (range 9)]
    (vector x y)))

(defn coord [i]
  [(quot (dec i) 9) (mod (dec i) 9)])
(map #(coord %) (range 1 82))

(defn empty-map []
  "empty sudoku board for testing naive solution"
  (into {}
        (for [x (coords)]
          {x 0})))

(def sample (get-puzzle))

(identity sample)

(defn new-puzzle [puzzle]
  (loop [puzzle puzzle
         i 1]
    (if (= i 82)
      puzzle
      (if (= (puzzle i) nil)
        (recur (assoc puzzle i 0) (inc i))
        (recur puzzle (inc i))))))



(new-puzzle sample)


; this one!
(defn gen-puzzle []
  (into {} (map (fn [[key val]] [(coord key) val]) (new-puzzle (get-puzzle))))
  )
; this one!


(map identity sample)

(eval tester)
(coords)
(eval sample)



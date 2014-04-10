(ns sudoku.puzzle-getter
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(def ^:dynamic *base-url* "http://www.free-sudoku.com/sudoku.php")

(defn get-filled []
  (html/select (fetch-url *base-url*) [:div.pred2]))

(first (get-filled))

(defn get-puzzle []
  (reduce (fn [out car] (assoc out
                               (read-string (get-in car [:attrs :id]))
                               (read-string (first (:content car)))))
          {}
          (get-filled)))

; (get-puzzle)

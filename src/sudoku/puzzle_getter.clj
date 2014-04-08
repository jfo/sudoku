(ns sudoku.puzzle-getter
  (:require [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as client])
  (:import (java.net URL)
          (java.lang StringBuilder)
          (java.io BufferedReader InputStreamReader)))


(def page (client/get "http://www.free-sudoku.com/sudoku.php"))
(def page2 (client/get "http://www.free-sudoku.com/sudoku.php"))

(require 'clojure.pprint)
(clojure.pprint/pprint (html/html-resource @page))


(def derp (range 1 82))
(map inc (range 81))



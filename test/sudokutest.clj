(ns sudokutest
  (:require [sudoku.sudoku :as s])
  (:use clojure.test))



(deftest derp
  (is (= 1 1)))

(run-tests)

(s/return-column 4)

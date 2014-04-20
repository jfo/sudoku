(ns sudokutest
  (:require [sudoku.sudoku :as s])
  (:use clojure.test))

(defn test [])
(defmacro what [])

(deftest derp
  (is (= 1 1)))

(run-all-tests)
(run-tests)

(s/return-column 4)

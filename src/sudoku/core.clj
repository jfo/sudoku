(ns sudoku.core
  (:use [sudoku.sudoku]))


(def puzzle (gen-puzzle))
(identity puzzle)
(print-puzzle puzzle)


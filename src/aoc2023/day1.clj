(ns aoc2023.day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def input (slurp (io/resource "aoc2023/day1_input.txt")))

(def first-and-last-numbers-re #"^[^\d]*(\d)(?:[^\d]|\d(?=[^\d]*\d))*(\d)?.*$")

(defn parse-line [s]
  (let [[_ a b] (re-matches first-and-last-numbers-re s)]
    (Long/parseUnsignedLong (str a (or b a)))))

(defn answer []
  (transduce (map parse-line) + 0 (string/split-lines input)))

(comment
  (answer))

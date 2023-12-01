(ns aoc2023.day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def input (slurp (io/resource "aoc2023/day1_input.txt")))

(def number-re #"(?:\d|one|two|three|four|five|six|seven|eight|nine)")

(def first-and-last-numbers-re
  (re-pattern
   (string/replace
    (str #"^(?:(?!NUMBER).)*(NUMBER)(?:(?!NUMBER).|(?=NUMBER).(?=(?:(?!NUMBER).)*NUMBER))*(NUMBER)?.*$")
    "NUMBER"
    (str number-re))))

(def numbers
  {"0" 0
   "1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn parse-line [s]
  (let [[_ a b] (re-matches first-and-last-numbers-re s)]
    (Long/parseUnsignedLong (str (numbers a) (numbers (or b a))))))

(defn answer []
  (transduce (map parse-line) + 0 (string/split-lines input)))

(comment
  (answer))

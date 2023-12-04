(ns aoc2023.day4
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [instaparse.core :as instaparse]))

(def input (slurp (io/resource "aoc2023/day4_input.txt")))

(def test-input (slurp (io/resource "aoc2023/day4_test_input.txt")))

(def card-parser
  (instaparse/parser
   "Card = <'Card'> <' '+> Number <':'> Numbers <' | '> Numbers
    Numbers = (<' '*> Number <' '*>)+
    Number = #'\\d+'
    "))

(def card-parser-transformers
  {:Card (fn [n winning played] [(dec n) winning played])
   :Numbers vector
   :Number #(Long/parseUnsignedLong %)})

(defn parse-line [s]
  (instaparse/transform card-parser-transformers (card-parser s)))

(defn winning-numbers [card]
  (let [[_ winning played] card]
    (set/intersection (set winning) (set played))))

(defn card-points [card]
  (let [xs (winning-numbers card)]
    (if-not (zero? (count xs))
      (long (Math/pow 2 (dec (count xs))))
      0)))

(defn answer1 [input]
  (transduce (comp (map parse-line) (map card-points)) + 0 (string/split-lines input)))

(comment
  (answer1 test-input)
  (answer1 input))

(defn answer2 [input]
  (let [original-cards (into [] (comp (map parse-line) (map (fn [card] [card (count (winning-numbers card))]))) (string/split-lines input))]
    (loop [[[[idx] won-count] & cards] (into [] (filter #(not (zero? (second %)))) original-cards)
           processed-cards original-cards]
      (if idx
        (let [following-cards (subvec original-cards (inc idx) (min (+ (inc idx) won-count) (dec (count original-cards))))]
          (recur (into cards (filter #(not (zero? (second %)))) following-cards)
                 (into processed-cards following-cards)))
        (count processed-cards)))))

(comment
  (answer2 test-input)
  (answer2 input))

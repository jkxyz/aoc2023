(ns aoc2023.day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [instaparse.core :as instaparse]))

(def input (slurp (io/resource "aoc2023/day2_input.txt")))

(def test-input (slurp (io/resource "aoc2023/day2_test_input.txt")))

(def total-cube-counts
  {:red 12
   :green 13
   :blue 14})

(def parser
  (instaparse/parser
   "<Game> = <'Game '> Number <': '> ( (Set <'; '>)+ Set | Set )
    Set = ( (CubeCount <', '>)+ CubeCount | CubeCount )
    CubeCount = Number <' '> Color
    Color = 'red' | 'green' | 'blue'
    Number = #'\\d+'
    "))

(def transformers
  {:CubeCount vector
   :Set vector
   :Color keyword
   :Number #(Long/parseUnsignedLong %)})

(defn parse-line [s]
  (instaparse/transform transformers (parser s)))

(defn cube-count-possible? [cube-count]
  (let [[n color] cube-count]
    (<= n (total-cube-counts color))))

(defn set-possible? [set]
  (every? cube-count-possible? set))

(defn game-possible? [game]
  (let [[_ & sets] game]
    (every? set-possible? sets)))

(def possible-game-ids-xform
  (comp (map parse-line)
        (filter game-possible?)
        (map first)))

(defn answer [input]
  (transduce possible-game-ids-xform + 0 (string/split-lines input)))

(comment
  (answer test-input)
  (answer input))

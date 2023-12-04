(ns aoc2023.day3
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def input (slurp (io/resource "aoc2023/day3_input.txt")))

(def test-input (slurp (io/resource "aoc2023/day3_test_input.txt")))

(defn re-match-results [re s]
  (let [matcher (re-matcher re s)]
    ((fn step []
       (when (.find matcher)
         (cons (.toMatchResult matcher) (lazy-seq (step))))))))

(defn adjacent-coords [coord]
  (let [[row col] coord]
    #{[(dec row) (dec col)]
      [(dec row) col]
      [(dec row) (inc col)]
      [row (inc col)]
      [(inc row) (inc col)]
      [(inc row) col]
      [(inc row) (dec col)]
      [row (dec col)]}))

(defn symbol?* [c]
  (and (some? c) (not (#{\. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))))

(defn has-adjacent-symbol? [grid row start-col end-col]
  (let [coords (into #{} (map #(vector row %)) (range start-col (inc end-col)))
        all-adjacent-coords (set/difference (into #{} (mapcat adjacent-coords) coords) coords)]
    (boolean (some (comp symbol?* #(get-in grid [(first %) (second %)])) all-adjacent-coords))))

(defn numbers-with-adjacent-symbols [input]
  (let [lines (string/split-lines input)
        grid (mapv vec lines)]
    (loop [result (transient [])
           row 0]
      (if-let [line (nth lines row nil)]
        (let [match-results (re-match-results #"\d+" line)]
          (recur
           (loop [result result
                  [^java.util.regex.MatchResult match-result & match-results] match-results]
             (if match-result
               (if (has-adjacent-symbol? grid row (.start match-result) (dec (.end match-result)))
                 (recur (conj! result (Long/parseUnsignedLong (.group match-result)))
                        match-results)
                 (recur result match-results))
               result))
           (inc row)))
        (persistent! result)))))

(defn answer [input]
  (reduce + 0 (numbers-with-adjacent-symbols input)))

(comment
  (answer test-input)
  (answer input))

(defn parse-line [x s]
  (let [matches (re-match-results #"(?:(\d+)|([^\.]))" s)]
    (into
     #{}
     (mapcat
      (fn [^java.util.regex.MatchResult m]
        (for [y (range (.start m) (.end m))]
          [[x y]
           [[x (.start m)]
            (or (some-> (.group m 1) (Long/parseUnsignedLong))
                (.group m))]])))
     matches)))

(defn parse-input [input]
  (into {} (comp (map-indexed parse-line) cat) (string/split-lines input)))

(defn distinct-by [f]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [x (f input)]
           (if (contains? @seen x)
             result
             (do (vswap! seen conj x)
                 (rf result input)))))))))

(defn adjacent-numbers [m coord]
  (into
   []
   (comp (keep m)
         (filter (fn [[_ x]] (number? x)))
         (distinct-by first)
         (map second))
   (adjacent-coords coord)))

(defn gear-ratio [m coord]
  (let [xs (adjacent-numbers m coord)]
    (when (= 2 (count xs))
      (apply * xs))))

(defn keep-coords [m f]
  (into #{} (comp (filter (fn [[_ [_ x]]] (f x))) (map first)) m))

(defn answer2 [input]
  (let [m (parse-input input)]
    (transduce (keep #(gear-ratio m %)) + 0 (keep-coords m #(= "*" %)))))

(comment
  (answer2 test-input)
  (answer2 input))

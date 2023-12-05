(ns aoc2023.day5
  (:require [clojure.java.io :as io]
            [instaparse.core :as instaparse]))

(def input (slurp (io/resource "aoc2023/day5_input.txt")))

(def test-input (slurp (io/resource "aoc2023/day5_test_input.txt")))

(def parser
  (instaparse/parser
   "Input =
        Seeds Gap
        SeedToSoil Gap
        SoilToFertilizer Gap
        FertilizerToWater Gap
        WaterToLight Gap
        LightToTemperature Gap
        TemperatureToHumidity Gap
        HumidityToLocation Gap;

    Seeds = <'seeds: '> Numbers

    SeedToSoil = <'seed-to-soil map:\n'> Map
    SoilToFertilizer = <'soil-to-fertilizer map:\n'> Map
    FertilizerToWater = <'fertilizer-to-water map:\n'> Map
    WaterToLight = <'water-to-light map:\n'> Map
    LightToTemperature = <'light-to-temperature map:\n'> Map
    TemperatureToHumidity = <'temperature-to-humidity map:\n'> Map
    HumidityToLocation = <'humidity-to-location map:\n'> Map

    Map = (Numbers <'\n'>)+ Numbers | Numbers

    Numbers = (Number <' '>)+ Number | Number
    Number = #'\\d+'

    <Gap> = <'\n'+>
    "))

(defn transform-map [& ranges]
  (sort-by (fn [[[source-start _] _]] source-start)
           (into
            []
            (map
             (fn [[dest-start source-start len]]
               [[source-start (dec (+ source-start len))] dest-start]))
            ranges)))

(def transformers
  {:Input #(into {} %&)

   :Seeds #(into [::seeds] %&)
   :SeedToSoil #(into [::seed->soil] %&)
   :SoilToFertilizer #(into [::soil->fertilizer] %&)
   :FertilizerToWater #(into [::fertilizer->water] %&)
   :WaterToLight #(into [::water->light] %&)
   :LightToTemperature #(into [::light->temperature] %&)
   :TemperatureToHumidity #(into [::temperature->humidity] %&)
   :HumidityToLocation #(into [::humidity->location] %&)

   :Map transform-map
   :Numbers vector
   :Number #(Long/parseUnsignedLong %)})

(defn parse-input [s]
  (instaparse/transform transformers (parser s)))

(def lookup-order
  [::seed
   ::soil
   ::fertilizer
   ::water
   ::light
   ::temperature
   ::humidity
   ::location])

(def next-type
  (into {} (map-indexed (fn [idx type] [type (nth lookup-order (inc idx) nil)])) lookup-order))

(defn lookup* [m k n]
  (loop [[[[source-start source-end] dest-start] & ranges] (get m k)]
    (if source-start
      (if (<= source-start n source-end)
        (+ dest-start (- n source-start))
        (recur ranges))
      n)))

(defn lookup [m source dest n]
  (case [source dest]
    [::seed ::soil] (lookup* m ::seed->soil n)
    [::soil ::fertilizer] (lookup* m ::soil->fertilizer n)
    [::fertilizer ::water] (lookup* m ::fertilizer->water n)
    [::water ::light] (lookup* m ::water->light n)
    [::light ::temperature] (lookup* m ::light->temperature n)
    [::temperature ::humidity] (lookup* m ::temperature->humidity n)
    [::humidity ::location] (lookup* m ::humidity->location n)

    (let [next-source (next-type source)]
      (recur m next-source dest (lookup m source next-source n)))))

(defn answer1 [input]
  (let [m (parse-input input)]
    (apply min (map #(lookup m ::seed ::location %) (::seeds m)))))

(comment
  (answer1 test-input)
  (answer1 input))

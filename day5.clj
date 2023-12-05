(ns day5
  (:require [clojure.string :as str]))

(def sample
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def data (slurp "day5.txt"))

(defn get-range-value [coll x]
  (if (empty? coll)
    x
    (let [[r & rs]      coll
          [dst src len] r
          v (when (<= src x (+ src len -1)) (+ dst (- x src)))]
      (if (nil? v)
        (recur rs x)
        v))))

(get-range-value [[50 98 2] [52 50 48]] 50)
(map #(get-range-value [[50 98 2] [52 50 48]] %) (range 0 100))

(def almanac {:seed-to-soil (partial get-range-value [[50 98 2] [52 50 48]])})
((almanac :seed-to-soil) 79)

(defn to-number [x]
  (Long/parseUnsignedLong x))

(defn parse-data [s]
  (->> (str/split s #"\n\n")
       (map #(re-find #"([a-z-]+)( map)?:\s*((\d+\s*)+)" %))
       (map (fn [[_ t _ d]] [t (->> (str/split d #"\n")
                                    (map #(map to-number (re-seq #"\d+" %))))]))
       (reduce #(assoc %1 (keyword (%2 0)) (%2 1)) {})))

(defn analyze-almanac [data]
  (let [almanac (parse-data data)
        seeds (first (almanac :seeds))
        seed-to-soil (partial get-range-value (almanac :seed-to-soil))
        soil-to-fertilizer (partial get-range-value (almanac :soil-to-fertilizer))
        fertilizer-to-water (partial get-range-value (almanac :fertilizer-to-water))
        water-to-light (partial get-range-value (almanac :water-to-light))
        light-to-temperature (partial get-range-value (almanac :light-to-temperature))
        temperature-to-humidity (partial get-range-value (almanac :temperature-to-humidity))
        humidity-to-location (partial get-range-value (almanac :humidity-to-location))]
    (->> seeds
         (map seed-to-soil)
         (map soil-to-fertilizer)
         (map fertilizer-to-water)
         (map water-to-light)
         (map light-to-temperature)
         (map temperature-to-humidity)
         (map humidity-to-location)
         (reduce min))))

; Part 1
(analyze-almanac sample)
(analyze-almanac data)

; Part 2
; 

(defn split-range [[a b] x]
  (list  [a (dec x)] [x b]))

(defn in-range? [[a b] x]
  (<= a x b))

(in-range? [74 87] 77)

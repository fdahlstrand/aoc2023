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
(defn cut-range [[a b] [x y]]
  (cond
    (and (> a x) (< b y)) (list [x (dec a)] [a b] [(inc b) y])
    (and (> a x) (<= a y)) (list [x (dec a)] [a y])
    (and (>= b x) (< b y)) (list [x b] [(inc b) y])
    :else (list [x y])))

(cut-range [5 10] [1 4])
(cut-range [5 10] [1 5])

(defn make-range
  ([s l]   [s (dec (+ s l))])
  ([[s l]] [s (dec (+ s l))]))

(defn map-range [f [a b]] [(f a) (f b)])

(defn old-process-category [almanac category seed]
  (let [input  (map (fn [[_ s l]] (make-range s l)) (almanac category))
        mapper (partial map-range (partial get-range-value (almanac category)))]
    (loop [[x & xs] input
           seeds []]
      (if x
        (recur xs (concat seeds (cut-range x seed)))
        (map mapper (distinct seeds))))))

(defn process-category [almanac category seed]
  (let [input  (map (fn [[_ s l]] (make-range s l)) (almanac category))
        mapper (partial map-range (partial get-range-value (almanac category)))]
    (loop [[x & xs] input
           seeds (list seed)]
      (if x
        (recur xs (reduce into (for [s seeds] (cut-range x s))))
        (map mapper (distinct seeds))))))

(def almanac (parse-data data))
(def seeds (map make-range (partition 2 (first (almanac :seeds)))))
(def soils (map (fn [[_ s l]] (make-range s l)) (almanac :seed-to-soil)))
(def fertilizers (map (fn [[_ s l]] (make-range s l)) (almanac :soil-to-fertilizer)))
(def water (map (fn [[_ s l]] (make-range s l)) (almanac :water-to-light)))
(def light (map (fn [[_ s l]] (make-range s l)) (almanac :light-to-temperature)))
(def soil-map (partial map-range (partial get-range-value (almanac :seed-to-soil))))

(def seed (first seeds))

(def process-seed (partial process-category almanac :seed-to-soil))
(def process-soil (partial process-category almanac :soil-to-fertilizer))
(def process-fertilizer (partial process-category almanac :fertilizer-to-water))
(def process-water (partial process-category almanac :water-to-light))
(def process-light (partial process-category almanac :light-to-temperature))
(def process-temperature (partial process-category almanac :temperature-to-humidity))
(def process-humidity (partial process-category almanac :humidity-to-location))

(->> seeds
     (map process-seed)
     (reduce into)
     (map process-soil)
     (reduce into)
     (map process-fertilizer)
     (reduce into)
     (map process-water)
     (reduce into)
     (map process-light)
     (reduce into)
     (map process-temperature)
     (reduce into)
     (map process-humidity)
     (flatten)
     (reduce min))

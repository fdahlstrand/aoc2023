(ns day9
  (:require [clojure.string :as str]))

(def sample
  ["0 3 6 9 12 15"
   "1 3 6 10 15 21"
   "10 13 16 21 30 45"])

(def data (str/split-lines (slurp "day9.txt")))

(defn extrapolate [data]
  (reduce +
          (loop [s      (reverse data)
                 values []]
            (if (every? zero? s)
              (conj values 0)
              (recur (map #(apply - %) (partition 2 1 s)) (conj values (first s)))))))

(defn extrapolate-back [data]
  (reduce (fn [acc r] (- r acc))
          (reverse (loop [s      data
                          values []]
                     (if (every? zero? s)
                       (conj values 0)
                       (recur (map (fn [[a b]] (- b a)) (partition 2 1 s)) (conj values (first s))))))))

(defn parse-string [s]
  (->> s
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

; Part 1
(->> data
     (map parse-string)
     (map extrapolate)
     (reduce +))

; Part 2
(->> data
     (map parse-string)
     (map extrapolate-back)
     (reduce +))

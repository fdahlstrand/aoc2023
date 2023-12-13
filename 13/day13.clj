(ns day13
  (:require [clojure.string :as str]))

(defn load-data
  [path]
  (map #(str/split % #"\n") (str/split (slurp path) #"\n\n")))

(defn equal?
  [pattern [a b]]
  (= (get pattern a) (get pattern b)))

(defn rotate-pattern
  [pattern]
  (apply mapv vector pattern))

(defn find-candidates
  [pattern]
  (filter #(equal? pattern %)
          (for [i (range 0 (dec (count pattern)))]
            [i (inc i)])))

(defn reflection?
  [pattern pair]
  (let [last-ix (dec (count pattern))]
    (loop
     [[a b] pair]
      (if (equal? pattern [a b])
        (if (or (= a 0) (= b last-ix))
          true
          (recur [(dec a) (inc b)]))
        false))))

(defn reflection-score
  [pattern]
  (let [candidates (find-candidates pattern)]
    (map (fn [[a _]] (inc a))
         (remove nil?
                 (for [c candidates]
                   (when (reflection? pattern c) c))))))

(defn score
  [pattern]
  (let [col-scores (reflection-score pattern)
        row-scores (reflection-score (rotate-pattern pattern))]
    (+ (reduce + (map #(* 100 %) col-scores)) (reduce + row-scores))))

; Part 1
(reduce + (map score (load-data "sample.txt")))
(reduce + (map score (load-data "day13.txt")))

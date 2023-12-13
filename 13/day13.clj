(ns day13
  (:require [clojure.string :as str]))

(defn load-data
  [path]
  (map (fn [x] (mapv #(into [] %) (str/split x #"\n"))) (str/split (slurp path) #"\n\n")))

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
  (let [col-score (or (first (reflection-score pattern)) 0)
        row-score (or (first (reflection-score (rotate-pattern pattern))) 0)]
    (+ (* 100 col-score) row-score)))

; Part 1
(reduce + (map score (load-data "sample.txt")))
(reduce + (map score (load-data "day13.txt")))

; Part 2

(defn toggle
  [pattern row col]
  (let [s (case (get (get pattern row) col)
            \# \.
            \. \#)]
    (assoc pattern row (assoc (get pattern row) col s))))

(defn all-toggled
  [pattern]
  (for [row (range 0 (count pattern))
        col (range 0 (count (get pattern 0)))]
    (toggle pattern row col)))

(defn toggle-score
  [pattern]
  (let [toggled   (all-toggled pattern)
        orig-col  (or (first (reflection-score pattern)) 0)
        orig-row  (or (first (reflection-score (rotate-pattern pattern))) 0)
        col-score (or (first (flatten (remove empty? (map (fn [x] (remove #(= orig-col %) x)) (map #(reflection-score %) toggled))))) 0)
        row-score (or (first (flatten (remove empty? (map (fn [x] (remove #(= orig-row %) x)) (map #(reflection-score %) (map rotate-pattern toggled)))))) 0)]
    (+ (* 100 col-score) row-score)))

(reduce + (map toggle-score (load-data "sample.txt")))
(reduce + (map toggle-score (load-data "day13.txt")))

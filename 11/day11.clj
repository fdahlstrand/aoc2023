(ns day11
  (:require [clojure.string :as str]))

(defn parse-image [input]
  (str/split input #"\n"))

(defn image-height [image]
  (count image))

(defn image-width [image]
  (count (get image 0)))

(defn get-image [row col image]
  (get (get image row) col))

(defn find-galaxies [image]
  (remove nil? (for [row (range 0 (image-height image))
                     col (range 0 (image-width  image))]
                 (when (= \# (get-image row col image)) [row col]))))

(defn find-empty [image]
  (remove nil? (map-indexed (fn [ix v] (when (every? #(= \. %) v) ix)) image)))

(defn rotate-image [image]
  (apply mapv vector image))

(defn distance [[row-a col-a] [row-b col-b]]
  (+ (Math/abs (- row-a row-b)) (Math/abs (- col-a col-b))))

(defn all-pairs [galaxies]
  (for [x (range 0 (count galaxies))
        y (range (inc x) (count galaxies))]
    [(get galaxies x) (get galaxies y)]))

(defn expand-rows [galaxies row-expansion d]
  (loop [[r & rs] row-expansion
         g        galaxies]
    (if r
      (recur rs (map (fn [[row col]] (if (> row r) [(+ row (dec d)) col] [row col])) g))
      g)))

(defn expand-cols [galaxies col-expansion d]
  (loop [[c & cs] col-expansion
         g        galaxies]
    (if c
      (recur cs (map (fn [[row col]] (if (> col c) [row (+ col (dec d))] [row col])) g))
      g)))

; Part 1
(def sample (slurp "11/11-sample.txt"))
(def data (slurp "11/11-data.txt"))

(defn sum-distances [input d]
  (let [image (parse-image input)
        row-expansion (reverse (find-empty image))
        col-expansion (reverse (find-empty (rotate-image image)))
        galaxies (into [] (expand-cols (expand-rows (find-galaxies image) row-expansion d) col-expansion d))]
    (reduce + (map (fn [[a b]] (distance a b)) (all-pairs galaxies)))))

(sum-distances data 2)

; Part 2
(sum-distances sample 2)
(sum-distances sample 10)
(sum-distances sample 100)
(sum-distances data 1000000)

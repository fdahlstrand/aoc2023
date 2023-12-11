(ns day10take2
  (:require [clojure.string :as str]))

(defn load-sketch
  [path]
  (str/split (slurp path) #"\n"))

(defn sketch-height
  [sketch]
  (count sketch))

(defn sketch-width
  [sketch]
  (count (get sketch 0)))

(defn get-sketch
  ([sketch row col]   (get (get sketch row) col))
  ([sketch [row col]] (get (get sketch row) col)))

(defn find-start
  [sketch]
  (remove
   nil?
   (for [row (range 0 (sketch-height sketch))
         col (range 0 (sketch-width  sketch))]
     (when (= \S (get-sketch sketch row col))
       [row col]))))

(defn north
  ([[row col]] [(dec row) col])
  ([row col]   [(dec row) col]))

(defn south
  ([[row col]] [(inc row) col])
  ([row col]   [(inc row) col]))

(defn east
  ([[row col]] [row (inc col)])
  ([row col]   [row (inc col)]))

(defn west
  ([[row col]] [row (dec col)])
  ([row col]   [row (dec col)]))

(defn from
  [[from-row from-col] [row col]]
  (case [(- row from-row) (- col from-col)]
    [1  0] :from-north
    [-1  0] :from-south
    [0  1] :from-west
    [0 -1] :from-east))

(defn move-start
  [sketch [row col]]
  (cond
    (contains? #{\| \7 \F} (get-sketch sketch (north row col))) (north row col)
    (contains? #{\- \J \7} (get-sketch sketch (east  row col))) (east  row col)
    (contains? #{\| \L \J} (get-sketch sketch (south row col))) (south row col)
    (contains? #{\- \L \F} (get-sketch sketch (west  row col))) (west  row col)))

(defn next-pipe
  [sketch from now]
  (case (get-sketch sketch now)
    \| (case from :from-north (south now) :from-south (north now))
    \- (case from :from-west  (east  now) :from-east  (west  now))
    \L (case from :from-north (east  now) :from-east  (north now))
    \J (case from :from-north (west  now) :from-west  (north now))
    \7 (case from :from-south (west  now) :from-west  (south now))
    \F (case from :from-south (east  now) :from-east  (south now))))

(defn walk-path
  [input]
  (let [start (first (find-start input))]
    (loop [now    (move-start input start)
           prev   start
           visited #{start}
           path    [start]]
      (if (contains? visited now)
        path
        (recur (next-pipe input (from prev now) now) now (conj visited now) (conj path now))))))

; Part 1
(/ (count (walk-path (load-sketch "10/sample.txt"))) 2)
(/ (count (walk-path (load-sketch "10/sample2.txt"))) 2)
(/ (count (walk-path (load-sketch "10/sample3.txt"))) 2)
(/ (count (walk-path (load-sketch "10/day10.txt"))) 2)

(ns day14
  (:require [clojure.string :as str]))

(defn load-note
  [path]
  (into [] (map #(into [] %) (str/split (slurp path) #"\n"))))

(defn north
  [sym]
  (case sym
    \O 0
    \. 1
    \# 2))

(defn rotate-note
  [note]
  (apply mapv vector note))

(defn cube-shaped? [rock] (= \# rock))

(defn round-shaped? [rock] (= \O rock))

(defn move-left
  [coll]
  (sort-by #(case %1 \O 0 \. 1 \# 2) coll))

(defn move-line-left
  [line]
  (->> line
       (partition-by cube-shaped?)
       (map move-left)
       (reduce into [])))

(defn move-stones-north
  [note]
  (->> note
       (rotate-note)
       (map move-line-left)
       (rotate-note)))

(defn count-round-stones
  [line]
  (count (filter round-shaped? line)))

; Part 1
(->> (move-stones-north (load-note "day14.txt"))
     (map count-round-stones)
     (reverse)
     (map-indexed #(* (inc %1) %2))
     (reduce +))

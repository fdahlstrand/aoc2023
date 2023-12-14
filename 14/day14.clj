(ns day14
  (:require [clojure.string :as str]))

(defn load-note
  [path]
  (into [] (map #(into [] %) (str/split (slurp path) #"\n"))))

(defn rotate-note
  [note]
  (apply mapv vector note))

(defn cube-shaped? [rock] (= \# rock))

(defn round-shaped? [rock] (= \O rock))

(defn move-left
  [coll]
  (sort-by #(case %1 \O 0 \. 1 \# 2) coll))

(defn move-right
  [coll]
  (sort-by #(case %1 \O 1 \. 0 \# 2) coll))

(defn move-line-left
  [line]
  (->> line
       (partition-by cube-shaped?)
       (map move-left)
       (reduce into [])))

(defn move-line-right
  [line]
  (->> line
       (partition-by cube-shaped?)
       (map move-right)
       (reduce into [])))

(defn move-stones-north
  [note]
  (->> note
       (rotate-note)
       (map move-line-left)
       (rotate-note)))

(defn move-stones-east
  [note]
  (->> note
       (map move-line-right)))

(defn move-stones-south
  [note]
  (->> note
       (rotate-note)
       (map move-line-right)
       (rotate-note)))

(defn move-stones-west
  [note]
  (->> note
       (map move-line-left)))

(defn count-round-stones
  [line]
  (count (filter round-shaped? line)))

(defn spin-cycle
  [note]
  (->> note
       (move-stones-north)
       (move-stones-west)
       (move-stones-south)
       (move-stones-east)))

; Part 1
(->> (move-stones-north (load-note "day14.txt"))
     (map count-round-stones)
     (reverse)
     (map-indexed #(* (inc %1) %2))
     (reduce +))

; Part 2
(def notes
  (loop [note (load-note "day14.txt")
         notes []]
    (if (some #(= % note) notes)
      {:start  (.indexOf notes note)
       :length (- (count notes) (.indexOf notes note))
       :notes  notes}
      (recur (spin-cycle note) (conj notes note)))))

(let [index (+ (:start notes) (mod (- 1000000000 (:start notes)) (:length notes)))]
  (->> (get (:notes notes) index)
       (map count-round-stones)
       (reverse)
       (map-indexed #(* (inc %1) %2))
       (reduce +)))

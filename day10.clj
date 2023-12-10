(ns day10
  (:require [clojure.string :as str]))

(def sample
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(def data (slurp "day10.txt"))

(defn symbol-to-pipe [s]
  (case s
    \| #{:N :S}
    \- #{:E :W}
    \L #{:N :E}
    \J #{:N :W}
    \7 #{:S :W}
    \F #{:S :E}
    \S #{:start}
    \. nil))

(defn get-map
  ([m row col] (get (get m row) col))
  ([m [row col]] (get (get m row) col)))

(defn move [pn m d]
  (let [s  (get-map m pn)]
    (when (and s (or (contains? s d) (contains? s :start)))
      pn)))

(defn move-north [[row col] m] (move [(dec row) col]       m :S))
(defn move-south [[row col] m] (move [(inc row) col]       m :N))
(defn move-east  [[row col] m] (move [row       (inc col)] m :W))
(defn move-west  [[row col] m] (move [row       (dec col)] m :E))

(defn get-moves [p m]
  (partition 2 (remove nil?
                       (flatten (for [d (get-map m p)]
                                  (case d
                                    :start (list (move-north p m)
                                                 (move-south p m)
                                                 (move-east  p m)
                                                 (move-west  p m))
                                    :N     (move-north p m)
                                    :S     (move-south p m)
                                    :W     (move-west  p m)
                                    :E     (move-east  p m)))))))

(defn find-start [m]
  (remove nil?
          (for [row (range 0 (count m))
                col (range 0 (count (get m row)))]
            (when (contains? (get-map m row col) :start)
              (list row col)))))

(defn parse-map [input]
  (let [symbols (for [[row s] (map-indexed vector (str/split input #"\n"))]
                  [row (map-indexed vector s)])]
    (into [] (for [[_ row] symbols]
               (into [] (for [[_ s] row]
                          (symbol-to-pipe s)))))))

(defn visited? [coll n] (contains? coll n))

(def pipe-map (parse-map data))
(find-start pipe-map)

(defn find-loop [m]
  (loop [[n & ns] (find-start m)
         visited  #{}]
    (if n
      (if (visited? visited n)
        (recur ns visited)
        (recur (concat (get-moves n m) ns) (conj visited n)))
      visited)))

; Part 1
(/ (count (find-loop pipe-map)) 2)

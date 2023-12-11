(ns day8
  (:require [clojure.string :as str]))

(def sample
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def sample2
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def sample3
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(def data (slurp "day8.txt"))

(defn parse-map [map-string]
  (let [[instr, s] (str/split map-string #"\n\n")
        lines      (str/split s #"\n")
        nodes      (->> lines
                        (map #(re-find #"([A-Z0-9]{3}) = \(([A-Z0-9]{3}), ([A-Z0-9]{3})\)" %))
                        (map (fn [[_ k l r]] {k {:left l :right r}}))
                        (into {}))]
    {:instructions instr :nodes nodes}))

(defn follow-map [input start end laps]
  (loop [[i & is] (cycle (input :instructions))
         label    start
         values   []
         cnt      0]
    (let [node (get (input :nodes) label)]
      (if (some #(= label %) end)
        (if (= laps (count values))
          values
          (recur is (case i \L (node :left) \R (node :right)) (conj values [label cnt]) (inc cnt)))
        (recur is (case i \L (node :left) \R (node :right)) values (inc cnt))))))

; Part 1
(follow-map (parse-map sample) "AAA" '("ZZZ") 1)
(follow-map (parse-map sample2) "AAA" '("ZZZ") 1)
(follow-map (parse-map data) "AAA" '("ZZZ") 1)

;Part 2
(defn start-node? [[_ _ x]] (= x \A))
(defn end-node? [[_ _ x]] (= x \Z))

(def start-nodes (filter start-node? (keys ((parse-map data) :nodes))))
(def end-nodes (filter end-node? (keys ((parse-map data) :nodes))))

(defn integer-factorization [p]
  (loop [n p
         f 2
         v []]
    (if (= n 1)
      v
      (if (zero? (mod n f))
        (recur (quot n f) f (conj v f))
        (recur n (inc f) v)))))

(->> (for [s start-nodes] (follow-map (parse-map data) s end-nodes, 1))
     (map (fn [[[_ v]]] v))
     (map integer-factorization)
     (flatten)
     (set)
     (reduce *))

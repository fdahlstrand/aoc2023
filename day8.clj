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

(def data (slurp "day8.txt"))

(defn parse-map [map-string]
  (let [[instr, s] (str/split map-string #"\n\n")
        lines      (str/split s #"\n")
        nodes      (->> lines
                        (map #(re-find #"([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" %))
                        (map (fn [[_ k l r]] {k {:left l :right r}}))
                        (into {}))]
    {:instructions instr :nodes nodes}))

(defn follow-map [input]
  (loop [[i & is] (cycle (input :instructions))
         label    "AAA"
         path     []]
    (let [node (get (input :nodes) label)]
      (if (= label "ZZZ")
        path
        (recur is (case i \L (node :left) \R (node :right)) (conj path label))))))

; Part 1
(follow-map (parse-map sample))
(follow-map (parse-map sample2))
(count (follow-map (parse-map data)))

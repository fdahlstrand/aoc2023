(ns day15
  (:require [clojure.string :as str]))

(defn hash-fn
  [acc x]
  (rem (* 17 (+ acc (int x))) 256))

(defn hash-alg
  [[x & xs] val]
  (if x
    (recur xs (hash-fn val x))
    val))

(defn load-init-seq
  [path]
  (map str/trim (str/split (slurp path) #",")))

; Part 1
(reduce + (map #(hash-alg % 0) (load-init-seq "day15.txt")))

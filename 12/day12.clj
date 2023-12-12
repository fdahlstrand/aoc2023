(ns day12
  (:require [clojure.string :as str]))

(defn cartesian-product
  [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (cons x more))))

(defn unknowns
  [line]
  (count (re-seq #"\?" line)))

(defn get-alternatives
  [cnt]
  (cartesian-product (repeat cnt '(\. \#))))

(defn resolve-springs
  [springs values]
  (loop [[s & ss] springs
         v        values
         result   '()]
    (if s
      (if (= s \?)
        (recur ss (rest v) (concat result [(first v)]))
        (recur ss v (concat result [s])))
      (str/join result))))

(defn signature
  [springs]
  (map count (re-seq #"\#+" springs)))

(defn count-alternatives
  [springs expected]
  (->> (get-alternatives (unknowns springs))
       (map #(resolve-springs springs %))
       (map signature)
       (filter #(= expected %))
       (count)))

(defn parse-line
  [input]
  (let [[springs expected] (str/split input #" ")]
    {:springs springs
     :expected (map (fn [x] (Integer/parseInt x)) (re-seq #"\d+" expected))}))

; Part 1
(->> (str/split (slurp "12/day12.txt") #"\n")
     (map parse-line)
     (map #(count-alternatives (:springs %) (:expected %)))
     (reduce +))

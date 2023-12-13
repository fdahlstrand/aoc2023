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

; Part 2
(defn make-spring
  [n]
  (flatten [\. (repeat n \#) \.]))

(defn is-match?
  ([a b] (when (or (= a b) (= a \?) (nil? a)) true))
  ([line spring pos] (every? true? (map #(is-match? %1 %2) (for [x (range (dec pos) (dec (+ pos (count spring))))] (get line x)) spring))))

(defn find-possibilities
  [line spring]
  (map (fn [[a _]] a)
       (filter (fn [[a b]] (when b a))  (map-indexed (fn [a b] [a b])
                                                     (for [pos (range 0 (count line))]
                                                       (is-match? line spring pos))))))

(find-possibilities "?" (make-spring 1))

(find-possibilities (subs "???.###" 2) (make-spring 1))

(defn search
  [input springs]
  (let [[s & ss] springs
        ix       (- (count input) (+ (count ss) (reduce + ss)))
        spring   (make-spring s)
        possibilities (find-possibilities (subs input 0 ix) spring)]
    {:input (for [p possibilities]
              (subs input (min (count input) (+ p s 1))))
     :ix ix
     :springs ss
     :possibilities possibilities}))

(defn rec-search
  [input springs]
  (let [[s & ss] springs
        ix       (- (count input) (+ (count ss) (reduce + ss)))
        spring   (make-spring s)
        possibilities (find-possibilities (subs input 0 ix) spring)]
    (if s
      (if (empty? possibilities)
        0
        (for [p possibilities]
          (let [next-input (subs input (min (count input) (+ p s 1)))]
            (rec-search next-input ss))))
      1)))
; ------------
(rec-search "???.###" '(1 1 3))
(search "???.###" '(1 1 3))
(search "?.###" '(1 3))
(search "###" '(3))

; ------------
(search ".??..??...?##." '(1 1 3))

(search "..??...?##." '(1 3))
(search ".??...?##." '(1 3))
(search "...?##." '(1 3))
(search "..?##." '(1 3))

(search "...?##." '(3))
(search "..?##." '(3))
(search "...?##." '(3))
(search "..?##." '(3))

(search "" '())
(search "" '())
(search "" '())
(search "" '())

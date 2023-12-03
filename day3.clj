(ns day3
  (:require [clojure.string     :as str]))

(def sample
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(def data (str/split-lines (slurp "day3.txt")))

(defn find-digits [s]
  (let [m (re-matcher #"\d+" s)]
    ((fn next []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :value (Integer/parseInt (. m group))}
               (lazy-seq (next))))))))

(defn contains-symbol? [schematic x y]
  (let [coords (for [dx (range -1 2)
                     dy (range -1 2)]
                 [(+ x dx) (+ y dy)])]
    (some true? (map (fn [x] (cond
                               (nil? x)              false
                               (Character/isDigit x) false
                               (= \. x)              false
                               :else                 true))
                     (map (fn [[x y]] (get (get schematic y) x)) coords)))))

(defn find-numbers [schematic]
  (for [y (range 0 (count schematic))]
    [y (find-digits (get schematic y))]))

(defn part-numbers [schematic]
  (map #(% :value)
       (filter #(true? (% :check))
               (flatten (map (fn [[row numbers]]
                               (map (fn [value]
                                      {:value (value :value)
                                       :check (some true? (for [x (range (value :start) (value :end))]
                                                            (contains-symbol? schematic x row)))})
                                    numbers))
                             (find-numbers schematic))))))

; Part 1
(reduce + 0 (part-numbers sample))
(reduce + 0 (part-numbers data))

; Part 2
(defn find-gears [schematic] (filter :gear (for [y (range 0 (count schematic))
                                                 x (range 0 (count (schematic y)))]
                                             (let [ch (get (schematic y) x)]
                                               {:x x :y y :gear (= ch \*)}))))

(defn touches? [x y gear-x gear-y]
  (some true? (for [dx (range -1 2)
                    dy (range -1 2)]
                (and (= (+ x dx) gear-x) (= (+ y dy) gear-y)))))

(find-gears sample)

(defn gear-parts [schematic gear-x gear-y]
  (filter number?
          (flatten (map (fn [[row numbers]]
                          (map (fn [value]
                                 (when (some true? (for [x (range (value :start) (value :end))]
                                                     (touches? x row gear-x gear-y))) (value :value))) numbers))
                        (find-numbers schematic)))))

(defn gear-ratios [schematic] (filter #(= (count %) 2) (map (fn [gear] (gear-parts schematic (gear :x) (gear :y))) (find-gears schematic))))

(reduce + 0 (map #(apply * %) (gear-ratios sample)))
(reduce + 0 (map #(apply * %) (gear-ratios data)))


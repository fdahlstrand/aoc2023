(ns day1
  (:require [clojure.string :as str]))

(def sample
  ["1abc2"
   "pqr3stu8vwx"
   "a1b2c3d4e5f"
   "treb7uchet"])

(def sample2
  ["two1nine"
   "eightwothree"
   "abcone2threexyz"
   "xtwone3four"
   "4nineeightseven2"
   "zoneight234"
   "7pqrstsixteen"])

(def bad-samples
  ["three3qs7sevenpkjone18twonek"])

(def digits
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(def rex (re-pattern (str/join #"|"  (conj (keys digits) #"\d"))))
(def rex-reverse (re-pattern (str/join #"|"  (conj (map str/reverse (keys digits)) #"\d"))))

(defn text-to-digit [s]
  (let [value (or (digits s) s)] value))

(defn parse-calibration-value [s]
  (let [first (re-find #"\d" s)
        last (re-find #"\d" (str/reverse s))]
    (Integer/parseInt (str/join [first last]))))

(defn parse-calibration-value-text [s]
  (let [first (text-to-digit (re-find rex s))
        last (text-to-digit (str/reverse (re-find rex-reverse (str/reverse s))))]
    (Integer/parseInt (str/join [first last]))))

(defn sum-calibration-value [input]
  (reduce + 0 (map parse-calibration-value input)))

(defn sum-calibration-value-text [input]
  (reduce + 0 (map parse-calibration-value-text input)))

(sum-calibration-value sample)
(sum-calibration-value-text sample2)
(sum-calibration-value-text bad-samples)

; Day 1 - Test data
(def data (str/split (slurp "day1.txt") #"\n"))

; Puzzle 1
(sum-calibration-value data)

; Puzzle 2
(sum-calibration-value-text data)

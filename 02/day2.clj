(ns day2
  (:require [clojure.string     :as str])
  (:require [clojure.core.logic :as l]))

(def sample
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def data (str/split-lines (slurp "day2.txt")))

(defn parse-line [line]
  (let [[_ id data] (re-find #"Game (\d+): (.*)" line)
        xs          (str/split data #"; ")
        ys          (map (fn [s]
                           (let [[_ blue]  (re-find #"(\d+) blue" s)
                                 [_ red]   (re-find #"(\d+) red" s)
                                 [_ green] (re-find #"(\d+) green" s)]
                             {:blue (Integer/parseInt (or blue "0"))
                              :red (Integer/parseInt (or red "0"))
                              :green (Integer/parseInt (or green "0"))})) xs)]
    {:id (Integer/parseInt id) :subsets ys}))

(defn possible? [subset]
  (and (<= (subset :red) 12)
       (<= (subset :green) 13)
       (<= (subset :blue) 14)))

; Day 2 - Part 1
(reduce + 0
        (map (fn [game] (game :id))
             (filter (fn [game] (every? possible? (game :subsets))) (map parse-line data))))

; Day 2 - Part 2
(defn min-color [game color]
  (reduce max 0 (map color (game :subsets))))

(reduce + 0 (map
             (fn [game] (let [r (min-color game :red)
                              g (min-color game :green)
                              b (min-color game :blue)]
                          (* r g b)))
             (map parse-line data)))

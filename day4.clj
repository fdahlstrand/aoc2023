(ns day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def sample
  ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(def data (str/split-lines (slurp "day4.txt")))

(defn parse-line [line]
  (let [[_ card winning mine] (re-find #"Card\s+(\d+): ([^\|]+) \| (.*)" line)]
    {:winning (set (map #(Integer/parseInt %) (re-seq #"\d+" winning)))
     :mine    (set (map #(Integer/parseInt %) (re-seq #"\d+" mine)))
     :card    card}))

(defn pick-winning [card]
  (set/intersection (card :winning) (card :mine)))

(defn count-winnings [cards]
  (->> (map parse-line cards)
       (map pick-winning)
       (map count)
       (map #(bit-shift-left (min % 1) (- % 1)))
       (reduce + 0)))

; Part 1
(count-winnings sample)
(count-winnings data)

; Part 2
(->> (map parse-line sample)
     (map pick-winning)
     (map count))

(defn duplicate-cards [[ix winnings cards]]
  (let [cnt   (count (filter #(= ix %) cards))
        match (winnings (- ix 1))
        cs    (range (+ 1 ix) (+ 1 ix match))]
    [(inc ix) winnings (flatten (concat cards (repeat cnt cs)))]))

(defn count-winnings-2 [cards]
  (let [winnings (->> (map parse-line cards)
                      (map pick-winning)
                      (mapv count))
        length      (+ 1 (count cards))
        [_ _ cs] (last (take length (iterate duplicate-cards [1 winnings (into [] (range 1 length))])))]
    (count cs)))

(count-winnings-2 sample)
(count-winnings-2 data)

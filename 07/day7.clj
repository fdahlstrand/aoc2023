(ns day7
  (:require [clojure.string :as str]))

(def sample
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def data (str/split-lines (slurp "day7.txt")))

(def strength-score
  {0 (* 13 13 13 13)
   1 (* 13 13 13)
   2 (* 13 13)
   3 13
   4 1})

(def type-score (* 13 13 13 13 13))

(defn parse-line [s]
  (let [[_ hand bid] (re-find #"(.{5}) (\d+)" s)]
    {:hand hand :bid (Integer/parseInt bid)}))

(defn get-type [hand]
  (let [group (map count (vals (group-by identity hand)))]
    (* type-score (cond
                    (some #(= % 5) group)                             6    ; five of a kind
                    (some #(= % 4) group)                             5    ; four of a kind
                    (and (some #(= % 3) group) (some #(= % 2) group)) 4    ; full house
                    (some #(= % 3) group)                             3    ; three of a kind
                    (= 2 (count (filter #(= % 2) group)))             2    ; two pairs
                    (some #(= % 2) group)                             1    ; one pair
                    :else                                             0)))) ; high card

(defn get-strength [hand card-rank]
  (reduce + (for [ix (range 0 5)]
              (let [card (get hand ix)]
                (* (strength-score ix) (str/index-of card-rank card))))))

; Part 1
(defn score-hand [entry]
  (let [hand (entry :hand)]
    (assoc entry :value (+ (get-strength hand "23456789TJQKA") (get-type hand)))))

(reduce +
        (map-indexed (fn [rank hand] (* (inc rank) (hand :bid)))
                     (sort-by :value (map score-hand (map parse-line data)))))

; Part 2
(defn improve-hand [hand]
  (if (= hand "JJJJJ")
    "AAAAA"
    (let  [card ((first (sort-by (fn [[_ v]] (- (count v))) (filter (fn [[k _]] (not= k \J)) (group-by identity hand)))) 0)] (str/replace hand \J card))))

(defn score-improved-hand [entry]
  (let [hand (entry :hand)]
    (assoc entry :value (+ (get-strength hand "J23456789TQKA") (get-type (improve-hand hand))))))

(reduce +
        (map-indexed (fn [rank hand] (* (inc rank) (hand :bid)))
                     (sort-by :value (map score-improved-hand (map parse-line data)))))

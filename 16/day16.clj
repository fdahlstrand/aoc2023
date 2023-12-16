(ns day16
  (:require [clojure.string :as str]))

(defn load-layout
  [path]
  (into [] (map #(into [] %) (str/split (slurp path) #"\n"))))

(defn get-element
  ([layout row col] (get (get layout row) col)))

(defn width  [layout] (count (get layout 0)))
(defn height [layout] (count layout))

(defn in-layout?
  [layout [row col _]]
  (and (<= 0 row (dec (height layout)))
       (<= 0 col (dec (width layout)))))

(defn next-beams
  [layout [row col dir]]
  (filter #(in-layout? layout %)
          (case (get-element layout row col)
            \. (case dir
                 :up    [[(dec row) col        :up]]
                 :left  [[row       (dec col)  :left]]
                 :right [[row       (inc col)  :right]]
                 :down  [[(inc row) col        :down]])
            \\ (case dir
                 :up    [[row       (dec col) :left]]
                 :left  [[(dec row) col       :up]]
                 :right [[(inc row) col       :down]]
                 :down  [[row       (inc col) :right]])
            \/ (case dir
                 :up    [[row       (inc col) :right]]
                 :left  [[(inc row) col       :down]]
                 :right [[(dec row) col       :up]]
                 :down  [[row       (dec col) :left]])
            \- (case dir
                 :up    [[row       (dec col) :left]
                         [row       (inc col) :right]]
                 :left  [[row       (dec col) :left]]
                 :right [[row       (inc col) :right]]
                 :down  [[row       (dec col) :left]
                         [row       (inc col) :right]])
            \| (case dir
                 :up    [[(dec row) col       :up]]
                 :left  [[(dec row) col       :up]
                         [(inc row) col       :down]]
                 :right [[(dec row) col       :up]
                         [(inc row) col       :down]]
                 :down  [[(inc row) col       :down]]))))

(defn trace-beams
  [layout]
  (loop [[b & bs] [[0 0 :right]]
         visited  #{}]
    (if b
      (if (contains? visited b)
        (recur bs visited)
        (recur (concat bs (next-beams layout b)) (conj visited b)))
      visited)))

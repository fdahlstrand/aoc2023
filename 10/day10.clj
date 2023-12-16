(ns day10
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn load-sketch
  [path]
  (into [] (map #(into [] %) (str/split (slurp path) #"\n"))))

(defn sketch-height
  [sketch]
  (count sketch))

(defn sketch-width
  [sketch]
  (count (get sketch 0)))

(defn get-sketch
  ([sketch row col]   (get (get sketch row) col))
  ([sketch [row col]] (get (get sketch row) col)))

(defn find-start
  [sketch]
  (remove
   nil?
   (for [row (range 0 (sketch-height sketch))
         col (range 0 (sketch-width  sketch))]
     (when (= \S (get-sketch sketch row col))
       [row col]))))

(defn print-sketch
  [sketch]
  (doall
   (map println
        (for [row (range 0 (sketch-height sketch))]
          (str/join
           (for [col (range 0 (sketch-width sketch))]
             (get-sketch sketch row col)))))))

(defn print-outline
  [sketch {path :path left-outline :left-outline right-outline :right-outline}]
  (doall
   (map println
        (for [row (range 0 (sketch-height sketch))]
          (str/join
           (for [col (range 0 (sketch-width sketch))]
             (cond
               (contains? path [row col])          \#
               (contains? left-outline [row col])  \l
               (contains? right-outline [row col]) \r
               :else                               \.)))))))

(defn north
  ([[row col]] [(dec row) col])
  ([row col]   [(dec row) col]))

(defn north-east
  ([[row col]] [(dec row) (inc col)])
  ([row col]   [(dec row) (inc col)]))

(defn north-west
  ([[row col]] [(dec row) (dec col)])
  ([row col]   [(dec row) (dec col)]))

(defn south
  ([[row col]] [(inc row) col])
  ([row col]   [(inc row) col]))

(defn south-east
  ([[row col]] [(inc row) (inc col)])
  ([row col]   [(inc row) (inc col)]))

(defn south-west
  ([[row col]] [(inc row) (dec col)])
  ([row col]   [(inc row) (dec col)]))

(defn east
  ([[row col]] [row (inc col)])
  ([row col]   [row (inc col)]))

(defn west
  ([[row col]] [row (dec col)])
  ([row col]   [row (dec col)]))

(defn from
  [[from-row from-col] [row col]]
  (case [(- row from-row) (- col from-col)]
    [1  0] :from-north
    [-1  0] :from-south
    [0  1] :from-west
    [0 -1] :from-east))

(defn move-start
  [sketch [row col]]
  (cond
    (contains? #{\| \7 \F} (get-sketch sketch (north row col))) (north row col)
    (contains? #{\- \J \7} (get-sketch sketch (east  row col))) (east  row col)
    (contains? #{\| \L \J} (get-sketch sketch (south row col))) (south row col)
    (contains? #{\- \L \F} (get-sketch sketch (west  row col))) (west  row col)))

(defn fix-start
  [sketch]
  (let [start     (first (find-start sketch))
        north     (get-sketch sketch (north start))
        east      (get-sketch sketch (east  start))
        south     (get-sketch sketch (south start))
        west      (get-sketch sketch (west  start))
        [row col] start
        pipe      (cond
                    (and (contains? #{\| \7 \F} north) (contains? #{\| \L \J} south)) \|
                    (and (contains? #{\- \J \7} east)  (contains? #{\- \L \F} west))  \-
                    (and (contains? #{\| \7 \F} north) (contains? #{\- \J \7} east))  \L
                    (and (contains? #{\| \7 \F} north) (contains? #{\- \L \F} west))  \J
                    (and (contains? #{\- \L \F} west)  (contains? #{\| \L \J} south)) \7
                    (and (contains? #{\- \J \7} east)  (contains? #{\| \L \J} south)) \F)]
    (assoc sketch row (assoc (get sketch row) col pipe))))

(defn next-pipe
  [sketch from now]
  (case (get-sketch sketch now)
    \| (case from :from-north (south now) :from-south (north now))
    \- (case from :from-west  (east  now) :from-east  (west  now))
    \L (case from :from-north (east  now) :from-east  (north now))
    \J (case from :from-north (west  now) :from-west  (north now))
    \7 (case from :from-south (west  now) :from-west  (south now))
    \F (case from :from-south (east  now) :from-east  (south now))))

(defn walk-path
  [input]
  (let [start (first (find-start input))]
    (loop [now    (move-start input start)
           prev   start
           visited #{start}
           path    [start]]
      (if (contains? visited now)
        path
        (recur (next-pipe input (from prev now) now) now (conj visited now) (conj path now))))))

; Part 1
(/ (count (walk-path (load-sketch "sample.txt"))) 2)
(/ (count (walk-path (load-sketch "sample2.txt"))) 2)
(/ (count (walk-path (load-sketch "sample3.txt"))) 2)
(/ (count (walk-path (load-sketch "day10.txt"))) 2)

; Part 2
(defn left-outline-pipe
  [sketch pos next-pos]
  (let [from  (from pos next-pos)]
    (case (get-sketch sketch pos)
      \| (case from
           :from-north #{(east pos)}
           :from-south #{(west pos)})
      \- (case from
           :from-west  #{(north pos)}
           :from-east  #{(south pos)})
      \L (case from
           :from-south #{(south pos) (south-west pos) (west pos)}
           :from-west  #{(north-east pos)})
      \J (case from
           :from-east  #{(south pos) (south-east pos) (east pos)}
           :from-south #{(north-west pos)})
      \7 (case from
           :from-north #{(north pos) (north-east pos) (east pos)}
           :from-east  #{(south-west pos)})
      \F (case from
           :from-west  #{(west pos) (north-west pos) (north pos)}
           :from-north #{(south-east pos)}))))

(defn right-outline-pipe
  [sketch pos next-pos]
  (let [from  (from pos next-pos)]
    (case (get-sketch sketch pos)
      \| (case from
           :from-south #{(east pos)}
           :from-north #{(west pos)})
      \- (case from
           :from-east  #{(north pos)}
           :from-west  #{(south pos)})
      \L (case from
           :from-west  #{(south pos) (south-west pos) (west pos)}
           :from-south #{(north-east pos)})
      \J (case from
           :from-south #{(south pos) (south-east pos) (east pos)}
           :from-east  #{(north-west pos)})
      \7 (case from
           :from-east  #{(north pos) (north-east pos) (east pos)}
           :from-north #{(south-west pos)})
      \F (case from
           :from-north #{(west pos) (north-west pos) (north pos)}
           :from-west  #{(south-east pos)}))))

(defn find-outline
  [input]
  (let [start  (first (find-start input))
        sketch (fix-start input)]
    (loop [now           (move-start sketch start)
           prev          start
           path          #{start}
           right-outline (right-outline-pipe sketch start now)
           left-outline  (left-outline-pipe sketch start now)]
      (let [nxt (next-pipe sketch (from prev now) now)]
        (if (contains? path now)
          {:path          path
           :right-outline (set/union right-outline (right-outline-pipe sketch prev now))
           :left-outline  (set/union left-outline (left-outline-pipe sketch prev now))}
          (recur nxt
                 now
                 (conj path now)
                 (set/union right-outline (right-outline-pipe sketch prev now))
                 (set/union left-outline (left-outline-pipe sketch prev now))))))))

(defn surround
  [[row col]]
  #{[(inc row) col]
    [(dec row)  col]
    [row (inc col)]
    [row (dec col)]})

(defn invalid?
  [sketch [row col]]
  (let [height (sketch-height sketch)
        width  (sketch-width sketch)]
    (or (< row 0) (>= row height) (< col 0) (>= col width))))

(defn flood-fill
  [sketch path outline]
  (loop [[q & qs] outline
         fill     #{}]
    (if q
      (if (or (contains? path q) (contains? fill q) (invalid? sketch q))
        (recur qs fill)
        (recur (set/union qs (surround q)) (conj fill q)))
      fill)))

(def sketch (load-sketch "day10.txt"))
(def wombat (find-outline sketch))

(print-outline
 sketch
 {:path (wombat :path)
  :right-outline (flood-fill sketch (wombat :path) (wombat :right-outline))
  :left-outline (flood-fill sketch (wombat :path) (wombat :left-outline))})

(count (flood-fill sketch (wombat :path) (wombat :right-outline)))

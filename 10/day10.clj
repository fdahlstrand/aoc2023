(ns day10
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def sample
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(def data (slurp "day10.txt"))

(defn symbol-to-pipe [s]
  (case s
    \| #{:N :S}
    \- #{:E :W}
    \L #{:N :E}
    \J #{:N :W}
    \7 #{:S :W}
    \F #{:S :E}
    \S #{:start}
    \. nil))

(defn get-map
  ([m row col] (get (get m row) col))
  ([m [row col]] (get (get m row) col)))

(defn move [pn m d]
  (let [s  (get-map m pn)]
    (when (and s (or (contains? s d) (contains? s :start)))
      pn)))

(defn move-north [[row col] m] (move [(dec row) col]       m :S))
(defn move-south [[row col] m] (move [(inc row) col]       m :N))
(defn move-east  [[row col] m] (move [row       (inc col)] m :W))
(defn move-west  [[row col] m] (move [row       (dec col)] m :E))

(defn get-moves [p m]
  (partition 2 (remove nil?
                       (flatten (for [d (get-map m p)]
                                  (case d
                                    :start (list (move-north p m)
                                                 (move-south p m)
                                                 (move-east  p m)
                                                 (move-west  p m))
                                    :N     (move-north p m)
                                    :S     (move-south p m)
                                    :W     (move-west  p m)
                                    :E     (move-east  p m)))))))

(defn find-start [m]
  (remove nil?
          (for [row (range 0 (count m))
                col (range 0 (count (get m row)))]
            (when (contains? (get-map m row col) :start)
              (list row col)))))

(defn parse-map [input]
  (let [symbols (for [[row s] (map-indexed vector (str/split input #"\n"))]
                  [row (map-indexed vector s)])]
    (into [] (for [[_ row] symbols]
               (into [] (for [[_ s] row]
                          (symbol-to-pipe s)))))))

(defn visited? [coll n] (contains? coll n))

(def pipe-map (parse-map data))
(find-start pipe-map)

(defn find-loop [m]
  (let [start (first (find-start m))
        sec   (first (get-moves start m))]
    (loop [[n & ns] (list sec)
           visited  #{start}
           path [start]]
      (if n
        (if (visited? visited n)
          (recur ns visited path)
          (recur (concat (get-moves n m) ns) (conj visited n) (conj path n)))
        path))))

; Part 1
(/ (count (find-loop (parse-map data))) 2)

; Part 2
(def pipe-loop (find-loop pipe-map))

(defn dir [delta]
  (case delta
    [0  1] :right
    [0 -1] :left
    [1  0] :down
    [-1  0] :up
    :else   :error))

(defn follow-path-left [path]
  (->> (partition 2 1 (conj path (first path)))
       (map (fn [[[rowa cola] [rowb colb]]] {:pos (list rowb colb) :dir (dir (list (- rowa rowb) (- cola colb)))}))
       (map (fn [{[row col] :pos d :dir}]
              (case d
                :up    (list row (dec  col))
                :right (list (dec row) col)
                :down  (list row       (inc col))
                :left  (list (inc row) col))))))

(defn follow-path-right [path]
  (->> (partition 2 1 (conj path (first path)))
       (map (fn [[[rowa cola] [rowb colb]]] {:pos (list rowb colb) :dir (dir (list (- rowa rowb) (- cola colb)))}))
       (map (fn [{[row col] :pos d :dir}]
              (case d
                :up    (list row (inc  col))
                :right (list (inc row) col)
                :down  (list row       (dec col))
                :left  (list (dec row) col))))))

(defn follow-path-ext [path]
  (->> (partition 2 1 (conj path (first path)))
       (map (fn [[[rowa cola] [rowb colb]]] {:pos (list rowb colb) :dir (dir (list (- rowa rowb) (- cola colb)))}))
       (map (fn [{[row col] :pos d :dir}] {(list row col) d}))))

(defn surround [[row col] width height]
  (remove nil? (for [x '((0 -1) (0 1) (-1 0) (1 0))]
                 (let [[dr dc] x
                       prow      (+ row dr)
                       pcol      (+ col dc)
                       p       [prow pcol]]
                   (when (and (>= prow 0) (>= pcol 0) (< prow height) (< pcol width))
                     p)))))

(def part2-sample
  "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def part2-sample2
  ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def part2-sample3
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(defn region [outline path width height]
  (loop [[q & qs] outline
         r        #{}]
    (if q
      (if (or (contains? path q) (contains? r q))
        (recur qs r)
        (recur (doall (concat qs (surround q width height))) (conj r q)))
      r)))

(defn find-region [input]
  (let [m      (parse-map input)
        height (count (str/split input #"\n"))
        width  (count (first (str/split input #"\n")))
        start  (find-start m)
        path   (find-loop m)
        left   (set (follow-path-left path))
        right  (set (follow-path-right path))
        path   (set path)]
    {:width        width
     :height       height
     :left-region  (region left path width height)
     :right-region (region right path width height)
     :path         path
     :left         left
     :right        right
     :start        (set start)
     :all          (set (for [row (range 0 height)
                              col (range 0 width)]
                          [row col]))}))

(def the-map (parse-map part2-sample2))
(def the-height (count (str/split part2-sample2 #"\n")))
(def the-width (count (first (str/split part2-sample2 #"\n"))))
(def the-start (find-start the-map))
(def the-path (set (find-loop the-map)))
(def the-outline (set (follow-path (find-loop the-map))))
(def all-nodes
  (set (for [row (range 0 the-height)
             col (range 0 the-width)]
         (list row col))))

(count (let [region (find-region part2-sample2)]
         (set/difference (:all region) (:path region) (:region region))))

(def the-fill
  (loop [[q & qs] '([0 0])
         out      #{}]
    (if q
      (if (or (contains? the-path q) (contains? out q))
        (recur qs out)
        (recur (concat qs (surround q the-width the-height)) (conj out q)))
      out)))

(get-map the-map 0 0)

(def the-outline-ext (into {} (follow-path-ext (find-loop the-map))))

(defn print-region [region]
  (for [row (range 0 (:height region))]
    (str/join (for [col (range 0 (:width region))]
                (cond
                  (contains? (:left-region region) [row col]) \O
                  (contains? (:right-region region) [row col]) \I
                  (contains? (:start  region) [row col]) \S
                  (contains? (:path   region) [row col]) \X
                  (contains? (:left   region) [row col]) \l
                  (contains? (:right  region) [row col]) \r
                  :else                                  \.)))))

(print-region (find-region data))

(let [r (find-region data)]
  (if (contains? (:left-region r) [0 0])
    (count (:right-region r))
    (count (:left-region r))))

(for [row (range 0 (count the-map))]
  (str/join (for [col (range 0 (count (get the-map 0)))]
              (cond
                (contains? (set the-start) [row col]) \S
                ; (contains? the-path [row col]) \X
                ; (contains? (set/difference all-nodes the-path the-fill) [row col]) \I
                (contains? the-outline-ext [row col]) (case (the-outline-ext [row col])
                                                        :up    \u
                                                        :down  \d
                                                        :right \r
                                                        :left  \l
                                                        :else  \?)
                ; (contains? the-fill [row col]) \O
                (contains? the-outline [row col]) \*
                ; (contains? the-region [row col]) \A
                ; (contains? the-path [row col]) \X
                :else                          \.))))

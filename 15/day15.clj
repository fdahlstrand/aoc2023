(ns day15
  (:require [clojure.string :as str]))

(defn hash-fn
  [acc x]
  (rem (* 17 (+ acc (int x))) 256))

(defn hash-alg
  [[x & xs] val]
  (if x
    (recur xs (hash-fn val x))
    val))

(defn load-init-seq
  [path]
  (map str/trim (str/split (slurp path) #",")))

(defn compile-instr
  [s]
  (let  [[_ label op len] (re-find #"([a-zA-Z]+)(-|=)([1-9])?" s)]
    {:label        label
     :op           (case op "=" :equal "-" :minus)
     :focal-length (when len (Integer/parseInt len))}))

(defn find-slot
  [box label]
  (first (remove nil?
                 (map-indexed (fn [ix [l _]]
                                (when (= l label) ix))
                              box))))

(defn remove-slot
  [box ix]
  (into (subvec box 0 ix) (subvec box (inc ix))))

(defn equal-op
  [box label focal-length]
  (let [slot (find-slot box label)]
    (if slot
      (assoc box slot [label focal-length])
      (conj box [label focal-length]))))

(defn minus-op
  [box label]
  (let [slot (find-slot box label)]
    (if slot
      (remove-slot box slot)
      box)))

(defn execute-instr
  [boxes instr]
  (let [box (hash-alg (:label instr) 0)]
    (case (:op instr)
      :equal (assoc boxes box (equal-op (get boxes box) (:label instr) (:focal-length instr)))
      :minus (assoc boxes box (minus-op (get boxes box) (:label instr))))))

(defn execute-init-seq
  [[i & is] boxes]
  (if i
    (recur is (execute-instr boxes (compile-instr i)))
    boxes))

(defn box-power
  [box slots]
  (->> slots
       (map-indexed (fn [slot [_ f]] (* (inc box) (inc slot) f)))
       (reduce +)))

(defn create-empty-boxes
  []
  (into [] (repeat 256 [])))

; Part 1
(reduce + (map #(hash-alg % 0) (load-init-seq "day15.txt")))

; Part 2
(reduce + (map-indexed box-power
                       (-> (load-init-seq "day15.txt")
                           (execute-init-seq (create-empty-boxes)))))

(defn records [t d]
  (let [t1 (int (Math/floor (+ (/ (- t (Math/sqrt (+ (* t t) (* -4 d)))) 2) 1)))
        t2 (int (Math/ceil (- (/ (+ t (Math/sqrt (+ (* t t) (* -4 d)))) 2) 1)))]
    [t1 t2]))

(def sample
  [[7   9]
   [15  40]
   [30 200]])

(def data
  [[41  214]
   [96 1789]
   [88 1127]
   [94 1055]])

; Part 1
(->> data
     (map #(apply records %))
     (map (fn [[a b]] (inc (- b a))))
     (reduce *))

; Part 2
(let [[a b] (records 41968894 214178911271055)]
  (inc (- b  a)))

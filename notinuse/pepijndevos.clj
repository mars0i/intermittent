(ns incant.pepijndevos 
  (:use (incanter core charts)))
 
; naive, O(n+m)
(defn take-rand1 [n coll] (take n (shuffle coll)))
 
; lazy, O(n!@#$%m^&)
(defn take-rand2 [n coll]
  (let [coll (vec coll)]
    (take n (distinct (repeatedly #(rand-nth coll))))))
 
; reduce, reorder, subvec, O(m)
(defn take-rand3 [nr coll]
  (let [len (count coll)
        ; why doesn't rand-int take a start?
        rand-int (fn [lo hi] (+ lo (rand-int (- hi lo))))]
    (subvec (->> (range nr)
                 (reduce #(conj %1 [%2 (rand-int %2 len)]) [])
                 (reduce
                   (fn swap [a [i b]]
                      (assoc a b (get a i) i (get a b)))
                   coll))
            0 nr)))
 
; amalloy, O(m)
(defn take-rand4 [num coll]
  (first
   (nth
    (iterate (fn [[ret candidates]]
               (let [idx (rand-int (count candidates))]
                 [(conj ret (candidates idx))
                  (subvec (assoc candidates idx (candidates 0))
                          1)]))
             [[]
              coll])
    num)))
 
; amalloy, o(mg)
(defn take-rand5 [nr coll]
  (take nr
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (rand-int c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
           (transient coll))))
 
(defn t [f]
  (let [start (. System (nanoTime))]
    (f)
     (- (. System (nanoTime)) start)))
 
(defn plot-len [f n]
  (let [coll (vec (range n))]
    (t #(doall (f 1000 coll)))))
 
(defn plot-take [f n]
  (let [coll (vec (range 100000))]
    (t #(doall (f n coll)))))
 
(def x (range 1000 100000 1000))
 
(defn points [f g]
  (System/gc)
  (map (partial f g) x))
 
(defn draw-line [plot plotfn randfn name]
  (add-points plot x (points plotfn randfn) :series-label name))
 
(defn do-it []
  (-> (scatter-plot [] [] :legend true)
      (draw-line plot-len  take-rand1 "shuffle")
      (draw-line plot-len  take-rand2 "filtered")
      (draw-line plot-len  take-rand3 "reduce")
      (draw-line plot-len  take-rand4 "iterate")
      (draw-line plot-len  take-rand5 "transient")
      (save "len.png")) ; http://i.imgur.com/SJ0pH.png
 
  (-> (scatter-plot [] [] :legend true)
      (draw-line plot-take take-rand1 "shuffle")
      (draw-line plot-take take-rand2 "filtered")
      (draw-line plot-take take-rand3 "reduce")
      (draw-line plot-take take-rand4 "iterate")
      (draw-line plot-take take-rand5 "transient")
      (save "take.png"))) ; http://i.imgur.com/ExyD7.png

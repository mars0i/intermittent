

;(defn sample-with-repl
;  [rng num-samples coll]
;  (let [size (count coll)]
;    (for [_ (range num-samples)]
;      (nth coll (.nextInt rng size))))) ; don't use Clojure's rand-nth; it's based on java.util.Random.

;(defn remove-nth
;  "Returns a lazy sequence like coll, but with the nth item removed."
;  [coll n]
;  (concat (take n coll) (drop (inc n) coll)))

;; This is a little bit simpler than the version in popco2's utils/random.clj, 
;; which is based on Incanter code.  I make no claims that this is efficient.
;; (Perhaps remove-nth above should be defined using subvec instead, maybe using transients.)
;(defn sample-without-repl
;  [rng num-samples coll]
;  (letfn [(sample-it [samples-remaining remaining acc]
;            (if (<= samples-remaining 0)
;              acc
;              (let [idx (.nextInt rng (count remaining))]
;                (recur (dec samples-remaining)
;                       (remove-nth remaining idx)
;                       (conj acc (nth remaining idx))))))]
;    (sample-it num-samples coll [])))

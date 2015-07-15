
            ^MersenneTwisterFast rng (.random sim)
            pop-bag (sim.util.Bag. ^Collection population)] ; THIS is where most of the order-of-magnitude hit comes
        (.shuffle pop-bag rng)                                         ; CONSIDER using a Clojure shuffle routine, or use Bags more of the time to avoid conversion
        ;(copy-relig! this sim (vec pop-bag))))
        ;(copy-relig! this sim (shuffle population)))) ; JUST AS slow as the Bag version. Clojure shuffle throws coll into an ArrayList and then calls java.util.Collections/shuffle on it.

;; Can I avoid repeated accesses to the same field, caching them?  Does it matter?
;; 
;; TODO: BUG: When there are many with the max success, this is biased toward the end of the sequence.
;; Why? This is OK when the seq is in random order, but it shouldn't happen.
;; Ah, this is why: Because the random choice of the winner does pairwise comparisons.
;; So it randomly chooses one from the first pair, but then the winner of that randomly competes
;; against the next, and so on.  Therefore prob that the first indiv at max will win is 0.5^n,
;; where n is the number of indivs with the max value.  (It's also bad that I'm running params that
;; tend to force success values to 0.0 or 1.0.)
;;
;; Note that the analogous procedure in LKJPlus.nlogo, find-best, uses NetLogo's ask, which means
;; that subaks are always compared in random order.  In effect they're shuffled before the comparison
;; process starts.
;;
(defn choose-most-successful
  "Given a collection of Indiv's, returns the one with the greatest success, or
  nil if the collection is empty."
  ^Indiv [^MersenneTwisterFast rng models]
  (letfn [(compare-success 
            ([] nil) ; what reduce does if collection is empty
            ([^Indiv i1 ^Indiv i2]
             (let [^double success1 (getSuccess i1)
                   ^double success2 (getSuccess i2)]
               (cond (== success1 success2) (if (< ^double (.nextDouble rng) 0.5) ; a rare case, but we don't want ties' results to be path dependent.
                                              i1
                                              i2)
                     (> success1 success2) i1
                     :else i2))))]
    (reduce compare-success models)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALTERNATIVE SHUFFLING AND SAMPLING FUNCTIONS (not all in use)

;; WARNING Returns a set, at present.  There's no reason to think that when
;; converted to a vector, the result is in random order,
;; but I need for it to be random if choose-most-successful requires that.
;;
;; It's much faster to remove the originating Indiv from samples here,
;; rather than removing it from the collection to be sampled, at least
;; for reasonably large populations.  Makes this function more complicated, 
;; though.  Hopefully the bugs are out now.
(defn sample-wout-repl-or-me
  "Special sample without replacement function:
  Returns num-samples samples from coll without replacement, excluding 
  items identical? to me.  Returns a vector or a set; if you want something
  more specific, it's the caller's responsibility.  Should be fastest when
  coll has fast index access (e.g. it's a vector) and when the elements hash
  by identity (e.g. when defined by deftype rather than defrecord).  NOTE:
  results are not in random order."
  [^MersenneTwisterFast rng ^long num-samples me coll]
  (let [size (count coll)
        size-wout-me (dec size)] ; how many elements, excluding me?
    (when (>= num-samples size) (throw (Exception. "num-samples is larger than coll size w/out 'me'")))
    (loop [sample-set #{}]   ; don't return coll as is when num-samples = size; need to allow it to be shuffled anyway (maybe not so efficient ...)
      (if (== (count sample-set) num-samples)
        sample-set
        (let [sample (nth coll (.nextInt rng size))]
          (if (identical? me sample) ; if it's the one we don't want,
            (recur sample-set)       ; lose it
            (recur (conj sample-set sample))))))))

;;;;;;
;;; The take-randX versions both sample without replacement a collection and shuffle the output.
;;; take-randnth seems a little slower; the other three are similar in speed.

;; From Rosetta Code page for "Knuth Shuffle" (= Fisher-Yates): http://rosettacode.org/wiki/Knuth_shuffle#Clojure
;; Note vect *must* be a vector.  
;; Very slow on vec of len 200--like take-rand[3-5], it's an order of magnitude slower than bag-shuffle.
;; I added use of rng, fixed typo (missing paren at end of anonymous fn in original), and added the use
;; of transients, which produces only a slight improvement.
(defn rosetta-shuffle [rng vect]
  (persistent!
    (reduce (fn [v i] (let [r (.nextInt rng i)]
                        (assoc! v i (v r) r (v i))))  ; i.e. starting from vector v, replace element at i with (v r), the element at r, and vice versa.
            (transient vect)
            (range (dec (count vect)) 1 -1)))) ; counts down from one less than length to 2, inclusive.


;; Marshall Abrams
;; Faster than Clojure's shuffle, at least for pops of size 200 or so
(defn bag-shuffle
  [rng coll]
  (let [bag (sim.util.Bag. coll)]
    (.shuffle bag rng)
    bag))

;; On large sample, almost as fast as take-rand1, but uses MersenneTwisterFast.
;; Note that the 'take' gives a 2X drop in perf over mere bag-shuffle, so 
;; if you know you want the whole thing, just shuffle it.
;; On small sample, order of mag slower than take-rand[3-5].
;; Marshall Abrams
(defn bag-sample
  [rng n coll]
  (subvec (vec (bag-shuffle rng coll)) 0 n))
  ;(take n (bag-shuffle rng coll))) ; seems a little slower than subvec sometimes

;; DON'T USE THIS: It uses Java's rng
;; Very slow for small samples, but incredibly fast for large samples.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
; naive, O(n+m)
(defn take-rand1 [_ n coll] (take n (shuffle coll)))
 
;; THIS ONE IS ACTUALLY A BIT FASTER THAN THE ... COOLER ONES BELOW FOR SMALL SAMPLES,
;; BUT IT'S DOG SLOW FOR LARGE SAMPLES--2 orders of magnitude less than take-rand1.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
; lazy, O(n!@#$%m^&)
(defn take-rand2 [rng n coll]
  (let [coll (vec coll)
        len (count coll)]
    (take n (distinct (repeatedly #(nth coll (.nextInt rng len)))))))

;; TODO CHECK WHETHER THESE ARE FISHER-YATES SHUFFLES:

;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
;; reduce, reorder, subvec, O(m)
(defn take-rand3 [rng nr coll]
  (let [len (count coll)
        ; why doesn't rand-int take a start?
        rand-int (fn [lo hi] (+ lo (.nextInt rng (- hi lo))))]
    (subvec (->> (range nr)
                 (reduce #(conj %1 [%2 (rand-int %2 len)]) []) ; for ea num in range, assoc it with a rand idx between num and end
                 (reduce
                   (fn swap [a [i b]]
                      (assoc a b (get a i) i (get a b)))
                   coll))
            0 nr)))


;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; from https://gist.github.com/pepijndevos/805747, by amalloy (?), with small mod by Marshall
; amalloy, O(m)
(defn take-rand4 [rng nr coll]
  (first
   (nth
    (iterate (fn [[ret candidates]]
               (let [idx (.nextInt rng (count candidates))]
                 [(conj ret (candidates idx))
                  (subvec (assoc candidates idx (candidates 0))
                          1)]))
             [[]
              coll])
    nr)))

;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; A LITTLE FASTER than the preceding two on large samples.
;; from https://gist.github.com/pepijndevos/805747, by amalloy (?), with small mod by Marshall
; amalloy, o(mg)
(defn take-rand5 [rng nr coll]
  (take nr
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (.nextInt rng c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
           (transient coll))))

;; One order of magnitude slower than take-rand1 on large samples.
;; By amalloy, from https://gist.github.com/amalloy/805546, with small mod by Marshall:
(defn take-randnth [rng nr coll]
  (take nr
        (rest
         (map first
              (iterate (fn [[ret items]]
                         (let [idx (.nextInt rng (count items))]
                           [(items idx)
                            (subvec (assoc items idx (items 0))
                                    1)]))
                       [nil
                        (vec coll)])))))

;; By amalloy, from https://gist.github.com/amalloy/805546:
;; See also http://stackoverflow.com/questions/3944556/what-if-anything-is-wrong-with-this-shuffling-algorithm-and-how-can-i-know
(defn lazy-shuffle [coll]
  ((fn shuffle [^clojure.lang.ITransientVector coll]
     (lazy-seq
      (let [c (count coll)]
        (when-not (zero? c)
          (let [n (rand-int c)]
            (cons (get coll n)
                  (shuffle (.pop (assoc! coll n (get coll (dec c)))))))))))
   (transient (vec coll))))

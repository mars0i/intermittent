
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

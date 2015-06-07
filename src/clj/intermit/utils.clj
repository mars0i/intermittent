;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

(ns intermit.utils)

;; very slow
(defn but-nth-vec
  "Given a vector v, returns a vector that's the same except that
  element idx of the original vector is absent."
  [v ^long idx]
  (into (subvec v 0 idx)
        (subvec v (inc idx))))

;; even slower
(defn but-nth
 "Returns a lazy sequence like coll, but with the nth item removed."
  [coll n]
  (concat (take n coll) (drop (inc n) coll)))

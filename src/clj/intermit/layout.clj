(ns intermit.layout
  (:require [clojure.math.numeric-tower :as m]))

(declare near-factors middle-factors middle-factors-helper)

;; Ported from my CultranDejanet.nlogo or one of my NetworkExperiment NetLogo models
(defn near-factors
  "Finds the pair of factors of n whose product is n and whose
  values are closest in value to each other; if there are no
  such factors, computes them for n+1."
  [n]
  (cond (== n 1) [1 1]
        (== n 2) [2 1]
        :else 
        (vec (map int
                  (let [[factor1 factor2 :as facs] (middle-factors n)]
                    (if (== factor1 1)         ; we failed with n
                      (middle-factors (inc n)) ; try again with n+1
                      facs)))))) ; return the answer

(defn middle-factors
  "Finds the pair of factors of n whose product is n and whose
  values are closest in value to each other."
  [n]
  (middle-factors-helper n (m/floor (m/sqrt n))))

(defn middle-factors-helper
  "Helper function for middle-factors."
  [n fac]
  (cond (== fac 0) [0 0]
        (== fac 1) [1 n]
        (== (mod n fac) 0.0) [fac (/ n fac)]
        :else (middle-factors-helper n (dec fac))))

;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; Much of the code in this file is ported from my CultranDejanet.nlogo
;; or one of my NetworkExperiment NetLogo models.

(ns intermit.layout
  (:require [clojure.math.numeric-tower :as m]
            [intermit.utils :as u]
            [intermit.Sim :as s])
  (:import [sim.field.continuous Continuous2D]
           [sim.util Double2D]))

(declare near-factors middle-factors middle-factors-helper set-community-locs! lattice-locs)

(defn set-communities-locs!
  "Insert communities into field at locations calculated so that they are spread
  out in a lattice across the field."
  [field communities]
  (doseq [[community x-loc y-loc] (lattice-locs 0.5 
                                                (.getWidth field)
                                                (.getHeight field)
                                                communities)]
    (.setObjectLocation field community (Double2D. x-loc y-loc))))

(defn calc-indiv-locs
  [community space-size]
  (let [members (s/get-members community)
        comm-size (count members)]
    ;; divide 2pi by comm-size ...
    ;; or maybe do a little lattice??
  ))

(defn lattice-locs
  "Calculates x and y coordinates for communities so that they are spread out
  in a lattice across the field, with each object shifted by offset-factor 
  within its position in both dimensions."
  [offset-factor width height communities]
  (let [[num-comms-1 num-comms-2] (near-factors (count communities))
        [num-comms-horiz num-comms-vert] (if (< width height) ; num-comms-1 is always <= num-comms-2
                                           [num-comms-1 num-comms-2]
                                           [num-comms-2 num-comms-1])
        comm-width (/ width num-comms-horiz)
        comm-height (/ height num-comms-vert)]
    (for [i (range num-comms-horiz)
          j (range num-comms-vert)]
      [(nth communities (+ i (* j num-comms-horiz)))
       (* (+ i offset-factor) comm-width)      ; add 0.5 to move to center of region
       (* (+ j offset-factor) comm-height)])))

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

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

(def community-shrink 0.5)
(def community-offset 4.0)
(def community-offset-factor 1.25)

(declare near-factors middle-factors middle-factors-helper set-community-locs! set-indiv-locs! indiv-locs lattice-locs)

;; Might be unused.
(defn set-community-locs!
  "Insert communities into field at locations calculated so that they are spread
  out in a lattice across the field."
  [field communities]
  (let [[_ _ locs] (lattice-locs 0.0 0.0 (.getWidth field) (.getHeight field) communities)] ; TODO offset parameters need adjustment
    (doseq [[community x-loc y-loc] locs]
      (.setObjectLocation field community (Double2D. x-loc y-loc)))))

(defn set-indiv-locs!
  "Insert indivs into field at locations calculated so that they're spread out
  in a lattice of lattices across the field.  (Uses communities merely to organize
  indivs; doesn't set locations of communities.)"
  [field communities]
  (doseq [[indiv x-loc y-loc] (indiv-locs (.getWidth field) (.getHeight field) communities)]
      (.setObjectLocation field indiv (Double2D. x-loc y-loc))))

(defn indiv-locs
  [overall-width overall-height communities]
  (let [[comm-width* comm-height* comm-locs] (lattice-locs overall-width overall-height communities) ; calc 0-offset locs of communities
        comm-width (* community-shrink comm-width*)
        comm-height (* community-shrink comm-height*)]
    (apply concat  ; seq of seqs into seq
           (for [[community comm-x comm-y] comm-locs  ; now we use all of the community locs
                 :let [indivs (s/get-members community)
                       [_ _ indiv-locs] (lattice-locs community-offset
                                                      community-offset-factor
                                                      comm-width
                                                      comm-height
                                                      indivs)]]
             (for [[indiv indiv-x indiv-y] indiv-locs]
               [indiv (+ comm-x indiv-x) (+ comm-y indiv-y)]))))) ; shifts the small region to community's location

(defn lattice-locs
  "Calculates x and y coordinates for communities so that they are spread out
  in a lattice across the field, with each object shifted by offset-factor 
  within its position in both dimensions.  Returns a 2-element sequence, where
  the first element is a pair [community-width community-height], and the second
  element is a sequence of triplets of the form [community, x-coord, y-coor]."
  ([width height things] (lattice-locs 0.0 0.0 width height things))
  ([offset offset-factor width height things]
  (let [[num-a num-b] (near-factors (count things))
        [num-horiz num-vert] (if (< width height) [num-a num-b] [num-b num-a]); num-a is always <= num-b
        thing-width (/ width num-horiz)
        thing-height (/ height num-vert)]
    [thing-width   ; Let caller know size of elements, in case we
     thing-height  ;   want to put things inside them.
     (for [i (range num-horiz)  ; coords to put arrange things in a lattice
           j (range num-vert)
           :let [thing (get things (+ i (* j num-horiz)))] ; When num elts doesn't factor, near-factor returns
           :when thing]                                    ;  factors for count+1, so last elt will be missing.
       [thing
        (+ offset (* (+ i offset-factor) thing-width))
        (+ offset (* (+ j offset-factor) thing-height))])])))

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


(defn set-links!
  "Given a sim.field.network.Network and a collection of indivs, adds
  the indivs to the network, and creates a single edge for each neighbor relationship
  captured in indivs' neighbor fields.  Assumes that there are no duplicates of neighbor
  relationships except that if I am your neighbor, you are my neighbor."
  [network indivs]
  (doseq [indiv indivs]
    (doseq [neighbor (.getNeighbors indiv)]
      (when-not (.getEdge network neighbor indiv) ; if undirected, order doesn't matter
        (.addEdge network indiv neighbor nil))))) ; automatically adds nodes, too.


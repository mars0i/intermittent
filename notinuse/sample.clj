;;; Part of the source code here is derived from the
;;; definition of sample-uniform in the stats.clj in Incanter.
;;; That file contains the following notice:
;;; Copyright (c) David Edgar Liebke, 2009. All rights reserved.  The use
;;; and distribution terms for this software are covered by the Eclipse
;;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.htincanter.at the root of this
;;; distribution.  By using this software in any fashion, you are
;;; agreeing to be bound by the terms of this license.  You must not
;;; remove this notice, or any other, from this software.
;;;
;;; Everything else in this file is copyright 2015 by Marshall Abrams, and
;;; if it's separable from the Incanter source code, is 
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

(ns intermit.sample
  (:require [intermit.utils :as u])
  (:import [ec.util MersenneTwisterFast]))

;; TRY COLT VERSION cern.jet.random.sampling Class RandomSampler
;; https://dst.lbl.gov/ACSSoftware/colt/api/cern/jet/random/sampling/RandomSampler.html

(defn slow-sample-wout-repl
  "Given a random number generator, a number of samples, and a vector, returns
  a vector of num-samples random samples without replacement from vector."
  [^MersenneTwisterFast rng ^long num-samples v]
  (letfn [(sample-it [^long samples-remaining ^clojure.lang.PersistentVector remaining ^clojure.lang.PersistentVector acc]
            (if (<= samples-remaining 0)
              acc
              (let [idx (.nextInt rng (count remaining))]
                (recur (dec samples-remaining)
                       (u/but-nth-vec remaining idx)
                       (conj acc (nth remaining idx))))))]
    (sample-it num-samples v [])))

;; The next several defs are all equally fast on small samples.

;; Incanter-derived version
(defn sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)]
    (cond
      (= num-samples 1) [(nth v (.nextInt rng size))]  ; if only one element needed, don't bother with the "with replacement" algorithm
      ;; Rather than creating subseqs of the original v, we create a seq of indices below,
      ;; and then [in effect] map (partial nth v) through the indices to get the samples that correspond to them.
      (< num-samples size) (mapv #(nth v %) 
                                 (loop [samp-indices [] indices-set #{}]    ; loop to create the set of indices
                                   (if (= (count samp-indices) num-samples) ; until we've collected the right number of indices
                                     samp-indices
                                     (let [i (.nextInt rng size)]           ; get a random index
                                       (if (contains? indices-set i)        ; if we've already seen that index,
                                         (recur samp-indices indices-set)   ;  then try again
                                         (recur (conj samp-indices i) (conj indices-set i))))))) ; otherwise add it to our indices
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

;; Like Incanter version, but assumes that the vector contains
;; objects that hash quickly, so that we can use a set containing them rather than indices.
(defn identity-sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)]
    (cond
      (= num-samples 1) [(nth v (.nextInt rng size))]  ; if only one element needed, don't bother with the "with replacement" algorithm
      (< num-samples size) 
      (loop [samples [] sample-set #{}]      ; loop until we've collected the right number of samples
        (if (= (count samples) num-samples)
          samples
          (let [sample (nth v (.nextInt rng size))]  ; get a single random sample
            (if (contains? sample-set sample)        ; if we've already seen that index,
              (recur samples sample-set)             ; try again
              (recur (conj samples sample) (conj sample-set sample)))))) ; otherwise add it to our samples
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

(defn simple-identity-sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)]
    (cond
      (= num-samples 1) [(nth v (.nextInt rng size))]  ; if only one element needed, don't bother with the "with replacement" algorithm
      (< num-samples size) (loop [sample-set #{}]      ; loop until we've collected the right number of samples
                             (if (= (count sample-set) num-samples)
                               (vec sample-set)
                               (let [sample (nth v (.nextInt rng size))]  ; get a single random sample
                                 (if (contains? sample-set sample)        ; if we've already seen that index,
                                   (recur sample-set)             ; try again
                                   (recur (conj sample-set sample)))))) ; otherwise add it to our samples
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

(defn simpler-identity-sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)]
    (cond
      (= num-samples 1) [(nth v (.nextInt rng size))]  ; if only one element needed, don't bother with the "with replacement" algorithm
      (< num-samples size) (loop [sample-set #{}]      ; loop until we've collected the right number of samples
                             (if (= (count sample-set) num-samples)
                               (vec sample-set)
                               (recur (conj sample-set (nth v (.nextInt rng size)))))) ; if we found a duplicate, let set figure that out
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

(defn transient-identity-sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform.
  Assumes that samples can be compared by identity.  Returns
  a vector or set; if you want something particular, that's
  the caller's responsibility."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)]
    (cond
      (= num-samples 1) [(nth v (.nextInt rng size))]  ; if only one element needed, don't bother with the "with replacement" algorithm
      (= num-samples 2) (let [sample1 (nth v (.nextInt rng size))]
                          (loop [sample2 (nth v (.nextInt rng size))]
                            (if (identical? sample1 sample2)
                              (recur (nth v (.nextInt rng size)))
                              [sample1 sample2])))
      (< num-samples size) (loop [sample-set #{}]      ; loop until we've collected the right number of samples
                             (if (= (count sample-set) num-samples)
                               sample-set
                               (recur (conj sample-set (nth v (.nextInt rng size)))))) ; if we found a duplicate, let set figure that out
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

(defn selfaware-transient-identity-sample-vec-wout-repl
  "Sample num-samples from vector v without replacement.
  Derived from Incanter's algorithm from sample-uniform.
  Assumes that samples can be compared by identity.  Returns
  a vector or set; if you want something particular, that's
  the caller's responsibility."
  [^MersenneTwisterFast rng ^long num-samples v to-exclude]
  (let [size (count v)]
    (loop [sample-set #{}]
      (if (= (count sample-set) num-samples)
        sample-set
        (let [sample (nth v (.nextInt rng size))]
          (if (identical? to-exclude sample) ; if it's the one we don't want,
            (recur sample-set)               ; lose it
            (recur (conj sample-set sample))))))))

(def sample-wout-repl selfaware-transient-identity-sample-vec-wout-repl)

;;; This software is copyright 2015, 2019 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; Plot beta distributions, with various parameterizations.
;; NOTE: This is *not* used by the intermittran simulation
;; Intermittran uses the Beta class from MersenneTwisterFast.
;; This file, which uses Incanter's pdf-beta, is *just* a utility
;; to make it easy to plot various beta functions and experiment to
;; see what different parameter combinations do.

;; NOTE: These plots use JFreeChart via Incanter.  When the mean is too close to 0 or 1,
;; the chart becomes non-sensical; the y ticks disappear, and nothing is plotted.
;; You can still make a plot e.g. with R in this situation.

;; Other useful things:
;; (use '[incanter.pdf])
;; (save-pdf xyp filename)
;; where xyp is a plot object.

(ns utils.plots
  (:require [incanter.core :as ic]
            [incanter.stats :as ist]
            [incanter.charts :as ich])
  (:import PinkNoise
           PinkNoiseFast
           [ec.util MersenneTwister MersenneTwisterFast])) ; note MersenneTwister is slow but implements java.util.Random

;; EXAMPLES:
; (simple-plot 200 (normalize 2 (pink-nums (make-pink 1 10))))
;;
; (map ist/mean (partition 100 1 (pink-nums (make-pink 1 1000))))
;; Define some pink noise values:
;    (def ps (pink-nums (make-pink 1 1000)))
;; Define a sequence of means over the first n values in the pink sequence:
;    (def means (map mean (map take (map inc (range)) (repeat ps))))
;; Plot the means to see whether they seem to converge:
;    (simple-plot (take 1000 means))
;; Same thing as a 1-liner:
;    (simple-plot (take 1000 (map mean (map take (map inc (range)) (repeat (pink-nums (make-pink 1 1000)))))))
;; Normalize the noise to [0,1]:
;    (map (partial normal-logistic 1) (pink-nums (make-pink 1 1000)))
;; BUT SEE normal-pink-nums, which does this for you.
;; Plot first 1000 means for logistic-tranformed pink-nums:
;    (let [nps (logistic-pink-nums (make-pink 1 1000) 1) means (map mean (map take (map inc (range)) (repeat nps)))] (simple-plot 1000 means))
;; geneate a new sequence but preserve the ability to extend it further:
;    (def lps$ (atom nil))
;    (def means$ (atom nil))
;    (let [lps (logistic-pink-nums (make-pink 1 1000) 1) 
;          means (map mean (map take (map inc (range)) (repeat lps)))]
;       (reset! lps$ lps) (reset! means$ means) (simple-plot 1000 means))
;    (simple-plot 2000 @means$)


;; Used by multiple functions
(def +xs+ (range 0.001 1 0.001)) ; Start the range above 0, which would map to Infinity when alpha < 1. Infinity confuses xy-plot.  Note there may be an extra value that's just below 1.

(declare beta-plot beta-plot* beta-plot** alpha-parm beta-parm sample-size-parm variance variance*)

(defn roul
  "Treats inputs as spins of a wheel of fortune with widths d."
  [d x]
  (if (odd? (int (quot (* d x) 1))) 1 0))


(defn simple-replot
  "Given plot object xyp, adds the values in ys in relation to x values from 0
  to the length of ys, displays the modified plot object, and returns it.  If n
  is provided, no more than the first n elements of ys will be used."
  ([xyp ys] (let [xs (range (count ys))]
              (ic/view xyp)
              (ich/add-lines xyp xs ys)))
  ([xyp n ys] (simple-replot xyp (take n ys))))

(defn simple-plot
  "Displays a plot of the values in ys in relation to x values from 0 to the length
  of ys, and returns the plot object.  A new xy-plot object is generated and returned.
  If n is provided, no more than the first n elements of ys will be used."
  ([ys] (simple-replot (ich/xy-plot) ys))
  ([n ys] (simple-replot (ich/xy-plot) n ys)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn beta-plot
  "Display and return a plot of a beta distribution with parameters alpha
  and beta, or add a new plot to an existing one, if passed a plot as p.
  Returns the plot."
  ([alpha beta]
   (let [xyp (ich/xy-plot)]
     (ic/view xyp)
     (beta-plot xyp alpha beta)))
  ([xyp alpha beta]
   (ich/add-lines xyp +xs+ (ist/pdf-beta +xs+ :alpha alpha :beta beta))))

(defn beta-plot*
 "Display and return a plot of a beta distribution with given mean mn and
 \"sample-size\" samp-sz, i.e. the sum of the usual alpha and beta parameters
 [alpha = sample-size * mean, and beta = sample-size * (1 - mean)].  If a plot
 object is passed as xyp, add to an existing plot."
  ([mn samp-sz] 
   (beta-plot (alpha-parm mn samp-sz) (beta-parm mn samp-sz)))
  ([xyp mn samp-sz] 
   (beta-plot xyp (alpha-parm mn samp-sz) (beta-parm mn samp-sz))))

(defn beta-plot**
  "Display and return a plot of a beta distribution with given mean and 
  variance.  Variance must be less than (mn * (1 - mn))."
  ([mn variance]
   (beta-plot* mn (sample-size-parm mn variance)))
  ([xyp mn variance]
   (beta-plot* xyp mn (sample-size-parm mn variance))))

(defn beta-plots*
  "Display a range of beta distributions with the same sample-size but 
  different means using beta-plot*.  Returns a sequence containing the 
  plot object followed by the distributions' variances, in order."
  [samp-sz]
  (let [xyp (ich/xy-plot)
        mns (rest (range 0 1 1/20))]
    (ich/set-title xyp (str "Beta distributions with sample size = " samp-sz)) ; xy-plot's :title option doesn't seem to work
    (ic/view xyp)
    (doseq [mn mns]
      (beta-plot* xyp mn samp-sz))
    (cons xyp
          (map #(variance* (double %) samp-sz) mns))))

(defn beta-plots**
  "Display a range of beta distributions with the same variance different
  means using beta-plot**.  Only works for very small variances, since 
  variance must be less than (mean * (1 - mean))."
  [variance]
  (let [xyp (ich/xy-plot)]
    (doseq [mn (rest (range 0 1 1/5))]
      (beta-plot** xyp mn variance))
    (ic/view xyp)))


;; Different ways of calculating parameters and associated values:

(defn mean-product
  [mn]
  (* mn (- 1 mn)))

(defn alpha-parm
  [mn samp-sz]
  (* mn samp-sz))

(defn beta-parm
  [mn samp-sz]
  (* (- 1 mn) samp-sz))

(defn sample-size-parm
  [mn variance]
  (let [mean-prod (mean-product mn)]
    (when (>= variance mean-prod)
      (throw
        (Exception.
          (clojure.pprint/cl-format nil "variance = ~s is not less than (mean * (1 - mean)) = ~s for mean = ~s" 
                                    variance mean-prod mn))))
    (dec (/ mean-prod variance))))

(defn variance
  "Calculate variance of a beta distribution from its alpha and beta parameters."
  [alpha beta]
  (let [absum (+ alpha beta)]
    (/ (* alpha beta)
       (* absum absum (inc absum)))))
     
(defn variance*
  "Calculate the variance of a beta distribution from its mean and \"sample-size\"."
  [mn sample-size]
  (/ (mean-product mn)
     (inc sample-size)))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

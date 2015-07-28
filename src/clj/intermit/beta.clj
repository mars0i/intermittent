;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

(ns intermit.beta
  (:require [incanter.core :as ic]
            [incanter.stats :as ist]
            [incanter.charts :as ich]))

(declare beta-plot beta-plot* beta-plot** alpha-parm beta-parm sample-size-parm variance variance*)

;; Plot beta distributions, with various parameterizations:

;; NOTE: These plots use JFreeChart via Incanter.  When the mean is too close to 0 or 1,
;; the chart becomes non-sensical; the y ticks disappear, and nothing is plotted.
;; You can still make a plot e.g. with R in this situation.

(defn beta-plot
  "Display and return a plot of a beta distribution with parameters alpha
  and beta, or add a new plot to an existing one, if passed a plot as p.
  Returns the plot."
  ([alpha beta]
   (let [p (ich/xy-plot)]
     (ic/view p)
     (beta-plot p alpha beta)))
  ([p alpha beta]
   (let [xs (range 0.001 1 0.001)] ; Start the range above 0, which would map to Infinity when alpha < 1. Infinity confuses xy-plot.  Note there may be an extra value that's just below 1.
     (ich/add-lines p xs (ist/pdf-beta xs :alpha alpha :beta beta)))))

(defn beta-plot*
 "Display and return a plot of a beta distribution with given mean mn and
 \"sample-size\" samp-sz, i.e. the sum of the usual alpha and beta parameters
 [alpha = sample-size * mean, and beta = sample-size * (1 - mean)].  If a plot
 object is passed as p, add to an existing plot."
  ([mn samp-sz] 
   (beta-plot (alpha-parm mn samp-sz) (beta-parm mn samp-sz)))
  ([p mn samp-sz] 
   (beta-plot p (alpha-parm mn samp-sz) (beta-parm mn samp-sz))))

(defn beta-plot**
  "Display and return a plot of a beta distribution with given mean and 
  variance.  Variance must be less than (mn * (1 - mn))."
  ([mn variance]
   (beta-plot* mn (sample-size-parm mn variance)))
  ([p mn variance]
   (beta-plot* p mn (sample-size-parm mn variance))))


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


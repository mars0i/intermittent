;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

(ns intermit.plots
  (:require [incanter.core :as ic]
            [incanter.stats :as ist]
            [incanter.charts :as ich]))

(defn beta-plot
  "Display and return a plot of a beta distribution with parameters alpha
  and beta, or add a new plot to an existing one, if passed a plot as p.
  Returns the plot."
  ([alpha beta]
   (let [p (ich/xy-plot)]
     (ic/view p)
     (beta-plot p alpha beta)))
  ([p alpha beta]
   (let [xs (range 0 1 0.01)]
     (ich/add-lines p xs (ist/pdf-beta xs :alpha alpha :beta beta)))))

(defn calc-alpha
  [mn samp-sz]
  (* mn samp-sz))

(defn calc-beta
  [mn samp-sz]
  (* (- 1 mn) samp-sz))

(defn beta-plot*
 "Display and return a plot of a beta distribution with given mean mn and
 \"sample-size\" samp-sz, i.e. the sum of the usual alpha and beta parameters
 [alpha = sample-size * mean, and beta = sample-size * (1 - mean)].  If a plot
 object is passed as p, add to an existing plot."
  ([mn samp-sz] 
   (beta-plot (calc-alpha mn samp-sz) (calc-beta mn samp-sz)))
  ([p mn samp-sz] 
   (beta-plot p (calc-alpha mn samp-sz) (calc-beta mn samp-sz))))

(defn calc-sample-size
  [mn variance]
  (let [mean-product (* mn (- 1 mn))]
    (when (>= variance mean-product)
      (throw
        (Exception.
          (clojure.pprint/cl-format nil "variance = ~s is not less than (mean * (1 - mean)) = ~s for mean = ~s" 
                                    variance mean-product mn))))
    (dec (/ mean-product variance))))

(defn beta-plot**
  "Display and return a plot of a beta distribution with given mean and 
  variance.  Variance must be less than (mn * (1 - mn))."
  ([mn variance]
   (beta-plot* mn (calc-sample-size mn variance)))
  ([p mn variance]
   (beta-plot* p mn (calc-sample-size mn variance))))

;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;(set! *warn-on-reflection* true)

(ns intermit.file
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:import [sim.util Double2D]))

(defn double2d-coll-to-y-rows
  "Given a sequence of Double2D's, returns a vector in which each
  element is a vector containing the y value of the Double2D that is
  the corresponding of the original sequence."
  [double2d-coll]
  (map #(vector (.y ^Double2D %)) double2d-coll))

(defn spit-csv
  "Given a sequence of sequences of data, opens a file and writes to it
  using write-csv.  Options are those that can be passed to spit or writer."
  [f rows & options]
   (with-open [w (apply io/writer f options)]
     (csv/write-csv w rows)))

(defn spit-ys
  "Given a file name and a sequence of Double2D's, the y values of the Double2D's,
  one in each row.  options are options to clojure.java.io.writer.  For example,
  to append to an existing file, add :append true after the sequence."
  [f double2d-coll & options]
  (apply spit-csv 
         f 
         (double2d-coll-to-y-rows double2d-coll) 
         options))

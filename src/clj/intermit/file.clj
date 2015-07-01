;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;(set! *warn-on-reflection* true)

(ns intermit.file
  (:require ;[intermit.Sim :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:import [sim.util Double2D])
  )

(defn double2d-coll-to-y-rows
  [double2d-coll]
  (map #(vector (.y ^Double2D %)) double2d-coll))

(defn spit-csv
  "Given a sequence of sequences of data, opens a file and writes to it
  using write-csv.  Options are those that can be passed to spit or writer."
  [f rows & options]
   (with-open [w (apply io/writer f options)]
     (csv/write-csv w rows)))

(defn spit-ys
  [f double2d-coll]
  (spit-csv f
            (double2d-coll-to-y-rows double2d-coll)))

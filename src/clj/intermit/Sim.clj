;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; Note: Traditional MASON models put e.g. Continuous2D and Network here, 
;; and then the GUIState class would normally use those instances from 
;; this class, passing them to portrayals created in the GUIState class.
;; Since the underlying model doesn't need spatial relations or explicit
;; link representations, I only create the Continuous2D and Network objects
;; in the GUIState class (SimWithGUI), where they're needed to be used by portrayals.

;; IN THIS VERSION:
;; * There is a step function in each agent, i.e. Indiv implements Steppable.
;; * The scheduler calls the agents' (indivs') step functions in random order on each timestep.
;; * Indivs update their states sequentially in this random order, rather than updating in
;;   parallel by updating a "new" version of a variable from others "old" versions.

;; Tip: Methods named "getBlahBlah" or "setBlahBlah" will be found by the UI via reflection.

;(set! *warn-on-reflection* true)

;; Put gen-class Sim first so we can type-hint methods in Indiv etc.
;; But put intermit.Sim's methods at end, so we can type-hint references to Indiv, etc. in them.
(ns intermit.Sim
  (:require [clojure.tools.cli :as cli]
            [clojure.pprint :as pp])
  (:import [sim.engine Steppable Schedule]
           [sim.portrayal Oriented2D]
           [sim.util Interval Double2D]
           [sim.util.distribution Poisson Normal]
           [ec.util MersenneTwisterFast]
           [java.lang String]
           [java.util Collection]
           [intermit Sim]) ; import rest of classes after each is defined
  (:gen-class :name intermit.Sim
              :extends sim.engine.SimState                         ; includes signature for the start() method
              :exposes-methods {start superStart}                  ; alias method start() in superclass. (Don't name it 'super-start'; use a Java name.)
              :methods [[getNumCommunities [] long]                ; these methods are defined much further down
                        [setNumCommunities [long] void]
                        [getMeanIndivsPerCommunity [] long]
                        [setMeanIndivsPerCommunity [long] void]
                        [getLinkProb [] double]
                        [setLinkProb [double] void]
                        [domLinkProb [] java.lang.Object]
                        [getTranStddev [] double]
                        [setTranStddev [double] void]
                        [domTranStddev [] java.lang.Object]
                        [getGlobalInterlocMean [] double]     ; i.e. mean # of interlocutors from global population
                        [setGlobalInterlocMean [double] void]
                        ;[domGlobalInterlocMean [] java.lang.Object]
                        [getSuccessStddev [] double]
                        [setSuccessStddev [double] void]
                        [domSuccessStddev [] java.lang.Object]
                        [getSuccessMean [] double]
                        [setSuccessMean [double] void]
                        [domSuccessMean [] java.lang.Object]
                        [getReligDistribution [] "[D" ]
                        [getMeanReligDistribution [] "[D" ]
                        [getMeanReligTimeSeries [] "[Lsim.util.Double2D;"]
                        [getSuccessDistribution [] "[D" ]
                        [getMeanSuccessDistribution [] "[D" ]
                        [getMeanSuccessTimeSeries [] "[Lsim.util.Double2D;"]
                        [getLinkStyle [] long]
                        [setLinkStyle [long] void]
                        [domLinkStyle [] java.lang.Object]]
              :state instanceState
              :init init-instance-state
              :main true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS AND GENERAL UTILITY CODE

(declare sample-wout-repl-or-me choose-others-from-pop choose-most-successful add-noise sum-relig calc-success getRelig
         getSuccess get-population link-styles binomial-link-indivs! sequential-link-indivs! both-link-indivs! 
         link-style-names link-style-idxs binomial-link-style-idx sequential-link-style-idx both-link-style-idx)

(def initial-num-communities 12) ; use something that factors into x and y dimensions
(def initial-mean-indivs-per-community 15)
(def initial-link-prob 0.20)
(def initial-tran-stddev 0.03)
(def initial-global-interloc-mean 0.01)     ; i.e. Poisson-mean interlocutors from global population
(def initial-success-stddev 2.0)
(def initial-success-mean 0.0)
(def initial-link-style-idx 1) ; This is an index into link-style-names and link-style-fns, defined below.
;; (We can't put link-style-fns here; eval'ing them at this point produces nothing.)

(defn remove-if-identical
  "Removes from coll any object that's identical to obj."
  [obj coll]
  (remove #(identical? obj %) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTANCESTATE FOR SIM CLASS
;; Used to hold mutable data in Sim's instanceState variable
;; Need def here so we can type-hint Indiv's methods

;; Note some of these have to be atoms so that that we can allow restarting with a different setup.
(deftype InstanceState [; run parameters:
                        numCommunities          ; number of communities
                        meanIndivsPerCommunity  ; mean or exact number of indivs in each
                        linkStyleIdx ; see sect 3.4.2, "MASON Extensions",  of MASON manual v. 19
                        linkProb
                        tranStddev
                        globalInterlocMean ; mean number of interlocutors from global pop
                        successStddev
                        successMean
                        ; runtime storage slots:
                        communities             ; holds the communities
                        population              ; holds all individuals
                        poisson                 ; a Poisson-distribution wrapper
                        gaussian                ; a Normally-distribution wrapper
                        meanReligSeries         ; records mean relig values at timestep
                        meanSuccessSeries])     ; records mean success values at timestep

(defn -init-instance-state
  "Initializes instance-state when an instance of class Sim is created."
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom initial-link-style-idx)
                          (atom initial-link-prob)
                          (atom initial-tran-stddev)
                          (atom initial-global-interloc-mean)
                          (atom initial-success-stddev)
                          (atom initial-success-mean)
                          (atom nil)   ; communities
                          (atom nil)   ; population
                          (atom nil)   ; poisson
                          (atom nil)   ; gaussian
                          (atom [])    ; meanReligSeries
                          (atom []))]) ; meanSuccessSeries

(defn -getNumCommunities ^long [^Sim this] @(.numCommunities ^InstanceState (.instanceState this)))
(defn -setNumCommunities [^Sim this ^long newval] (reset! (.numCommunities ^InstanceState (.instanceState this)) newval))
(defn -getMeanIndivsPerCommunity ^long [^Sim this] @(.meanIndivsPerCommunity ^InstanceState (.instanceState this)))
(defn -setMeanIndivsPerCommunity [^Sim this ^long newval] (reset! (.meanIndivsPerCommunity ^InstanceState (.instanceState this)) newval))
(defn -getLinkProb ^double [^Sim this] @(.linkProb ^InstanceState (.instanceState this)))
(defn -setLinkProb [^Sim this ^double newval] (reset! (.linkProb ^InstanceState (.instanceState this)) newval))
(defn -domLinkProb [this] (Interval. 0.0 1.0))
(defn -getTranStddev ^double [^Sim this] @(.tranStddev ^InstanceState (.instanceState this)))
(defn -setTranStddev [^Sim this ^double newval] (reset! (.tranStddev ^InstanceState (.instanceState this)) newval))
(defn -domTranStddev [this] (Interval. 0.0 1.0))
(defn -getGlobalInterlocMean ^double [^Sim this] @(.globalInterlocMean ^InstanceState (.instanceState this)))
(defn -setGlobalInterlocMean [^Sim this ^double newval] 
  (let [^InstanceState istate (.instanceState this)]
    (reset! (.globalInterlocMean istate) newval)    ; store it so that UI can display its current value
    (when-let [^Poisson poisson @(.poisson istate)] ; avoid npe: poisson isn't created until start is run (at which point it will be init'ed with value of globalInterlocMean)
      (.setMean poisson newval))))                  ; allows changing value during the middle of a run.
;(defn -domGlobalInterlocMean [this] (Interval. 0.0 20.0)) ; a mean for a Poisson distribution.  Should go high enough to guarantee that everyone talks to everyone, but large numbers choke the app.
(defn -getSuccessStddev ^double [^Sim this] @(.successStddev ^InstanceState (.instanceState this)))
(defn -setSuccessStddev [^Sim this ^double newval] (reset! (.successStddev ^InstanceState (.instanceState this)) newval))
(defn -domSuccessStddev [this] (Interval. 0.0 3.0))
(defn -getSuccessMean ^double [^Sim this] @(.successMean ^InstanceState (.instanceState this)))
(defn -setSuccessMean [^Sim this ^double newval] (reset! (.successMean ^InstanceState (.instanceState this)) newval))
(defn -domSuccessMean [this] (Interval. -1.0 1.0)) 

;; We set the function that decides how to link nodes using MASON's popup menu functionality,
;; which uses a mapping between strings in an array and their indexes.  It's the string that's
;; displayed; it's the index that's returned, and that we need to use to choose the appropriate function.
;; See related defs below, and sect 3.4.2, "MASON Extensions",  of MASON manual v. 19.
(defn -getLinkStyle ^long [^Sim this] @(.linkStyleIdx ^InstanceState (.instanceState this)))
(defn -setLinkStyle [^Sim this ^long newval] (reset! (.linkStyleIdx ^InstanceState (.instanceState this)) newval))
(defn -domLinkStyle [^Sim this] (into-array link-style-names))


;; Useful since the fields contain atoms:
(defn get-communities [^Sim this] @(.communities ^InstanceState (.instanceState this)))
(defn get-population [^Sim this] @(.population ^InstanceState (.instanceState this)))

(defn -getReligDistribution
  "Returns array of doubles of relig values in population at current timestep."
  [^Sim this] 
  (double-array (map getRelig (get-population this))))

(defn -getMeanReligTimeSeries
  "Returns array of sim.util.Double2D's in which the first element is a
  timestep and the second is the population's mean relig at that timestep."
  [^Sim this] 
  (into-array sim.util.Double2D @(.meanReligSeries ^InstanceState (.instanceState this)))) ; Double2D version: just convert Clojure vector to Java array

(defn -getMeanReligDistribution
  "Returns array of doubles containing the population's mean relig values at
  all timesteps until and including the current timestep.  (Useful for generating
  a histogram over all timesteps so far.)"
  [^Sim this]
  (double-array (map #(.y ^Double2D %) @(.meanReligSeries ^InstanceState (.instanceState this)))))    ; Double2D version: extract data in y element

(defn -getSuccessDistribution 
 "Returns array of doubles of success values in population at current timestep."
  [^Sim this]
  (double-array (map getSuccess (get-population this))))

(defn -getMeanSuccessTimeSeries
  "Returns array of sim.util.Double2D's in which the first element is a
  timestep and the second is the population's mean success at that timestep."
  [^Sim this] 
  (into-array sim.util.Double2D @(.meanSuccessSeries ^InstanceState (.instanceState this)))) ; Double2D version: just convert Clojure vector to Java array

(defn -getMeanSuccessDistribution
  "Returns array of doubles containing the population's mean success values at
  all timesteps until and including the current timestep.  (Useful for generating
  a histogram over all timesteps so far.)"
  [^Sim this]
  (double-array (map #(.y ^Double2D %) @(.meanSuccessSeries ^InstanceState (.instanceState this)))))    ; Double2D version: extract data in y element

;;; MORE METHODS FOR Sim BELOW.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROTOCOLS/INTERFACES

;; Separating these allows future versions in which e.g. communities can communicate,
;; and allows a unified interface for finding out average culture of a community
;; cultural values of indivs, etc.

(defprotocol IndivP
  "Protocol for Indivs."
  (getId [this])
  (getSuccess [this])   
  (getRelig [this])     
  (getNeighbors [this]) 
  (get-restofpop [this]) 
  (get-prev-speaker [this])
  (add-neighbor! [this newval])
  (set-restofpop! [this newval])
  (update-success! [this sim])
  (copy-relig! [this sim population]))

(defprotocol CommunityP
  (get-members [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDIV: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

;; NOTE: Objects created by deftype faster than those created by
;; defrecord when they are hashed (e.g. in the set that's used in sample-wout-repl).
;; 
;; volatile-mutable is a bit inconvenient since it requires accessors,
;; but it's faster than atoms, and these fields get accessed a lot.


(deftype Indiv [id
                ^:volatile-mutable success 
                ^:volatile-mutable relig 
                ^:volatile-mutable neighbors 
                ^:volatile-mutable restofpop
                ^:volatile-mutable prevspeaker]
  IndivP
    (getId [this] id)
    (getSuccess [this] success)
    (getRelig [this] relig)
    (getNeighbors [this] neighbors)
    (get-restofpop [this] restofpop)
    (get-prev-speaker [this] prevspeaker)
    (add-neighbor! [this new-neighbor] (set! neighbors (conj neighbors new-neighbor)))
    (set-restofpop! [this indivs] (set! restofpop indivs))
    (copy-relig! [this sim-state population]
      (let [^Sim sim sim-state ; can't type hint ^Sim in the parameter list
            ^MersenneTwisterFast rng (.random sim)
            ^InstanceState istate (.instanceState sim)
            ^Poisson poisson @(.poisson istate)
            ^Normal gaussian @(.gaussian istate)
            ^double stddev @(.tranStddev istate)]
        (set! prevspeaker nil) ; maybe refactor when-let, when below to make this the alt condition
        (when-let [^Indiv best-model (choose-most-successful 
                                       rng
                                       (into neighbors ;   (a) neighbors, (b) 0 or more random indivs from entire pop
                                             (choose-others-from-pop rng poisson this)))]
          (when (> (getSuccess best-model) success)     ; is most successful other, better than me?
            (set! relig (add-noise gaussian 0.0 stddev (getRelig best-model)))
            (set! prevspeaker best-model)))))
    (update-success! [this sim-state]
      (let [^Sim sim sim-state ; can't type hint ^Sim in the parameter list
            ^InstanceState istate (.instanceState sim)
            ^Normal gaussian @(.gaussian istate)
            ^double stddev @(.successStddev istate)
            ^double mean @(.successMean istate)]
        (set! success (add-noise gaussian mean stddev (calc-success relig neighbors)))))
  Steppable
    ;; Note that by maintaining only a single version of vars, and allowing each indiv to be stepped in random order, we allow per-tick path dependencies.
    (step [this sim-state] 
      (let [^intermit.Sim sim sim-state  ; kludge to cast to my class--can't put it in signature
            ^intermit.Sim.InstanceState istate (.instanceState sim)
            population @(.population istate)]
       (copy-relig! this sim @(.population istate)))) 
  Oriented2D ; display pointer in GUI
    (orientation2D [this] (+ (/ Math/PI 2) (* Math/PI success))) ; pointer goes from down (=0) to up (=1)
  Object
    (toString [this] (str id ": " success " " relig " " (vec (map #(.id %) neighbors)))))

;;; Runtime functions:

(defn sum-relig
  "Sum the relig values of indivs along with initial value init-value.
  Suitable for use with reduce."
  [^double init-value indivs]
  (let [add-relig (fn [^double acc ^Indiv indiv] (+ acc ^double (getRelig indiv)))]
    (reduce add-relig init-value indivs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALTERNATIVE SHUFFLING AND SAMPLING FUNCTIONS (not all in use)

;; BUG: Returns a set, at present.  There's no reason to think that when
;; converted to a vector, the result is in random order,
;; but I need for it to be random if choose-most-successful requires that.
;;
;; It's much faster to remove the originating Indiv from samples here,
;; rather than removing it from the collection to be sampled, at least
;; for reasonably large populations.  Makes this function more complicated, 
;; though.  Hopefully the bugs are out now.
(defn sample-wout-repl-or-me
  "Special sample without replacement function:
  Returns num-samples samples from coll without replacement, excluding 
  items identical? to me.  Returns a vector or a set; if you want something
  more specific, it's the caller's responsibility.  Should be fastest when
  coll has fast index access (e.g. it's a vector) and when the elements hash
  by identity (e.g. when defined by deftype rather than defrecord).  NOTE:
  results are not in random order."
  [^MersenneTwisterFast rng ^long num-samples me coll]
  (let [size (count coll)
        size-wout-me (dec size)] ; how many elements, excluding me?
    (when (>= num-samples size) (throw (Exception. "num-samples is larger than coll size w/out 'me'")))
    (loop [sample-set #{}]   ; don't return coll as is when num-samples = size; need to allow it to be shuffled anyway (maybe not so efficient ...)
      (if (== (count sample-set) num-samples)
        sample-set
        (let [sample (nth coll (.nextInt rng size))]
          (if (identical? me sample) ; if it's the one we don't want,
            (recur sample-set)       ; lose it
            (recur (conj sample-set sample))))))))

;;;;;;
;;; The take-randX versions both sample without replacement a collection and shuffle the output.
;;; take-randnth seems a little slower; the other three are similar in speed.

;; From Rosetta Code page for "Knuth Shuffle" (= Fisher-Yates): http://rosettacode.org/wiki/Knuth_shuffle#Clojure
;; Note vect *must* be a vector.  
;; Very slow on vec of len 200--like take-rand[3-5], it's an order of magnitude slower than bag-shuffle.
;; I added use of rng, fixed typo (missing paren at end of anonymous fn in original), and added the use
;; of transients, which produces only a slight improvement.
(defn rosetta-shuffle [rng vect]
  (persistent!
    (reduce (fn [v i] (let [r (.nextInt rng i)]
                        (assoc! v i (v r) r (v i))))  ; i.e. starting from vector v, replace element at i with (v r), the element at r, and vice versa.
            (transient vect)
            (range (dec (count vect)) 1 -1)))) ; counts down from one less than length to 2, inclusive.


;; Marshall Abrams
;; Faster than Clojure's shuffle, at least for pops of size 200 or so
(defn bag-shuffle
  [rng coll]
  (let [bag (sim.util.Bag. coll)]
    (.shuffle bag rng)
    bag))

;; On large sample, almost as fast as take-rand1, but uses MersenneTwisterFast.
;; Note that the 'take' gives a 2X drop in perf over mere bag-shuffle, so 
;; if you know you want the whole thing, just shuffle it.
;; On small sample, order of mag slower than take-rand[3-5].
;; Marshall Abrams
(defn bag-sample
  [rng n coll]
  (subvec (vec (bag-shuffle rng coll)) 0 n))
  ;(take n (bag-shuffle rng coll))) ; seems a little slower than subvec sometimes

;; DON'T USE THIS: It uses Java's rng
;; Very slow for small samples, but incredibly fast for large samples.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
; naive, O(n+m)
(defn take-rand1 [_ n coll] (take n (shuffle coll)))
 
;; THIS ONE IS ACTUALLY A BIT FASTER THAN THE ... COOLER ONES BELOW FOR SMALL SAMPLES,
;; BUT IT'S DOG SLOW FOR LARGE SAMPLES--2 orders of magnitude less than take-rand1.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
; lazy, O(n!@#$%m^&)
(defn take-rand2 [rng n coll]
  (let [coll (vec coll)
        len (count coll)]
    (take n (distinct (repeatedly #(nth coll (.nextInt rng len)))))))

;; TODO CHECK WHETHER THESE ARE FISHER-YATES SHUFFLES:

;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; by Pepijn de Vos, pepijndevos from https://gist.github.com/pepijndevos/805747, with small mod by Marshall
;; reduce, reorder, subvec, O(m)
(defn take-rand3 [rng nr coll]
  (let [len (count coll)
        ; why doesn't rand-int take a start?
        rand-int (fn [lo hi] (+ lo (.nextInt rng (- hi lo))))]
    (subvec (->> (range nr)
                 (reduce #(conj %1 [%2 (rand-int %2 len)]) []) ; for ea num in range, assoc it with a rand idx between num and end
                 (reduce
                   (fn swap [a [i b]]
                      (assoc a b (get a i) i (get a b)))
                   coll))
            0 nr)))


;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; from https://gist.github.com/pepijndevos/805747, by amalloy (?), with small mod by Marshall
; amalloy, O(m)
(defn take-rand4 [rng nr coll]
  (first
   (nth
    (iterate (fn [[ret candidates]]
               (let [idx (.nextInt rng (count candidates))]
                 [(conj ret (candidates idx))
                  (subvec (assoc candidates idx (candidates 0))
                          1)]))
             [[]
              coll])
    nr)))

;; Excellent on small samples, though not as good as take-rand2.
;; One order of magnitude slower than take-rand1 on large samples.
;; A LITTLE FASTER than the preceding two on large samples.
;; from https://gist.github.com/pepijndevos/805747, by amalloy (?), with small mod by Marshall
; amalloy, o(mg)
(defn take-rand5 [rng nr coll]
  (take nr
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (.nextInt rng c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
           (transient coll))))

;; One order of magnitude slower than take-rand1 on large samples.
;; By amalloy, from https://gist.github.com/amalloy/805546, with small mod by Marshall:
(defn take-randnth [rng nr coll]
  (take nr
        (rest
         (map first
              (iterate (fn [[ret items]]
                         (let [idx (.nextInt rng (count items))]
                           [(items idx)
                            (subvec (assoc items idx (items 0))
                                    1)]))
                       [nil
                        (vec coll)])))))

;; By amalloy, from https://gist.github.com/amalloy/805546:
;; See also http://stackoverflow.com/questions/3944556/what-if-anything-is-wrong-with-this-shuffling-algorithm-and-how-can-i-know
(defn lazy-shuffle [coll]
  ((fn shuffle [^clojure.lang.ITransientVector coll]
     (lazy-seq
      (let [c (count coll)]
        (when-not (zero? c)
          (let [n (rand-int c)]
            (cons (get coll n)
                  (shuffle (.pop (assoc! coll n (get coll (dec c)))))))))))
   (transient (vec coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is probably going overboard in service of speed, even though the 
;; optimizations do make big differences.  It's an experiment.
(defn choose-others-from-pop
  "Randomly sample a Poisson-distributed number of indivs from population,
  excluding me.  (The mean for the Poisson distribution is stored in the
  poisson object.)"
  [^MersenneTwisterFast rng ^Poisson poisson ^Indiv me]
  (let [restofpop (get-restofpop me)
        size (count restofpop)
        rand-num (.nextInt poisson)
        num-to-choose (if (< rand-num size) rand-num size)] ; When Poisson mean is large, result may be larger than number of indivs.
    ;; Choose a method that's likely to be fast for this situation (with guesses at cutoffs based on informal tests):
    (cond (== num-to-choose size)  (bag-shuffle rng restofpop)   ; return pop in random order
          (zero? num-to-choose) []
          (== num-to-choose 1) (vector (nth restofpop (.nextInt rng size)))
          (== num-to-choose 2) (letfn [(one-more [[oldelt :as coll]]                        ; if we only want a small sample, do the stupid thing and just sample until they're unique
                                         (let [newelt (nth restofpop (.nextInt rng size))]  ; note that this is not precisely the correct probability.
                                           (if (identical? newelt oldelt)
                                             (recur coll) ; direct recursion may be slightly faster even, but there is a low-probability possibility of blowing the stack
                                             (conj coll newelt))))]
                                 (one-more (vector (nth restofpop (.nextInt rng size)))))
          (<= num-to-choose 5) (letfn [(a-few-more [still-needed coll]                      ; if we only want a small sample, do the stupid thing and just sample until they're unique
                                         (let [newelt (nth restofpop (.nextInt rng size))]  ; don't let the cutoff be too large; the probs aren't precisely correct here
                                           (cond (some #(identical? newelt %) coll) (recur still-needed coll) ; can't use this one--already have it
                                                 (== still-needed 1) (conj coll newelt)                       ; needed just one more, and we found it, so we're done
                                                 :else (recur (dec still-needed) (conj coll newelt)))))]      ; got a new one, but we need more
                                 (a-few-more (dec num-to-choose) (vector (nth restofpop (.nextInt rng size))))) ; get the first one
          (> (/ num-to-choose size) 1/50) (bag-sample rng num-to-choose restofpop) ; for large samples, use the bag shuffle and take
          :else  (take-rand5 rng num-to-choose restofpop))))


;; Can I avoid repeated accesses to the same field, caching them?  Does it matter?
;; 
;; Note that the analogous procedure in LKJPlus.nlogo, find-best, uses NetLogo's ask, which means
;; that subaks are always compared in random order.  In effect they're shuffled before the comparison
;; process starts.
;;
(defn choose-most-successful
  "Given a collection of Indiv's, returns the one with the greatest success, or
  nil if the collection is empty.  NOTE if there are ties, this will always
  use the first one found, so collection must already be in random order if you want
  a random member of the winners."
  ^Indiv [^MersenneTwisterFast rng models]
  (letfn [(compare-success 
            ([] nil) ; what reduce does if collection is empty
            ([^Indiv i1 ^Indiv i2]
             (let [^double success1 (getSuccess i1)
                   ^double success2 (getSuccess i2)]
               (cond (== success1 success2) i1 ; NOTE always returns the first winner
                     (> success1 success2) i1
                     :else i2))))]
    (reduce compare-success models)))
    ;(reduce compare-success (take-rand3 rng (count models) models))))

;; TODO note this means that e.g. if indiv is isolated, then when it happens to get high relig, it will also have high success.  Is that realistic?
(defn calc-success
  "Returns the average of relig values of a collection of indivs and
  one more indiv, who has relig value init-value.  i.e. the sum of all
  these values is divided by (count indivs) + 1."
  ^double [^double init-value indivs]
  (/ (sum-relig init-value indivs) 
     (inc (count indivs))))

;; TODO THIS IS NOT RIGHT.  MERELY MULTIPLYING BY STDDEV DOESN'T GIVE YOU A PROBABILITY DIST, SINCE THE INNER SD PARAM IS UNMODIFIED.
;; nextGaussian has mean 0 and stddev 1, I believe
(defn add-noise
 "Add Normal noise with stddev to value, clipping to extrema 0.0 and 1.0."
  ^double [^Normal gaussian ^double mean ^double stddev ^double value]
  (max 0.0 (min 1.0 (+ value ^double (.nextDouble gaussian mean stddev)))))

;;; Initialization functions:

(defn make-indiv
  "Make an indiv with appropriate defaults."
  [sim-state]
  (Indiv.
    (str (gensym "i")) ; id
    (.nextDouble (.random sim-state))  ; success
    (.nextDouble (.random sim-state))  ; relig
    []   ; neighbors
    []   ; restofpop
    nil))  ; prevspeaker


(defn binomial-link-indivs!
  "For each pair of indivs, with probability prob, make them each others' neighbors.
  Set prob to 1 to link all indivs to each other.  (This is a 'binomial' [edge
  dist], 'Poisson' [degree dist], or 'Erdös-Rényi' random graph.)"
  [rng prob indivs]
  (doseq [i (range (count indivs))
          j (range i)          ; lower triangle without diagonal
          :when (< (.nextDouble rng) prob)
          :let [indiv-i (nth indivs i)     ; fires only if when does
                indiv-j (nth indivs j)]]
      (add-neighbor! indiv-i indiv-j)
      (add-neighbor! indiv-j indiv-i)))

(defn sequential-link-indivs!
  "Links each indiv to the next indiv in the sequence.  Each
  indiv except the first and last will have two links."
  ([indivs]
   (let [size (count indivs)
         dec-size (dec size)]
     (doseq [i (range size)
             :when (< i dec-size)
             :let [indiv-i (nth indivs i)     ; fires only if when does
                   indiv-j (nth indivs (inc i))]]
       (add-neighbor! indiv-i indiv-j)
       (add-neighbor! indiv-j indiv-i))))
  ([rng prob indivs] (sequential-link-indivs! indivs)))

(defn both-link-indivs!
  "Runs sequential-link-indivs! and then runs binomial-link-indivs!"
  [rng prob indivs]
  (sequential-link-indivs! indivs)
  (binomial-link-indivs! rng prob indivs))

;; These defs must match up.  Not very Clojurely; needed for MASON auto-dropdown.
;; See comment above near domLinkStyles, and sect 3.4.2, "MASON Extensions",  of MASON manual v. 19.
(def link-style-names ["binomial"             "sequential"             "both"])
(def link-style-fns   [binomial-link-indivs!  sequential-link-indivs!  both-link-indivs!] )
(def binomial-link-style-idx 0)
(def sequential-link-style-idx 1)
(def both-link-style-idx 2)

(defn link-indivs!
  [idx rng prob indivs]
  ((get link-style-fns idx (get link-style-fns initial-link-style-idx)) ; the fallback case works in first run, when the atom contains a nil
   rng
   prob
   indivs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMUNITY: class for collections of Indivs or collections of Communities.

(deftype Community [id members]
  CommunityP
    (get-members [this] members) ; so I don't have to remember whether I used atoms
  Object
    (toString [this] (str id ": " (vec (map #(.id %) members)))))


;;; Runtime functions:

;;; Initialization functions:

(defn make-community-of-indivs
  "Make a community with size number of indivs in it."
  [sim size]
  (let [indivs  (vec (repeatedly size #(make-indiv sim))) ; it's short; don't wait for late-realization bugs.
        rng (.random sim)
        link-style-idx @(.linkStyleIdx (.instanceState sim))]
    (link-indivs! link-style-idx rng @(.linkProb (.instanceState sim)) indivs)
    (Community. (str (gensym "c")) indivs)))

(defn make-communities-into-pop!
  "Given a collection of communities, returns a vector of individuals in all 
  of the communities after doing any additional housekeeping needed."
  [communities]
  (let [population (vec (mapcat get-members communities))]
    (doseq [indiv population]
      (set-restofpop! indiv (vec (remove-if-identical indiv population))))
    population))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: reset of class for overall system

(def commandline (atom nil)) ; toplevel var to pass info from main to start. Must be a better way.

(defn record-commandline-args 
  [args]
  (let [cli-options [["-h" "--help" "Print this help"]
                     ["-n" "--num-comms <number of communities>" "Number of communities" :parse-fn #(Integer. %)]]
        usage-fmt (fn [options]
                    (let [fmt-line (fn [[short-opt long-opt desc]] (str short-opt ", " long-opt ": " desc))]
                      (clojure.string/join "\n" (concat (map fmt-line options)))))
        error-fmt (fn [errors] (str "The following errors occurred while parsing your command:\n\n" (apply str errors)))
        {:keys [options arguments errors summary] :as commline} (clojure.tools.cli/parse-opts args cli-options)]
    (when (:help options)
      (println (usage-fmt cli-options))
      (System/exit 0))
    (when errors 
      (println (error-fmt errors))
      (System/exit 1))
    (reset! commandline commline))) ; to be read in start method

(defn -main
  [& args]
  (record-commandline-args args)
  (sim.engine.SimState/doLoop intermit.Sim (into-array String args))
  (System/exit 0))

(defn report-run-params
  [^Sim sim]
  (let [istate (.instanceState sim)]
    (pp/cl-format true
                  "~ax~a indivs, link style = ~a, link prob (if relevant) = ~a, tran stddev = ~a, global interlocutor mean = ~a, success stddev = ~a, success mean = ~a~%"
                  @(.numCommunities istate)
                  @(.meanIndivsPerCommunity istate)
                  (link-style-names @(.linkStyleIdx istate))
                  @(.linkProb istate)
                  @(.tranStddev istate)
                  @(.globalInterlocMean istate)
                  @(.successStddev istate)
                  @(.successMean istate))))


;; doall all sequences below.  They're short, so there's no point in waiting for them to get realized who knows where/when.
(defn -start
  "Function called to (re)start a new simulation run.  Initializes a new
  set of communities, each with a new set of community members."
  [^Sim this]
  (.superStart this)
  ;; If user passed commandline options, use them to set parameters, rather than defaults:
  (when @commandline
    (let [{:keys [options arguments errors summary]} @commandline]
      (when-let [num-comms (:num-comms options)] (.setNumCommunities this num-comms))))
  ;; Construct core data structures of the simulation:
  (let [^Schedule schedule (.schedule this)
        ^InstanceState instance-state (.instanceState this)
        num-communities  @(.numCommunities instance-state)
        indivs-per-community @(.meanIndivsPerCommunity instance-state)
        communities (vec (repeatedly num-communities
                                     #(make-community-of-indivs this indivs-per-community)))
        population (make-communities-into-pop! communities)
        meanReligSeriesAtom (.meanReligSeries instance-state)]
    ;; Record core data structures and utility states:
    (reset! (.poisson instance-state) (Poisson. @(.globalInterlocMean instance-state) (.random this)))
    (reset! (.gaussian instance-state) (Normal. 0.0 1.0 (.random this))) ; mean and sd here can be overridden later
    (reset! (.communities instance-state) communities)
    (reset! (.population instance-state) population)
    (reset! meanReligSeriesAtom [])
    ;; Schedule each indiv's step function:
    (doseq [indiv population] (.scheduleRepeating schedule Schedule/EPOCH 0 indiv))  ; indivs' step fns run first to communicate relig
    ;; Schedule a step to update each indiv's success field:
    (.scheduleRepeating schedule Schedule/EPOCH 1                                    ; then update success fields afterwards
                        (reify Steppable 
                          (step [this sim-state]
                            (doseq [^Indiv indiv population] (update-success! indiv sim-state))
                            (swap! meanReligSeriesAtom
                                   conj 
                                   (Double2D.
                                     (double (.getSteps schedule)) ; coercion will happen automatically; I made it explicit. (getTime incorrect if funny scheduling.)
                                     (/ (sum-relig 0.0 population)
                                        (count population))))))))
  (report-run-params this))

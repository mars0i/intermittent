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
  (:require [intermit.utils :as u])
  (:import [sim.engine Steppable Schedule]
           [sim.portrayal Oriented2D]
           [sim.util Interval Double2D]
           [sim.util.distribution Poisson]
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
                        [getReligDistribution [] "[D" ]
                        [getSuccessDistribution [] "[D" ]
                        [getMeanReligDistribution [] "[D" ]
                        [getMeanReligTimeSeries [] "[Lsim.util.Double2D;"]
                        [getLinkStyle [] java.lang.String]
                        [setLinkStyle [java.lang.String] void]]
              :state instanceState
              :init init-instance-state
              :main true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS AND GENERAL UTILITY CODE

(def initial-num-communities 12) ; use something that factors into x and y dimensions
(def initial-mean-indivs-per-community 15)
(def initial-link-prob 0.20)
(def initial-tran-stddev 0.02)
(def initial-global-interloc-mean 0.025)     ; i.e. Poisson-mean interlocutors from global population
(def initial-success-stddev 2.0)
(def initial-link-style "binomial")

(declare sample-wout-repl-or-me choose-others-from-pop choose-most-successful add-noise sum-relig calc-success getRelig getSuccess get-population link-styles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTANCESTATE FOR SIM CLASS
;; Used to hold mutable data in Sim's instanceState variable
;; Need def here so we can type-hint Indiv's methods

;; Note some of these have to be atoms so that that we can allow restarting with a different setup.
(deftype InstanceState [numCommunities          ; number of communities
                        meanIndivsPerCommunity  ; mean or exact number of indivs in each
                        linkProb
                        tranStddev
                        globalInterlocMean ; mean number of interlocutors from global pop
                        successStddev
                        communities             ; holds the communities
                        population              ; holds all individuals
                        poisson
                        meanReligSeries
                        linkStyle])

(defn -init-instance-state
  "Initializes instance-state when an instance of class Sim is created."
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom initial-link-prob)
                          (atom initial-tran-stddev)
                          (atom initial-global-interloc-mean)
                          (atom initial-success-stddev)
                          (atom nil)   ; communities
                          (atom nil)   ; population
                          (atom nil)   ; poisson
                          (atom [])    ; meanReligSeries
                          (atom initial-link-style))]) 

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
(defn -domSuccessStddev [this] (Interval. 0.0 4.0)) ; since success ranges from 0 to 1, it doesn't make sense to have a stddev that's much larger than about 0.7.
(defn -getLinkStyle ^java.lang.String [^Sim this] @(.linkStyle ^InstanceState (.instanceState this)))
(defn -setLinkStyle [^Sim this ^java.lang.String newval] (reset! (.linkStyle ^InstanceState (.instanceState this)) newval))
(defn -domLinkStyle [^Sim this] (into-array (keys link-styles)))

;; Useful since the fields contain atoms:
(defn get-communities [^Sim this] @(.communities ^InstanceState (.instanceState this)))
(defn get-population [^Sim this] @(.population ^InstanceState (.instanceState this)))

(defn -getReligDistribution [^Sim this] (double-array (map getRelig (get-population this))))
(defn -getSuccessDistribution [^Sim this] (double-array (map getSuccess (get-population this))))

(defn -getMeanReligDistribution [^Sim this]
  (double-array (map #(.y ^Double2D %) @(.meanReligSeries ^InstanceState (.instanceState this)))))    ; Double2D version: extract data in y element
  ;(double-array (map #(second %) @(.meanReligSeries ^InstanceState (.instanceState this))))) ; vector version: strip ticks, extract data

(defn -getMeanReligTimeSeries [^Sim this] 
  (into-array sim.util.Double2D @(.meanReligSeries ^InstanceState (.instanceState this)))) ; Double2D version: just convert Clojure vector to Java array
  ;(into-array sim.util.Double2D                    ; vector of vectors version: convert to Java array of Double2D
  ;            (map (fn [[x y]] (Double2D. x y))
  ;                 @(.meanReligSeries ^InstanceState (.instanceState this))))) 

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
  (get-prev-speaker [this])
  (add-neighbor! [this newval])
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


(deftype Indiv [id ^:volatile-mutable success ^:volatile-mutable relig ^:volatile-mutable neighbors ^:volatile-mutable prevspeaker]
  IndivP
    (getId [this] id)
    (getSuccess [this] success)
    (getRelig [this] relig)
    (getNeighbors [this] neighbors)
    (get-prev-speaker [this] prevspeaker)
    (add-neighbor! [this new-neighbor] (set! neighbors (conj neighbors new-neighbor)))
    (update-success! [this sim-state]
      (let [^Sim sim sim-state ; can't type hint ^Sim in the parameter list
            ^MersenneTwisterFast rng (.random sim)
            ^InstanceState istate (.instanceState sim)
            ^double stddev @(.successStddev istate)]
        (set! success (add-noise rng stddev (calc-success relig neighbors)))))
    (copy-relig! [this sim-state population]
      (let [^Sim sim sim-state ; can't type hint ^Sim in the parameter list
            ^MersenneTwisterFast rng (.random sim)
            ^InstanceState istate (.instanceState sim)
            ^Poisson poisson @(.poisson istate)
            ^double stddev @(.tranStddev istate)]
        (set! prevspeaker nil) ; maybe refactor when-let, when below to make this the alt condition
        (when-let [^Indiv best-model (choose-most-successful 
                                       rng
                                       (into neighbors ;   (a) neighbors, (b) 0 or more random indivs from entire pop
                                             (choose-others-from-pop rng poisson this population)))]
          (when (> (getSuccess best-model) success)     ; is most successful other, better than me?
            (set! relig (add-noise rng stddev (getRelig best-model)))
            (set! prevspeaker best-model)))))
  Steppable
    ;; Note that by maintaining only a single version of vars, and allowing each indiv to be stepped in random order, we allow per-tick path dependencies.
    (step [this sim-state] 
      (let [^intermit.Sim sim sim-state  ; kludge to cast to my class--can't put it in signature
            ^intermit.Sim.InstanceState istate (.instanceState sim)
            population @(.population istate)]
            ;^MersenneTwisterFast rng (.random sim)
            ;pop-bag (sim.util.Bag. ^Collection population)] ; THIS is where most of the order-of-magnitude hit comes
        ;(.shuffle pop-bag rng)                                         ; CONSIDER using a Clojure shuffle routine, or use Bags more of the time to avoid conversion
        ;(copy-relig! this sim (vec pop-bag))))
        ;(copy-relig! this sim (shuffle population)))) ; JUST AS slow as the Bag version. Clojure shuffle throws coll into an ArrayList and then calls java.util.Collections/shuffle on it.
       ;; This next version, without shuffling, makes entire program an order of magnitude faster, but is clearly incorrect:
       ;; With large Poisson mean, most of the speakers are in the last community.  Horrible.
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

;; TODO note this means that e.g. if indiv is isolated, then when it happens to get high relig, it will also have high success.  Is that realistic?
(defn calc-success
  "Returns the average of relig values of a collection of indivs and
  one more indiv, who has relig value init-value.  i.e. the sum of all
  these values is divided by (count indivs) + 1."
  ^double [^double init-value indivs]
  (/ (sum-relig init-value indivs) 
     (inc (count indivs))))

(defn choose-others-from-pop
  "Randomly sample a Poisson-distributed number of indivs from population,
  excluding me.  (The mean for the Poisson distribution is stored in the
  poisson object.)"
  [^MersenneTwisterFast rng ^Poisson poisson me population]
  (let [size-wout-me (dec (count population)) ; we're sampling indivs *other* than me; there are count - 1 of them.
        rand-num (.nextInt poisson)
        num-to-choose (if (< rand-num size-wout-me) rand-num size-wout-me)] ; When Poisson mean is large, result may be larger than number of indivs.
    (let [res (sample-wout-repl-or-me rng num-to-choose me population)]
      res)))

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
  by identity (e.g. when defined by deftype rather than defrecord)."
  [^MersenneTwisterFast rng ^long num-samples me coll]
  (let [size (count coll)
        size-wout-me (dec size)] ; how many elements, excluding me?
    (cond (>= num-samples size) (throw (Exception. "num-samples is larger than coll size w/out 'me'"))
          (== num-samples size-wout-me) (remove #(identical? me %) coll)
          :else (loop [sample-set #{}]   ; num-samples < size - 1
                  (if (== (count sample-set) num-samples)
                    sample-set
                    (let [sample (nth coll (.nextInt rng size))]
                      (if (identical? me sample) ; if it's the one we don't want,
                        (recur sample-set)       ; lose it
                        (recur (conj sample-set sample)))))))))

;; Can I avoid repeated accesses to the same field, caching them?  Does it matter?
(defn choose-most-successful
  "Given a collection of Indiv's, returns the one with the greatest success, or
  nil if the collection is empty."
  ^Indiv [^MersenneTwisterFast rng models]
  (letfn [(compare-success 
            ([] nil) ; what reduce does if collection is empty
            ([^Indiv i1 ^Indiv i2]
             (let [^double success1 (getSuccess i1)
                   ^double success2 (getSuccess i2)]
               (cond (== success1 success2) (if (< ^double (.nextDouble rng) 0.5) ; a rare case, but we don't want ties' results to be path dependent.
                                              i1
                                              i2)
                     (> success1 success2) i1
                     :else i2))))]
    (reduce compare-success models)))

(defn add-noise
 "Add Normal noise with stddev to value, clipping to extrema 0.0 and 1.0."
  ^double [^MersenneTwisterFast rng ^double stddev ^double value]
  (max 0.0 (min 1.0 (+ value (* stddev ^double (.nextGaussian rng))))))

;;; Initialization functions:

(defn make-indiv
  "Make an indiv with appropriate defaults."
  [sim-state]
  (Indiv.
    (str (gensym "i")) ; id
    (.nextDouble (.random sim-state))  ; success
    (.nextDouble (.random sim-state))  ; relig
    []   ; neighbors
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

(def link-styles {"binomial" binomial-link-indivs!
                  "sequential" sequential-link-indivs!})

(defn link-indivs!
  [keystr rng prob indivs]
  ((link-styles keystr) rng prob indivs))

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
        link-style @(.linkStyle (.instanceState sim))]
    (link-indivs! link-style rng @(.linkProb (.instanceState sim)) indivs)
    (Community. (str (gensym "c")) indivs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: reset of class for overall system
(defn -main
  [& args]
  (sim.engine.SimState/doLoop ^SimState intermit.Sim (into-array String args))
  (System/exit 0))

;; doall all sequences below.  They're short, so there's no point in waiting for
;; them to get realized who knows where/when, given that the program has mutable state.
(defn -start
  "Function called to (re)start a new simulation run.  Initializes a new
  set of communities, each with a new set of community members."
  [^Sim this]
  (.superStart this)
  (let [^Schedule schedule (.schedule this)
        ^InstanceState instance-state (.instanceState this)
        num-communities  @(.numCommunities instance-state)
        indivs-per-community @(.meanIndivsPerCommunity instance-state)
        communities (vec (repeatedly num-communities
                                     #(make-community-of-indivs this indivs-per-community)))
        population (vec (mapcat get-members communities))
        meanReligSeriesAtom (.meanReligSeries instance-state)]
    ;; set up core simulation structures (the stuff that runs even in headless mode)
    (reset! (.poisson instance-state) (Poisson. @(.globalInterlocMean instance-state) (.random this)))
    (reset! (.communities instance-state) communities)
    (reset! (.population instance-state) population)
    (reset! meanReligSeriesAtom [])
    (doseq [indiv population] (.scheduleRepeating schedule Schedule/EPOCH 0 indiv))  ; indivs' step fns run first to communicate relig
    (.scheduleRepeating schedule Schedule/EPOCH 1            ; then update success fields afterwards
                        (reify Steppable 
                          (step [this sim-state]
                            (doseq [^Indiv indiv population] (update-success! indiv sim-state))
                            (swap! meanReligSeriesAtom
                                   conj 
                                   (Double2D.
                                     (double (.getSteps schedule)) ; coercion will happen automatically; I made it explicit. getTime incorrect if funny scheduling.
                                     (/ (sum-relig 0.0 population)
                                        (count population)))))))))
;; for Double2D version of mean relig series, use "(sim.util.Double2D. ...)".
;; for vector version of mean relig series, use "[...]".

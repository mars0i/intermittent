;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

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
  (:import ;[sim.field.continuous Continuous2D]
           ;[sim.field.network Network Edge]
           ;[sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [sim.util.distribution Poisson]
           [ec.util MersenneTwisterFast]
           [java.lang String]
           ;[clojure.lang PersistentVector]
           [intermit Sim]) ; import rest of classes after each is defined
  (:gen-class :name intermit.Sim
              :extends sim.engine.SimState                         ; includes signature for the start() method
              :exposes-methods {start superStart}                  ; alias method start() in superclass. (Don't name it 'super-start'; use a Java name.)
              :methods [[getNumCommunities [] long]                ; these methods are defined much further down
                        [setNumCommunities [long] void]
                        [getTargetIndivsPerCommunity [] long]
                        [setTargetIndivsPerCommunity [long] void]
                        [getLinkProb [] double]
                        [setLinkProb [double] void]
                        [getNoiseStddev [] double]
                        [setNoiseStddev [double] void]
                        [getPoissonMean [] double]
                        [setPoissonMean [double] void]]
              :state instanceState
              :init init-instance-state
              :main true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS AND UTILITY CODE

(def initial-num-communities 10)
(def initial-mean-indivs-per-community 10)
(def initial-link-prob 0.3)
(def initial-noise-stddev 0.2)
(def initial-poisson-mean 1)

;(defn but-nth
; "Returns a lazy sequence like coll, but with the nth item removed."
;  [coll n]
;  (concat (take n coll) (drop (inc n) coll)))

(defn but-nth-vec
  "Given a vector v, returns a vector that's the same except that
  element idx of the original vector is absent."
  [v ^long idx]
  (into (subvec v 0 idx)
        (subvec v (inc idx))))

;; My ad-hoc version
;; INCREDIBLY SLOW.  IS INCANTER VERSION FASTER?  WHAT OTHERS ARE AVAILABLE?
;(defn sample-wout-repl
;  "Given a random number generator, a number of samples, and a vector, returns
;  a vector of num-samples random samples without replacement from vector."
;  [^MersenneTwisterFast rng ^long num-samples v]
;  (letfn [(sample-it [^long samples-remaining ^clojure.lang.PersistentVector remaining ^clojure.lang.PersistentVector acc]
;            (if (<= samples-remaining 0)
;              acc
;              (let [idx (.nextInt rng (count remaining))]
;                (recur (dec samples-remaining)
;                       (but-nth-vec remaining idx)
;                       (conj acc (nth remaining idx))))))]
;    (sample-it num-samples v [])))


;; TRY COLT VERSION cern.jet.random.sampling Class RandomSampler
;; https://dst.lbl.gov/ACSSoftware/colt/api/cern/jet/random/sampling/RandomSampler.html

;; Incanter-derived version
;; Twice as fast as my ad-hoc version, but still very slow.
;; DOES INCANTER HAVE A BETTER ONE NOW??
(defn sample-wout-repl
  "Derived from Incanter's algorithm from sample-uniform for sampling without replacement."
  [^MersenneTwisterFast rng ^long num-samples v]
  (let [size (count v)
        max-idx size]
    (cond
      (= num-samples 1) (vector (nth v (.nextInt rng size)))  ; if only one element needed, don't bother with the "with replacement" algorithm
      ;; Rather than creating subseqs of the original v, we create a seq of indices below,
      ;; and then [in effect] map (partial nth v) through the indices to get the samples that correspond to them.
      (< num-samples size) (mapv #(nth v %) 
                                 (loop [samp-indices [] indices-set #{}]    ; loop to create the set of indices
                                   (if (= (count samp-indices) num-samples) ; until we've collected the right number of indices
                                     samp-indices
                                     (let [i (.nextInt rng size)]             ; get a random index
                                       (if (contains? indices-set i)      ; if we've already seen that index,
                                         (recur samp-indices indices-set) ;  then try again
                                         (recur (conj samp-indices i) (conj indices-set i))))))) ; otherwise add it to our indices
      :else (throw (Exception. "num-samples can't be larger than (count v).")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROTOCOLS/INTERFACES

;; Separating these allows future versions in which e.g. communities can communicate,
;; and allows a unified interface for finding out average culture of a community
;; cultural values of indivs, etc.

;; This can be applied to communities as well as individuals.
(defprotocol CulturedP
  "Protocol for things that can have culture."
  (getRelig [this])             ;; UI-available methods
  (getSuccess [this])
  (update-success! [this])) ;; not available to UI

(defprotocol CommunicatorP
  "Protocol for things that can communicate culture."
  (copy-relig! [this sim population]))

(defprotocol CommunityP
  "Protocol for communities."
  (getMembers [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTANCESTATE
;; Used to hold mutable data in Sim's instanceState variable
;; Need def here so we can type-hint Indiv's methods

;; Note some of these have to be atoms so that that we can allow restarting with a different setup.
(deftype InstanceState [numCommunities          ; number of communities
                        meanIndivsPerCommunity  ; mean or exact number of indivs in each
                        linkProb
                        noiseStddev
                        poissonMean
                        communities             ; holds the communities
                        population              ; holds all individuals
                        poisson])

(import [intermit.Sim InstanceState])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDIV: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

(defn choose-relig
  "Given a relig value and a sequence of CulturedP's, compares the relig
  value with the relig values of the CulturedP's, returning the largest one."
  [my-relig others]
  (reduce #(max %1 (getRelig %2))
          my-relig others))

(defn copy-best-relig
  "Choose the best, accurately perceived relig value of others if better than
  my-relig, add Normally distributed noise (with mean zero and standard deviation
  stddev) to the result, and return the sum."
  [^MersenneTwisterFast rng stddev my-relig others]
  (+ (* stddev (.nextGaussian rng))
     (choose-relig my-relig others)))

(defn choose-from-pop
  [^MersenneTwisterFast rng ^Poisson poisson rest-of-pop]
  (let [num-to-choose (.nextInt poisson)]
    (sample-wout-repl rng num-to-choose rest-of-pop)))

(deftype Indiv [id success relig neighbors popIdx] ; should neighbor relations be in the community instead? nah.
  CulturedP
  (getRelig [this] @relig)
  (getSuccess [this] @success)
  (update-success! [this] "TODO") ; TODO
  CommunicatorP
  (copy-relig! [this sim-state population]
    (let [^Sim sim sim-state
          ^InstanceState istate (.instanceState sim)
          ^MersenneTwister rng (.random sim)
          ^Poisson poisson @(.poisson istate)
          ^double noise-stddev @(.noiseStddev istate)]
      (when-let [models (not-empty (into @neighbors
                                         (choose-from-pop rng poisson (but-nth-vec population @popIdx))))]
        (swap! relig (partial copy-best-relig rng noise-stddev)
               models))))
  Steppable
  (step [this sim-state] 
    (let [^intermit.Sim sim sim-state  ; kludge to cast to my class--can't put it in signature
          ^intermit.Sim.InstanceState istate (.instanceState sim)]
      (copy-relig! this sim @(.population istate))))
  Object
  (toString [this] (str id ": " @relig " " @success " " (vec (map #(.id %) @neighbors)))))

(import [intermit.Sim Indiv])

(defn make-indiv
  "Make an indiv with appropriate defaults."
  [sim-state]
  (Indiv.
    (str (gensym "indiv"))
    (atom (.nextDouble (.random sim-state)))  ; relig
    (atom (.nextDouble (.random sim-state)))  ; success
    (atom []) ;  Need atom for inititialization stages, though won't change after that.
    (atom 0))) ; temp value

;; Erdos-Renyi network linking (I think)
(defn erdos-renyi-link-indivs!
  "For each pair of indivs, with probability mean-links-per-indiv / indivs,
  make them each others' neighbors.  Set the former to be equal to the latter
  to link everything to everything."
  [rng prob indivs]
  (doseq [i (range (count indivs))
          j (range i)          ; lower triangle without diagonal
          :when (< (.nextDouble rng) prob)]
    (swap! (.neighbors (nth indivs i)) conj (nth indivs j))
    (swap! (.neighbors (nth indivs j)) conj (nth indivs i)))
  indivs)

;; poss define other linkers here
;; Useful info:
;; http://www.drdobbs.com/architecture-and-design/simulating-small-world-networks/184405611
;; https://compuzzle.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/
;; https://codepretty.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMUNITY: class for collections of Indivs or collections of Communities.

;; TODO ADD ID
(deftype Community [id members]
  CommunityP
  (getMembers [this] members)
  CulturedP
  (getRelig [this] 0.5)    ; TODO summary of members' values
  (getSuccess [this] 0.5) ; TODO summary of members' values
  Object
  (toString [this] (str id ": " (vec (map #(.id %) @members)))))

(import [intermit.Sim Community])

(defn make-community-of-indivs
  "Make a community with size number of indivs in it."
  [sim size]
  (let [indivs  (doall (repeatedly size #(make-indiv sim)))] ; it's short; don't wait for late-realization bugs.
    (erdos-renyi-link-indivs! (.random sim) @(.linkProb (.instanceState sim)) indivs) 
    (Community. (str (gensym "comm")) indivs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: class for overall system


;; TODO: How can I allow user to choose the link function?

(defn -init-instance-state
  "Initializes instance-state when an instance of class Sim is created."
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom initial-link-prob)
                          (atom initial-noise-stddev)
                          (atom initial-poisson-mean)
                          (atom nil)
                          (atom nil)
                          (atom nil))])


;(defn gitInstanceState ^intermit.Sim.InstanceState [^intermit.Sim this] (.instanceState this)) ; wrapper for the sake of type hinting, doesn't help, though.

;; Only used for (re-)initialization; no need to type hint:
(defn -getNumCommunities ^long [^Sim this] @(.numCommunities (.instanceState this)))
(defn -setNumCommunities [^Sim this ^long newval] (reset! (.numCommunities (.instanceState this)) newval))
(defn -getTargetIndivsPerCommunity ^long [^Sim this] @(.meanIndivsPerCommunity (.instanceState this)))
(defn -setTargetIndivsPerCommunity [^Sim this ^long newval] (reset! (.meanIndivsPerCommunity (.instanceState this)) newval))
(defn -getLinkProb ^double [^Sim this] @(.linkProb (.instanceState this)))
(defn -setLinkProb [^Sim this ^double newval] (reset! (.linkProb (.instanceState this)) newval))
(defn -getNoiseStddev ^double [^Sim this] @(.noiseStddev (.instanceState this)))
(defn -setNoiseStddev [^Sim this ^double newval] (reset! (.noiseStddev (.instanceState this)) newval))
(defn -getPoissonMean ^double [^Sim this] @(.poissonMean (.instanceState this)))
(defn -setPoissonMean [^Sim this ^double newval] 
  (reset! (.poissonMean (.instanceState this) newval)) ; store it so that UI can display its current value
  (.setMean (.poisson (.instanceState this)) newval))  ; allows changing value during the middle of a run.

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.Sim (into-array String args))
  (System/exit 0))

;; doall all sequences below.  They're short, so there's no point in waiting for
;; them to get realized who knows where/when, given that the program has mutable state.
(defn -start
  "Function called to (re)start a new simulation run.  Initializes a new
  set of communities, each with a new set of community members."
  [this]
  (.superStart this)
  (let [schedule (.schedule this)
        instance-state (.instanceState this)
        num-communities  @(.numCommunities instance-state)
        indivs-per-community @(.meanIndivsPerCommunity instance-state)
        communities (vec (repeatedly num-communities
                                       #(make-community-of-indivs this indivs-per-community)))
        population (vec (mapcat getMembers communities))]
    (reset! (.poisson instance-state) (Poisson. @(.poissonMean instance-state) (.random this)))
    (reset! (.communities instance-state) communities)
    (reset! (.population instance-state) population)
    (dotimes [idx (count population)]   ; need to store indexes in indivs
      (let [indiv (nth population idx)] ;  so go old school
        (reset! (.popIdx indiv) idx)
        (.scheduleRepeating schedule indiv)))))

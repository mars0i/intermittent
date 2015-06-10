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
  (:require [intermit.utils :as u])
  (:import [sim.field.continuous Continuous2D]
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
                        [getMeanIndivsPerCommunity [] long]
                        [setMeanIndivsPerCommunity [long] void]
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
;; DEFAULTS AND GENERAL UTILITY CODE

(def initial-num-communities 12) ; use something that factors into x and y dimensions
(def initial-mean-indivs-per-community 10)
(def initial-link-prob 0.3)
(def initial-noise-stddev 0.02)
(def initial-poisson-mean 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROTOCOLS/INTERFACES

;; Separating these allows future versions in which e.g. communities can communicate,
;; and allows a unified interface for finding out average culture of a community
;; cultural values of indivs, etc.

;; This can be applied to communities as well as individuals.
(defprotocol CulturedP
  "Protocol for things that can have culture."
  (getRelig [this])             ;; UI-available methods
  (getSuccess [this]))

;; Q: Why is this in a protocol?  A: So that communities can do it to, if desired.
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
(deftype InstanceState [space ; a Continuous2D--needed for display only.
                        numCommunities          ; number of communities
                        meanIndivsPerCommunity  ; mean or exact number of indivs in each
                        linkProb
                        noiseStddev
                        poissonMean
                        communities             ; holds the communities
                        population              ; holds all individuals
                        poisson])

;(import [intermit.Sim InstanceState])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDIV: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

(declare sample-wout-repl-or-me choose-others-from-pop choose-most-successful add-tran-noise avg-relig update-fields!)

(deftype Indiv [id success relig neighbors]
  CulturedP
    (getRelig [this] @relig)
    (getSuccess [this] @success)
  CommunicatorP
    (copy-relig! [this sim-state population]
      (let [^Sim sim sim-state ; can't type hint ^Sim in the parameter list
            ^MersenneTwisterFast rng (.random sim)
            ^InstanceState istate (.instanceState sim)
            ^Poisson poisson @(.poisson istate)
            ^double noise-stddev @(.noiseStddev istate)]
        (when-let [^Indiv best-model (choose-most-successful 
                                       rng
                                       (into @(.neighbors this) ;   (a) neighbors, (b) 0 or more random indivs from entire pop
                                             (choose-others-from-pop rng poisson this population)))]
          (when (> @(.success best-model) @success)     ; is most successful other, better than me?
            (reset! relig (add-tran-noise rng noise-stddev @(.relig best-model)))))))
  Steppable
    ;; Note that by maintaining only a single version of vars, and allowing each indiv to be stepped in random order, we allow per-tick path dependencies.
    (step [this sim-state] 
      (let [^intermit.Sim sim sim-state  ; kludge to cast to my class--can't put it in signature
            ^intermit.Sim.InstanceState istate (.instanceState sim)]
        (copy-relig! this sim @(.population istate))))
  Object
    (toString [this] (str id ": " @success " " @relig " " (vec (map #(.id %) @neighbors)))))

;(import [intermit.Sim Indiv])

;;; Runtime functions:

(defn choose-others-from-pop
  "Randomly sample a Poisson-distributed number of indivs from population,
  excluding me.  (The mean for the Poisson distribution is stored in the
  poisson object.)"
  [^MersenneTwisterFast rng ^Poisson poisson me population]
  (let [num-to-choose (.nextInt poisson)]
    (sample-wout-repl-or-me rng num-to-choose me population)))

;; It's much faster to remove the originating Indiv from samples here,
;; rather than removing it from the collection to be sampled, at least
;; for reasonably large populations.
(defn sample-wout-repl-or-me
  "Special sample without replacement function:
  Returns num-samples samples from coll without replacement, excluding 
  items identical? to me.  Returns a vector or a set; if you want something
  more specific, it's the caller's responsibility.  Should be fastest when
  coll has fast index access (e.g. it's a vector) and when the elements hash
  by identity (e.g. when defined by deftype rather than defrecord)."
  [^MersenneTwisterFast rng ^long num-samples me coll]
  (let [size (count coll)]
    (loop [sample-set #{}]
      (if (== (count sample-set) num-samples)
        sample-set
        (let [sample (nth coll (.nextInt rng size))]
          (if (identical? me sample) ; if it's the one we don't want,
            (recur sample-set)       ; lose it
            (recur (conj sample-set sample))))))))

;; Can I avoid repeated accesses to the same field, caching them?  Does it matter?
(defn choose-most-successful
  "Given a collection of Indiv's, returns the one with the greatest success, or
  nil if the collection is empty."
  ^Indiv [^MersenneTwisterFast rng models]
  (letfn [(compare-success 
            ([] nil) ; what reduce does if collection is empty
            ([^Indiv i1 ^Indiv i2] (let [^double success1 @(.success i1)
                                         ^double success2 @(.success i2)]
                                     (cond (== success1 success2) (if (< (.nextDouble rng) 0.5) i1 i2) ; a rare case, but we don't ties' results to be path dependent.
                                           (> success1 success2) i1
                                           :else i2))))]
          (reduce compare-success models)))

(defn add-tran-noise
 "Add Normal noise with stddev to relig, clipping to extrema 0.0 and 1.0."
  ^double [^MersenneTwisterFast rng ^double stddev ^double relig]
  (max 0.0 (min 1.0
                (+ relig
                   (* stddev ^double (.nextGaussian rng))))))


;;; Initialization functions:

(defn make-indiv
  "Make an indiv with appropriate defaults."
  [sim-state]
  (Indiv.
    (str (gensym "i")) ; id
    (atom (.nextDouble (.random sim-state)))  ; success
    (atom (.nextDouble (.random sim-state)))  ; relig
    (atom []))) ;  neighbors (need atom for inititialization stages, though won't change after that)

(defn binomial-link-indivs!
  "For each pair of indivs, with probability prob, make them eachothers' neighbors.
  Set prob to 1 to link all indivs to each other.  (This is a 'binomial' [edge
  dist], 'Poisson' [degree dist], or 'Erdös-Rényi' random graph.)"
  [rng prob indivs]
  (doseq [i (range (count indivs))
          j (range i)          ; lower triangle without diagonal
          :when (< (.nextDouble rng) prob)]
    (swap! (.neighbors (nth indivs i)) conj (nth indivs j))
    (swap! (.neighbors (nth indivs j)) conj (nth indivs i)))
  indivs)

;; poss define other linkers here
;; http://www.drdobbs.com/architecture-and-design/simulating-small-world-networks/184405611
;; https://compuzzle.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/
;; https://codepretty.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMUNITY: class for collections of Indivs or collections of Communities.

(deftype Community [id members] ; id and members are regular data; success is an atom
  CommunityP
    (getMembers [this] members)
  Steppable
    (step [this sim-state]
      ;(println "=======") (doseq [indiv members] (println indiv)) ; DEBUG
      ;; Success comes from neighbors in this model. (if from entire community, there's no way to choose between neighbors.)
      ;; It's lots faster to update indivs' success here than in Indiv's step fn,
      ;; and it removes relig->success path dependence: When Communities are stepped,
      ;; relig values for all of the indivs have already been determined for this timestep.
      (doseq [^Indiv indiv members] 
        (swap! (.success indiv) #(avg-relig %1 @(.neighbors indiv)))))
  Object
    (toString [this] (str id ": " (vec (map #(.id %) members)))))

;(import [intermit.Sim Community])

;;; Runtime functions:

(defn avg-relig
  "Returns the average of relig values of a collection of indivs and
  the given relig value."
  ^double [^double relig indivs]
  (let [size (inc (count indivs))
        add-success (fn [^double acc ^Indiv indiv] (+ acc ^double @(.relig indiv)))]
      (/ (reduce add-success relig indivs) size)))

;;; Initialization functions:

(defn make-community-of-indivs
  "Make a community with size number of indivs in it."
  [sim size]
  (let [indivs  (vec (repeatedly size #(make-indiv sim)))] ; it's short; don't wait for late-realization bugs.
    (binomial-link-indivs! (.random sim) @(.linkProb (.instanceState sim)) indivs) 
    (Community. (str (gensym "c")) indivs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: class for overall system

(defn -init-instance-state
  "Initializes instance-state when an instance of class Sim is created."
  [seed]
  [[seed] (InstanceState. (Continuous2D. 1.0 400 300)    ; space
                          (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom initial-link-prob)
                          (atom initial-noise-stddev)
                          (atom initial-poisson-mean)
                          (atom nil)
                          (atom nil)
                          (atom nil))])

(defn -getNumCommunities ^long [^Sim this] @(.numCommunities (.instanceState this)))
(defn -setNumCommunities [^Sim this ^long newval] (reset! (.numCommunities (.instanceState this)) newval))
(defn -getMeanIndivsPerCommunity ^long [^Sim this] @(.meanIndivsPerCommunity (.instanceState this)))
(defn -setMeanIndivsPerCommunity [^Sim this ^long newval] (reset! (.meanIndivsPerCommunity (.instanceState this)) newval))
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
    ;; set up core simulation structures (the stuff that runs even in headless mode)
    ;(println (map #(identity @(.relig %)) population))
    (reset! (.poisson instance-state) (Poisson. @(.poissonMean instance-state) (.random this)))
    (reset! (.communities instance-state) communities)
    (reset! (.population instance-state) population)
    (doseq [indiv population]
      (.scheduleRepeating schedule Schedule/EPOCH 0 indiv))       ; indivs run first
    (doseq [community communities]
      (.scheduleRepeating schedule Schedule/EPOCH 1 community)) ; then communities run
    ;; non-graphical data structures needed for graphics:
    ;; graphics data structures:
  ))

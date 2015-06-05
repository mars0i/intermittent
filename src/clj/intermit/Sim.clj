(ns intermit.Sim
  (:import ;[sim.field.continuous Continuous2D]
           ;[sim.field.network Network Edge]
           ;[sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

;(set! *warn-on-reflection* true)

;; Tip: Methods named "getBlahBlah" or "setBlahBlah" will be found by the UI via reflection.

;; IN THIS VERSION:
;; * There is a step function in each agent, i.e. Indiv implements Steppable.
;; * The scheduler calls the agents' (indivs') step functions in random order on each timestep.
;; * Indivs update their states sequentially in this random order, rather than updating in
;;   parallel by updating a "new" version of a variable from others "old" versions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS AND UTILITY CODE

(def initial-num-communities 10)
(def initial-mean-indivs-per-community 10)

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
  (copy-relig! [this rng population]))

(defprotocol CommunityP
  "Protocol for communities."
  (getMembers [this]))

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

(defn add-gaussian
  "Given an random number generator rng, adds to value a random number
  that's Normally distributed with mean zero and standard deviation stddev."
  [^MersenneTwisterFast rng stddev value]
  (+ (* stddev (.nextGaussian rng)) value))

;; TODO ADD ID
(deftype Indiv [success relig neighbors] ; should neighbor relations be in the community instead? nah.
  CulturedP
  (getRelig [this] @relig)
  (getSuccess [this] @success)
  (update-success! [this] "TODO") ; TODO
  CommunicatorP
  (copy-relig! [this rng population]  
    (when-let [models @neighbors] ; TODO ADD RANDOM MEMBERS OF POPULATION
      (swap! relig (comp (partial add-gaussian rng 0.2) ; TODO 0.2 is temporary--allow user to control
                         choose-relig)  ; choose the best (accurately-perceived) relig value among neighbors, etc., and then noisily copy it.
             models)))
  Steppable
  (step [this sim-state] 
    (let [sim sim-state ; kludge to cast to my class--can't put it in signature
          rng (.random sim)
          istate (.instanceState sim)
          population (.indivs istate)]
      (copy-relig! this rng population)
      ;(println @relig @success (count @neighbors)) ; DEBUG
    )))

(import [intermit.Sim Indiv])

(defn make-indiv
  "Make an indiv with appropriate defaults values."
  [sim-state]
  (Indiv.
    (atom (.nextDouble (.random sim-state)))  ; relig
    (atom (.nextDouble (.random sim-state)))  ; success
    (atom nil))) ; falsey nil, atom for init process even though the links don't change

;; Erdos-Renyi network linking (I think)
(defn erdos-renyi-link-indivs!
  "For each pair of indivs, with probability mean-links-per-indiv / indivs,
  make them each others' neighbors.  Set the former to be equal to the latter
  to link everything to everything."
  [rng mean-links-per-indiv indivs]
  (let [num-indivs (count indivs)
        mean (/ mean-links-per-indiv num-indivs)]
    (doseq [i (range num-indivs)
            j (range i)          ; lower triangle without diagonal
            :when (< (.nextDouble rng) mean)]
      (swap! (.neighbors (nth indivs i)) conj (nth indivs j))
      (swap! (.neighbors (nth indivs j)) conj (nth indivs i))))
  indivs)

;; define other linkers here
;; Useful info:
;; http://www.drdobbs.com/architecture-and-design/simulating-small-world-networks/184405611
;; https://compuzzle.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/
;; https://codepretty.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMUNITY: class for collections of Indivs or collections of Communities.

;; TODO ADD ID
(deftype Community [members]
  CommunityP
  (getMembers [this] members)
  CulturedP
  (getRelig [this] 0.5)    ; TODO summary of members' values
  (getSuccess [this] 0.5)) ; TODO summary of members' values

(import [intermit.Sim Community])

(defn make-community-of-indivs
  "Make a community with size number of indivs in it."
  [sim-state size]
  (let [indivs  (doall (repeatedly size #(make-indiv sim-state)))] ; it's short; don't wait for late-realization bugs.
    (erdos-renyi-link-indivs! (.random sim-state) 3 indivs) ; TODO TEMPORARY
    (Community. indivs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: class for overall system

(gen-class :name intermit.Sim
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'; use a Java name.)
    :methods [[getNumCommunities [] long]
              [setNumCommunities [long] void]
              [getTargetIndivsPerCommunity [] long]
              [setTargetIndivsPerCommunity [long] void]
              [getLinkFns [] java.util.Collection]]
    :state instanceState
    :init init-instance-state
    :main true)


;; TODO: How can I allow user to choose the link function?

;; Note some of these have to be atoms so that that we can allow restarting with a different setup.
(deftype InstanceState [numCommunities          ; number of communities
                        meanIndivsPerCommunity  ; mean or exact number of indivs in each
                        communities             ; holds the communities
                        indivs])                  ; holds all individuals

(defn -init-instance-state
  "Initializes instance-state when an instance of class Sim is created."
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom [])
                          (atom []))])


;(defn gitInstanceState ^intermit.Sim.InstanceState [^intermit.Sim this] (.instanceState this)) ; wrapper for the sake of type hinting, doesn't help, though.

;; Only used for (re-)initialization; no need to type hint:
(defn -getNumCommunities [this] @(.numCommunities (.instanceState this)))
(defn -setNumCommunities [this newval] (reset! (.numCommunities (.instanceState this))))
(defn -getTargetIndivsPerCommunity [this] @(.meanIndivsPerCommunity (.instanceState this)))
(defn -setTargetIndivsPerCommunity [this newval] (reset! (.meanIndivsPerCommunity (.instanceState this))))
(defn -getLinkFns [this] (.linkFns this))

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
        communities (doall (repeatedly num-communities
                                       #(make-community-of-indivs this indivs-per-community)))
        indivs (doall (mapcat getMembers communities))]
    (reset! (.communities instance-state) communities)
    (reset! (.indivs instance-state) indivs)
    (doseq [indiv indivs] (.scheduleRepeating schedule indiv))))

(ns intermit.Sim
  (:import ;[sim.field.continuous Continuous2D]
           ;[sim.field.network Network Edge]
           ;[sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

;(set! *warn-on-reflection* true)

;; Tip: Methods named "getBlahBlah" or "setBlahBlah" will be found by the UI via reflection.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS

(def initial-num-communities 10)
(def initial-mean-indivs-per-community 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROTOCOLS/INTERFACES

;; Separating these allows future versions in which e.g. communities can communicate,
;; and allows a unified interface for finding out average culture of a community
;; cultural values of indivs, etc.

(defprotocol CulturedP
  (getRelig [this])             ;; UI-available methods
  (getSuccess [this])
  (update-success! [this])) ;; not available to UI

(defprotocol CommunicatorP
  (copy-relig! [this]))

(defprotocol CommunityP
  (getMembers [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDIV: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

(deftype Indiv [success relig neighbors] ; should neighbor relations be in the community instead? nah.
  CulturedP
  (getRelig [this] @relig)
  (getSuccess [this] @success)
  (update-success! [this] "TODO") ; TODO
  CommunicatorP
  (copy-relig! [this] "TODO") ; TODO
  Steppable
  (step [this sim-state] 
    (let [sim ^Sim sim-state] ; kludge to cast to my class--can't put it in signature
      (println "My relig is" @relig "and my success is" @success)
    )))

(import [intermit.Sim Indiv])

(defn make-indiv
  [sim-state]
  (Indiv.
    (atom (.nextDouble (.random sim-state)))  ; relig
    (atom (.nextDouble (.random sim-state)))  ; success
    nil)) ; TODO should this be an atom, so we can set it?  Or assoc it into a clone?  Can I do that with deftype?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMUNITY: class for collections of Indivs or collections of Communities.

(deftype Community [members]
  CommunityP
  (getMembers [this] members)
  CulturedP
  (getRelig [this] 0.5)    ; summary of members' values
  (getSuccess [this] 0.5)) ; TODOsummary of members' values

(import [intermit.Sim Community])

(defn make-community-of-indivs
  [sim-state size]
  (Community. (doall (repeatedly size #(make-indiv sim-state))))) ; seq is short; don't wait for late-realization bugs
;; TODO: Add neighbor relations.  Hmm.  Should it be in the community, rather in the indiv??

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sim: class for overall system

(gen-class :name intermit.Sim
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'; use a Java name.)
    :methods [[getNumCommunities [] long]
              [setNumCommunities [long] void]
              [getTargetIndivsPerCommunity [] long]
              [setTargetIndivsPerCommunity [long] void]]
    :state instanceState
    :init init-instance-state
    :main true)

(deftype InstanceState [numCommunities meanIndivsPerCommunity communities indivs])

(defn -init-instance-state
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-mean-indivs-per-community) 
                          (atom [])
                          (atom []))])

;; Only used for (re-)initialization; no need to type hint:
(defn -getNumCommunities [this] @(.numCommunities (.instanceState this)))
(defn -setNumCommunities [this newval] (reset! (.numCommunities (.instanceState this))))
(defn -getTargetIndivsPerCommunity [this] @(.meanIndivsPerCommunity (.instanceState this)))
(defn -setTargetIndivsPerCommunity [this newval] (reset! (.meanIndivsPerCommunity (.instanceState this))))

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.Sim (into-array String args))
  (System/exit 0))

;; doall all sequences below.  They're short, so there's no point in waiting for
;; them to get realized who knows where/when, given that the program has mutable state.
(defn -start
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

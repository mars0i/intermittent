(ns intermit.Sim
  (:import [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]
           [java.util Collection]))

;(set! *warn-on-reflection* true)

;; Tip: Methods named "getBlahBlah" or "setBlahBlah" will be found by the UI via reflection.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS

(def initial-num-communities 10)
(def initial-target-indivs-per-community 10)

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

(deftype Indiv [success relig]
  CulturedP
  (getRelig [this] @relig)
  (getSuccess [this] @success)
  (update-success! [this] "TODO") ; TODO
  CommunicatorP
  (copy-relig! [this] "TODO") ; TODO
  Steppable
  (step [this sim-state] 
    (let [population ^Population sim-state]   ; why don't I have to import Population to cast with it?
    ;; TODO
    )))

(import [intermit.Sim Indiv])

(defn make-indiv
  [sim-state]
  (Indiv.
    (atom (.nextDouble (.gitRandom sim-state)))   ; relig
    (atom (.nextDouble (.gitRandom sim-state))))) ; success

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
  (Community. (repeatedly #(make-indiv sim-state) size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Population: class for overall system

(gen-class :name intermit.Population
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'; use a Java name.)
    :exposes {random {:get gitRandom}, schedule {:get gitSchedule}}
    :methods [[getNumCommunities [] long]
              [setNumCommunities [long] void]
              [getTargetIndivsPerCommunity [] long]
              [setTargetIndivsPerCommunity [long] void]]
    :state instanceState
    :init init-istate
    :main true) 

(deftype InstanceState [numCommunities           ; how many second-level communities
                        targetIndivsPerCommunity ; how many indivs on average per community
                        communities
                        indivs])  ; indivs in all communities

(defn -init-instance-state
  [seed]
  [[seed] (InstanceState. (atom initial-num-communities)
                          (atom initial-target-indivs-per-community) 
                          (atom [])
                          (atom []))])

;; Only used for (re-)initialization; no need to type hint:
(defn -getNumCommunities [this] @(.numCommunities (.instanceState this)))
(defn -setNumCommunities [this newval] (reset! (.numCommunities (.instanceState this))))
(defn -getTargetIndivsPerCommunity [this] @(.targetIndivsPerCommunity (.instanceState this)))
(defn -setTargetIndivsPerCommunity [this newval] (reset! (.targetIndivsPerCommunity (.instanceState this))))

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.Population (into-array String args))
  (System/exit 0))

(defn -start
  [this]
  (.superStart this)
  (let [instance-state (.instanceState this)
        num-communities  (.numCommunities instance-state)
        indivs-per-community (.targetIndivsPerCommunity instance-state)
        communities (repeatedly #(make-community-of-indivs this indivs-per-community)
                                num-communities)
        indivs (mapcat getMembers communities)]
    (reset! (.communities instance-state) communities)
    (reset! (.indivs instance-state) indivs)))

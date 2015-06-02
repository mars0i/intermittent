(ns intermit.Sim
  (:import [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]
           [java.util Collection]))

;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indiv: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

;; Methods named "get_" or "set_" will be found by the UI via reflection.

(defprotocol IndivMeths
  ;; UI-available methods:
  (getRelig [this])
  (getSuccess [this])
  ;; Other methods:
  (set-relig! [this newval])
  (set-success! [this newval]))

(deftype Indiv [success relig]
  Steppable
  (step [this sim-state] 
    (let [population ^Population sim-state]   ; why don't I have to import Population to cast with it?
    ;; do stuff here
    ))
  IndivMeths
  (getRelig [this] @relig)
  (getSuccess [this] @relig)
  (set-relig! [this newval] (reset! success newval))
  (set-success! [this newval] (reset! success newval)))

(import [intermit.Sim Indiv])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Community: class for collections of Indivs into communities.

(defprotocol CommunityMeths
  ;; UI-available methods:
  (getAvgRelig [this])    ; some kind of average value
  (getAvgSuccess [this])  ; some kind of average value
  ;; Other methods:
  (add-members! [this new-members]))

(deftype Community [members]
  CommunityMeths
  (getAvgRelig [this] 0.5)   ; TODO
  (getAvgSuccess [this] 0.5)) ; TODO

(import [intermit.Sim Community])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Population: class for overall system
;; Extends SimState.

(gen-class :name intermit.Population
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'. Use a Java name.)
    :exposes {random {:get gitRandom}, schedule {:get gitSchedule}}
    :methods [[getNumCommunities [] long]
              [setNumCommunities [long] void]
              [getTargetNumIndivsPerCommunity [] long]
              [setTargetIndivsPerCommunity [long] void]
              [getCommunities [] java.util.Collection]
              [getIndivis [] java.util.Collection]]
    :state iState
    :init init-istate
    :main true) 

(deftype IState [numCommunities targetIndivsPerCommunity communities indivs])
(defn -init-instance-state [seed] [[seed] (IState. (atom 10) (atom 10) (atom []) (atom []))])
(defn -getNumCommunities [this] @(.numCommunities (.iState this)))
(defn -setNumCommunities [this newval] (reset! (.numCommunities (.iState this)) newval))
(defn -getTargetIndivsPerCommunity [this] @(.targetIndivsPerCommunity (.iState this)))
(defn -setTargetIndivsPerCommunity [this newval] (reset! (.targetIndivsPerCommunity (.iState this)) newval))
(defn -getCommunities [this] @(.communities (.iState this)))
(defn -getIndivs [this] @(.indivs (.iState this)))

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.Population (into-array String args))
  (System/exit 0))

(defn -start
  [this]
  (.superStart this)
  (let [istate (.iState this)]
  ;; make m communities of (average) size n
  ))

(defn make-indiv
  [this]
  (Indiv.
    (atom (.nextDouble (.gitRandom this)))   ; relig
    (atom (.nextDouble (.gitRandom this))))) ; success

(defn make-community
  [this size]
  (let [members (repeatedly #(make-indiv this) size)
        community (Community. (atom members))]
    (swap! (.indivs this) concat members))) ; add members to entire population

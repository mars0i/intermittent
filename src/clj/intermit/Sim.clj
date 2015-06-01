(ns intermit.Sim
  (:import [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indiv: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

;; Note that naming methods get_ or set_ means that the GUI will find them and
;; allow inspection or setting of them in the UI.
(defprotocol IndivMeths
  (getRelig [this])
  (set-relig! [this newval])
  (getSuccess [this])
  (set-success! [this newval]))

(deftype Indiv [success relig]
  Steppable
  (step [this sim-state] 
    (let [population ^Population sim-state]   ; why don't I have to import Population to cast with it?
    ;; do stuff here
    ))
  IndivMeths
  (getRelig [this] @relig)
  (set-relig! [this newval] (reset! success newval))
  (getSuccess [this] @relig)
  (set-success! [this newval] (reset! success newval)))

(import [intermit.Sim Indiv])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Population: class for overall system
;; Extends SimState.

(gen-class :name intermit.Population
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'. Use a Java name.)
    :exposes {random {:get gitRandom}, schedule {:get gitSchedule}}
    :methods []
    :state iState
    :init init-istate
    :main true) 

;; example
(deftype IState [indivs])
(defn -init-instance-state [seed] [[seed] (IState. (atom []))])
;(defn -getX [^Population this] @(:x (.IState this)))

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.Population (into-array String args))
  (System/exit 0))

(defn -start
  [this]
  (.superStart this)
  ;; make m communities of (average) size n
  )

(defn make-indiv
  [this]
  (Indiv.
    (atom (.nextDouble (.gitRandom this)))   ; relig
    (atom (.nextDouble (.gitRandom this))))) ; success

(defn make-community
  [this size]
  (let [members (repeatedly #(make-indiv this) size)]
    ;; add members to community here
    (swap! (.indivs this) concat members))) ; add members to entire population

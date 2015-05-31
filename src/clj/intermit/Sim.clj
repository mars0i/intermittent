(ns intermit.Sim
  (:import [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World: class for overall system
;; Extends SimState.

(gen-class :name intermit.World
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
(defn -getX [^World this] @(:x (.IState this)))

(defn -main
  [& args]
  (sim.engine.SimState/doLoop intermit.World (into-array String args))
  (System/exit 0))

(defn -start
  [^World this]
  (.superStart this)
  ;; do stuff here
  )

(defn make-indiv
  [this]
  (Indiv.
    (atom (.nextDouble (.gitRandom this)))))

(defn make-community
  [this num-members]
  (swap! (.indivs this)
         concat
         (repeatedly #(make-indiv this) num-members)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indiv: class for individuals who communicate with each other.
;; These could be persons, villages, subaks, etc.
;; Initial version implements Steppable.

;; Note that naming methods get_ or set_ means that the GUI will find them and
;; allow inspection or setting of them in the UI.
(defprotocol IndivMeths
  (getRelig [this])
  (set-relig! [this newval])
  (getSucc [this])
  (set-succ! [this newval]))

(deftype Indiv [succ relig]
  Steppable
  (step [this sim-state] 
    (let [world ^World sim-state]
    ;; do stuff here
    ))
  IndivMeths
  (getRelig [this] @relig)
  (set-relig! [this newval] (reset! succ newval))
  (getSucc [this] @relig)
  (set-succ! [this newval] (reset! succ newval)))

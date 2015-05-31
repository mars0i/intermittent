(ns intermit.Sim
  (:import [intermit World]
           [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(gen-class :name intermit.World
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'. Use a Java name.)
    :exposes {random {:get gitRandom}, schedule {:get gitSchedule}}
    :methods []
    :state iState
    :init init-istate
    :main true) 

;; example
(deftype IState [x])
(defn -init-instance-state [seed] [[seed] (IState. (atom 0.0))])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IndivMeth ...)

(deftype Indiv [x]
  Steppable
  (step [this state] 
    ;; do stuff here
    )
  IndivMeths
  ;; add stuff here
  )

(defn make-indiv [] (Indiv. (atom 0.0)))

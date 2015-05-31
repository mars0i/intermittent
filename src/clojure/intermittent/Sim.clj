(ns intermittent.Sim
  (:import [intermittent World]
           [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.util Double2D MutableDouble2D Interval]
           [sim.engine Steppable Schedule]
           [ec.util MersenneTwisterFast]))

(gen-class :name intermittent.Sim
    :extends sim.engine.SimState  ; includes signature for the start() method
    :exposes-methods {start superStart} ; alias method start() in superclass. (Don't name it 'super-start'. Use a Java name.)
    :exposes {random {:get gitRandom}, schedule {:get gitSchedule}}
    :methods []
    :state instanceState
    :init init-instance-state
    :main true) 




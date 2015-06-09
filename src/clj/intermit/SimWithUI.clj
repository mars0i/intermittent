;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; Notes:
;; In Students, we have
;;  * the SimState object (students)
;;  * a Continuous2D (yard)
;;    - we calculate x,y coordinates for each student, and insert it into the yard 
;;      Continuous2D using setObjectLocation.
;;  * the ContinuousPortrayal2D (yard-portrayal)
;;    - we setField it with the yard, i.e. the Continous2D
;;    - we setPortrayalForAll it with various Portrayals that apply to the indiv students, somehow
;; Also:
;; * a Network (buddies)
;;   - we add each student to the Network using addNode
;;   - we add (random) links to the Network using addEdge
;; * a NetworkPortrayal2D (buddies-portrayal)
;;   - we setField it with a SpatialNetwork2D, to which we pass 
;;     the yard Continuous2D and the buddies Network.
;;   - we setPortryalForAll it with an edge portrayal, that applies to edges, apparently.
;; Also:
;; * we do the preceding in a method called from -start.
;; * we do various things in -init to attach() this stuff to the display objects.
;; * and some display setup in -main.


(ns intermit.SimWithUI
  ;; TODO REVISE THESE IMPORTS--PROBABLY NOT ALL NEEDED:
  (:import [sim.portrayal.continuous ContinuousPortrayal2D]
           [sim.portrayal.network NetworkPortrayal2D SpatialNetwork2D SimpleEdgePortrayal2D]
           [sim.portrayal.simple OvalPortrayal2D LabelledPortrayal2D CircledPortrayal2D MovablePortrayal2D]
           [sim.display Console Display2D]
           [java.awt Color])
  (:gen-class
    :name intermit.SimWithUI
    :extends sim.display.GUIState
    :main true
    :exposes {state {:get getState}}  ; accessor for field in superclass
    :exposes-methods {start superStart, quit superQuit, init superInit, getInspector superGetInspector}
    ;; WHICH OF THESE DO I NEED (if any)?:
    :methods [[getDisplay [] sim.display.Display2D]
              [setDisplay [sim.display.Display2D] void]
              [getDisplayFrame [] javax.swing.JFrame]
              [setDisplayFrame [javax.swing.JFrame] void]
              [gitSpacePortrayal [] sim.portrayal.continuous.ContinuousPortrayal2D]
              [gitLinksPortrayal [] sim.portrayal.network.NetworkPortrayal2D]
              [setupPortrayals [] void]]
    :state iState
    :init init-instance-state))


(defn -init-instance-state
  [& args]
  [(vec args) {:display (atom nil)
               :display-frame (atom nil)
               :space-portrayal (ContinuousPortrayal2D.)
               :buddies-portrayal (NetworkPortrayal2D.)
               }])

(defn -getDisplay [this] @(:display (.iState this)))
(defn -setDisplay [this newval] (reset! (:display (.iState this)) newval))
(defn -getDisplayFrame [this] @(:display-frame (.iState this)))
(defn -setDisplayFrame [this newval] (reset! (:display-frame (.iState this)) newval))
(defn -gitSpacePortrayal [this] (:space-portrayal (.iState this)))
(defn -gitLinksPortrayal [this] (:buddies-portrayal (.iState this)))

(defn -getSimulationInspectedObject [this] (.state this))

;; Override super fn to set it as volatile
(defn -getInspector
  [this]
  (let [i (.superGetInspector this)]
    (.setVolatile i true)
    i))

;;;;;;;;;;;;;;;;;;;;

(defn -main
  [& args]
  (let [vid (intermit.SimWithUI. (intermit.Sim. (System/currentTimeMillis)))]
    (.setVisible (Console. vid) true)))

(defn -getName [this] "Intermittent") ; override method in super

(defn -start
  [this]
  (.superStart this)
  (.setupPortrayals this))

(defn -setupPortrayals
  [this]
  (let [sim (.getState this)
        space-portrayal (.gitSpacePortrayal this)
        buddies-portrayal (.gitLinksPortrayal this)
        display (.getDisplay this)
        extended-oval-portayal (proxy [OvalPortrayal2D] []
                                 (draw [student graphics info]
                                   (let [agitation-shade (min 255 (int 
                                                                    (* (.getAgitation student) (/ 255 10.0))))]
                                     (set! (.-paint this)  ; paint var in OvalPortrayal2D; 'this' is auto-captured by proxy
                                           (Color. agitation-shade 0 (- 255 agitation-shade)))
                                     (proxy-super draw student graphics info))))]
    (doto space-portrayal 
      (.setField (.gitYard sim)) ; NOTE Sim is the regular sim-state object, and this returns a Continuous2D in the Students simulation
      (.setPortrayalForAll       ; SO MAYBE I need a Continuous2D field thing here, too?  Note it's 100x100, vs the display area that's in pixels.
        (-> extended-oval-portayal  ; The Continuous2D is the thing that the agents are hashed into.
          (LabelledPortrayal2D. 5.0 nil Color/black true)
          (CircledPortrayal2D. 0 5.0 Color/green true)
          (MovablePortrayal2D.))))
    (doto buddies-portrayal
      (.setField (SpatialNetwork2D. (.gitYard sim) (.gitLinks sim)))
      (.setPortrayalForAll (SimpleEdgePortrayal2D.)))
    (doto display
      (.reset )
      (.setBackdrop Color/white)
      (.repaint))))


;; MOSTLY OK NOW AFTER SMALL MODS FROM Students VERSION.
;; UI method--not for initializing the gen-class state.
(defn -init
  [this controller] ; controller is called c in Java version
  (.superInit this controller)
  (let [display (Display2D. 800 800 this)
        display-frame (.createFrame display)]
    (.setDisplay this display)
    (doto display
      (.setClipping false)
      (.attach (.gitLinksPortrayal this) "Links")  ; IS THIS OK?
      (.attach (.gitSpacePortrayal this) "Space")) ; IS THIS OK?
    ;; set up display frame:
    (.setDisplayFrame this display-frame)
    (.registerFrame controller display-frame)
    (doto display-frame 
      (.setTitle "Intermittent Display")
      (.setVisible true))))


;; THIS IS PROBABLY OK AS IS FROM Students
(defn -quit
  [this]
  (.superQuit this)  ; combine in doto?
  (when-let [display-frame (.getDisplayFrame this)]
    (.dispose display-frame))
  (doto this
    (.setDisplayFrame nil)
    (.setDisplay nil)))

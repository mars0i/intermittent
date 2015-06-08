;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

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
              [gitYardPortrayal [] sim.portrayal.continuous.ContinuousPortrayal2D]
              [gitBuddiesPortrayal [] sim.portrayal.network.NetworkPortrayal2D]
              [setupPortrayals [] void]]
    :state iState
    :init init-instance-state))


(defn -init-instance-state
  [& args]
  [(vec args) {:display (atom nil)
               :display-frame (atom nil)
               :yard-portrayal (ContinuousPortrayal2D.)
               :buddies-portrayal (NetworkPortrayal2D.)}])

(defn -getDisplay [this] @(:display (.iState this)))
(defn -setDisplay [this newval] (reset! (:display (.iState this)) newval))
(defn -getDisplayFrame [this] @(:display-frame (.iState this)))
(defn -setDisplayFrame [this newval] (reset! (:display-frame (.iState this)) newval))
(defn -gitYardPortrayal [this] (:yard-portrayal (.iState this)))
(defn -gitBuddiesPortrayal [this] (:buddies-portrayal (.iState this)))

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

(defn -getName [this] "Student Schoolyard Cliques") ; override method in super

(defn -start
  [this]
  (.superStart this)
  (.setupPortrayals this))

(defn -setupPortrayals
  [this]
  (let [sim (.getState this)
        yard-portrayal (.gitYardPortrayal this)
        buddies-portrayal (.gitBuddiesPortrayal this)
        display (.getDisplay this)
        extended-oval-portayal (proxy [OvalPortrayal2D] []
                                 (draw [student graphics info]
                                   (let [agitation-shade (min 255 (int 
                                                                    (* (.getAgitation student) (/ 255 10.0))))]
                                     (set! (.-paint this)  ; paint var in OvalPortrayal2D; 'this' is auto-captured by proxy
                                           (Color. agitation-shade 0 (- 255 agitation-shade)))
                                     (proxy-super draw student graphics info))))]
    (doto yard-portrayal 
      (.setField (.gitYard sim))
      (.setPortrayalForAll 
        (-> extended-oval-portayal 
          (LabelledPortrayal2D. 5.0 nil Color/black true)
          (CircledPortrayal2D. 0 5.0 Color/green true)
          (MovablePortrayal2D.))))
    (doto buddies-portrayal
      (.setField (SpatialNetwork2D. (.gitYard sim) (.gitBuddies sim)))
      (.setPortrayalForAll (SimpleEdgePortrayal2D.)))
    (doto display
      (.reset )
      (.setBackdrop Color/white)
      (.repaint))))


;; UI method--not for initializing the gen-class state.
(defn -init
  [this controller] ; controller is called c in Java version
  (.superInit this controller)
  (let [display (Display2D. 600 600 this)
        display-frame (.createFrame display)]
    ;; set up display:
    (.setDisplay this display)
    (doto display
      (.setClipping false)
      (.attach (.gitBuddiesPortrayal this) "Buddies")
      (.attach (.gitYardPortrayal this) "Yard"))
    ;; set up display frame:
    (.setDisplayFrame this display-frame)
    (.registerFrame controller display-frame)
    (doto display-frame 
      (.setTitle "Schoolyard Display")
      (.setVisible true))))


(defn -quit
  [this]
  (.superQuit this)  ; combine in doto?
  (when-let [display-frame (.getDisplayFrame this)]
    (.dispose display-frame))
  (doto this
    (.setDisplayFrame nil)
    (.setDisplay nil)))

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
;; * a Network (net)
;;   - we add each student to the Network using addNode
;;   - we add (random) links to the Network using addEdge
;; * a NetworkPortrayal2D (net-portrayal)
;;   - we setField it with a SpatialNetwork2D, to which we pass 
;;     the yard Continuous2D and the net Network.
;;   - we setPortryalForAll it with an edge portrayal, that applies to edges, apparently.
;; Also:
;; * we do the preceding in a method called from -start.
;; * we do various things in -init to attach() this stuff to the display objects.
;; * and some display setup in -main.


(ns intermit.SimWithUI
  (:require [intermit.layout :as lay]
            [intermit.Sim :as s])
  (:import [intermit Sim]
           [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.portrayal.continuous ContinuousPortrayal2D]
           [sim.portrayal.network NetworkPortrayal2D SpatialNetwork2D SimpleEdgePortrayal2D] ; TODO REVISE LIST
           [sim.portrayal.simple OvalPortrayal2D LabelledPortrayal2D CircledPortrayal2D MovablePortrayal2D] ; TODO REVISE LIST
           [sim.display Console Display2D] ; TODO REVISE LIST
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
              [setupPortrayals [] void]]
    :state iState
    :init init-instance-state))

(defn -init-instance-state
  [& args]
  (let [field (Continuous2D. 1.0 125 100)
        net (Network.)
        field-portrayal (ContinuousPortrayal2D.)
        net-portrayal (NetworkPortrayal2D.)]
    (.setField field-portrayal field) ; we only need this for the display
    (.setField net-portrayal (SpatialNetwork2D. field net))
    [(vec args) {:display (atom nil)
                 :display-frame (atom nil)
                 :field-portrayal field-portrayal
                 :net-portrayal (NetworkPortrayal2D.)}])) ; TODO

(defn -getDisplay [this] @(:display (.iState this)))
(defn -setDisplay [this newval] (reset! (:display (.iState this)) newval))
(defn -getDisplayFrame [this] @(:display-frame (.iState this)))
(defn -setDisplayFrame [this newval] (reset! (:display-frame (.iState this)) newval))
(defn -getSimulationInspectedObject [this] (.state this))

(defn get-field-portrayal [this] (:field-portrayal (.iState this)))
(defn get-field [this] (.getField (:field-portrayal (.iState this))))
(defn get-links-portrayal [this] (:net-portrayal (.iState this)))
(defn get-links [this] (throw (Exception. "Not yet implemented.")))


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
  (.clear (get-field this)) ; TODO Can I put this here?
  ;(.clear (get-links this)) ; TODO Can I put this here?
  (.setupPortrayals this))

(defn -setupPortrayals
  [this]
  (let [sim (.getState this)
        field-portrayal (get-field-portrayal this)
        field (.getField field-portrayal)
        ; net-portrayal (get-links-portrayal this)
        display (.getDisplay this)
        communities (s/get-communities sim)]

    ;(lay/set-community-locs! field communities)
    (lay/set-indiv-locs! field communities)
    (.setPortrayalForClass field-portrayal intermit.Sim.Community (OvalPortrayal2D. (Color. 255 0 0) 2.0))
    (.setPortrayalForClass field-portrayal intermit.Sim.Indiv (OvalPortrayal2D. (Color. 0 0 255) 1.5))
    ;(doto field-portrayal (.setPortrayalForAll (-> (OvalPortrayal2D.) (LabelledPortrayal2D. 5.0 nil Color/black true) (CircledPortrayal2D. 0 5.0 Color/green true) (MovablePortrayal2D.))))
    ; (doto net-portrayal (.setField (SpatialNetwork2D. (get-field sim) (get-links sim))) (.setPortrayalForAll (SimpleEdgePortrayal2D.)))
    (doto display
      (.reset )
      (.setBackdrop Color/white)
      (.repaint))))


;; MOSTLY OK NOW AFTER SMALL MODS FROM Students VERSION.
;; UI method--not for initializing the gen-class state.
(defn -init
  [this controller] ; controller is called c in Java version
  (.superInit this controller)
  (let [display (Display2D. 800 600 this)
        display-frame (.createFrame display)]
    (.setDisplay this display)
    (doto display
      (.setClipping false)
      ;(.attach (get-links-portrayal this) "Links")  ; IS THIS OK?
      (.attach (get-field-portrayal this) "Field")) ; IS THIS OK?
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

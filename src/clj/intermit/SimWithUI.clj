;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.


(ns intermit.SimWithUI
  (:require [intermit.layout :as lay]
            [intermit.Sim :as s])
  (:import [intermit Sim]
           [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
           [sim.portrayal Oriented2D Orientable2D] ; TODO I probably only use one of these, or none
           [sim.portrayal.continuous ContinuousPortrayal2D]
           [sim.portrayal.network NetworkPortrayal2D SpatialNetwork2D SimpleEdgePortrayal2D]
           [sim.portrayal.simple OvalPortrayal2D OrientedPortrayal2D]
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
              [setupPortrayals [] void]]
    :state iState
    :init init-instance-state))

(defn -init-instance-state
  [& args]
  (let [field (Continuous2D. 1.0 125 100)
        field-portrayal (ContinuousPortrayal2D.)
        net (Network.)
        net-portrayal (NetworkPortrayal2D.)]
    ;; setField on the net-portrayal here doesn't work--no error, but no net links displayed
    ;; setField on field-portrayal works, but I moved it to near where the net-portrayal version went.
    (.setField field-portrayal field)
    (.setField net-portrayal (SpatialNetwork2D. field net))
    [(vec args) {:display (atom nil)
                 :display-frame (atom nil)
                 :field-portrayal field-portrayal
                 :net-portrayal net-portrayal
                 :net net}]))

(defn -getDisplay [this] @(:display (.iState this)))
(defn -setDisplay [this newval] (reset! (:display (.iState this)) newval))
(defn -getDisplayFrame [this] @(:display-frame (.iState this)))
(defn -setDisplayFrame [this newval] (reset! (:display-frame (.iState this)) newval))
(defn -getSimulationInspectedObject [this] (.state this))

(defn get-field-portrayal [this] (:field-portrayal (.iState this)))
(defn get-field [this] (.getField (:field-portrayal (.iState this))))
(defn get-net-portrayal [this] (:net-portrayal (.iState this)))
(defn get-net [this] (:net (.iState this)))

;; Override super fn to set it as volatile
;(defn -getInspector
;  [this]
;  (let [i (.superGetInspector this)]
;    (.setVolatile i true)
;    i))

;;;;;;;;;;;;;;;;;;;;

(defn -main
  [& args]
  (let [vid (intermit.SimWithUI. (intermit.Sim. (System/currentTimeMillis)))]
    (.setVisible (Console. vid) true)))

(defn -getName [this] "Intermittent") ; override method in super

(defn -start
  [this]
  (.superStart this) ; this will call start() on the sim, i.e. our SimState object
  (.setupPortrayals this))

(defn -setupPortrayals
  [this-gui]  ; instead of 'this': avoid confusion with proxy below
  (let [sim (.getState this-gui)
        field-portrayal (get-field-portrayal this-gui)
        field (.getField field-portrayal)
        net-portrayal (get-net-portrayal this-gui)
        net (get-net this-gui)
        display (.getDisplay this-gui)
        communities (s/get-communities sim)
        population (s/get-population sim)
        indiv-portrayal (OrientedPortrayal2D.  ; what this represents is set in the Oriented2D part of Indiv in Sim.clj
                          (proxy [OvalPortrayal2D] [1.5]    ; note proxy auto-captures 'this'
                            (draw [indiv graphics info]                      ; override OvalPortrayal2D method
                              (let [shade (int (* (.getRelig indiv) 255))]
                                (set! (.-paint this) (Color. shade 0 (- 255 shade))) ; paint var is in OvalPortrayal2D
                                (proxy-super draw indiv graphics info))))
                          0 1.75 (Color. 0 0 0) OrientedPortrayal2D/SHAPE_LINE) ; color is of of orientation line/shape
        edge-portrayal (SimpleEdgePortrayal2D. (Color. 130 90 30) nil)]
    ;; set up node display
    (.clear field)
    (lay/set-indiv-locs! field communities)
    (.setPortrayalForClass field-portrayal intermit.Sim.Indiv indiv-portrayal)
    ;; set up network link display:
    (.clear net)
    (lay/set-links! net population)
    ;(.setAdjustsThickness edge-portrayal true) ;(.setBaseWidth edge-portrayal 1.0) ; trying to set line thicknesses
    (.setPortrayalForAll net-portrayal edge-portrayal)
    ;; set up display
    (doto display
      (.reset )
      (.setBackdrop Color/white)
      (.repaint))))


;;    (.setPortrayalForClass field-portrayal intermit.Sim.Indiv (OvalPortrayal2D. (Color. 0 0 255) 1.5))
;; community display--not in use:
    ;; specify where community objects should be placed, and what they look like:
    ;(lay/set-community-locs! field communities) ; not currently displaying communities per se
    ;(.setPortrayalForClass field-portrayal intermit.Sim.Community (OvalPortrayal2D. (Color. 255 0 0) 2.0))

(defn -init
  [this controller] ; controller is called c in Java version
  (.superInit this controller)
  (let [display (Display2D. 800 600 this)
        display-frame (.createFrame display)]
    (.setDisplay this display)
    (doto display
      (.setClipping false)
      (.attach (get-net-portrayal this) "Net")
      (.attach (get-field-portrayal this) "Field"))
    ;; set up display frame:
    (.setDisplayFrame this display-frame)
    (.registerFrame controller display-frame)
    (doto display-frame 
      (.setTitle "Intermittent")
      (.setVisible true))))


(defn -quit
  [this]
  (.superQuit this)  ; combine in doto?
  (when-let [display-frame (.getDisplayFrame this)]
    (.dispose display-frame))
  (doto this
    (.setDisplayFrame nil)
    (.setDisplay nil)))

;;; This software is copyright 2015 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; Note: Traditional MASON models put e.g. Continuous2D and Network in another
;; class that's central to the model.  This class would normally use those
;; instances from the other class, passing them to portrayals created here.
;; Since the underlying model (Sim) doesn't need spatial relations or explicit
;; link representations, I create the Continuous2D and Network here, because
;; they're needed by the portrayals.

(ns intermit.SimWithUI
  (:require [intermit.layout :as lay]
            [intermit.Sim :as s])
  (:import [intermit Sim]
           [sim.field.continuous Continuous2D]
           [sim.field.network Network Edge]
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
    ;:methods []
    :state iState
    :init init-instance-state))

(def indiv-position-jitter 0.30) ; stddev of noise added to node positions to make it easier to distinguish links to vs those that just happen to cross a node

(defn -init-instance-state
  [& args]
  (let [field (Continuous2D. 1.0 125 100)
        field-portrayal (ContinuousPortrayal2D.)
        socnet (Network.)
        socnet-portrayal (NetworkPortrayal2D.)]
    (.setField field-portrayal field)
    (.setField socnet-portrayal (SpatialNetwork2D. field socnet))
    [(vec args) {:display (atom nil)
                 :display-frame (atom nil)
                 :field-portrayal field-portrayal
                 :socnet-portrayal socnet-portrayal
                 :socnet socnet}]))

(defn get-display [this] @(:display (.iState this)))
(defn set-display [this newval] (reset! (:display (.iState this)) newval))
(defn get-display-frame [this] @(:display-frame (.iState this)))
(defn set-display-frame [this newval] (reset! (:display-frame (.iState this)) newval))
(defn get-field-portrayal [this] (:field-portrayal (.iState this)))
(defn get-field [this] (.getField (:field-portrayal (.iState this))))
(defn get-net-portrayal [this] (:socnet-portrayal (.iState this)))
(defn get-net [this] (:socnet (.iState this)))

;; Override methods in sim.display.GUIState so that UI can make graphs, etc.
(defn -getSimulationInspectedObject [this] (.state this))
(defn -getInspector [this] (let [i (.superGetInspector this)] (.setVolatile i true) i))

;;;;;;;;;;;;;;;;;;;;

(declare setup-portrayals)

(defn -main
  [& args]
  (let [vid (intermit.SimWithUI. (intermit.Sim. (System/currentTimeMillis)))]
    (.setVisible (Console. vid) true)))

(defn -getName [this] "Intermittent") ; override method in super

(defn -start
  [this]
  (.superStart this) ; this will call start() on the sim, i.e. our SimState object
  (setup-portrayals this))

(defn setup-portrayals
  [this-gui]  ; instead of 'this': avoid confusion with proxy below
  (let [sim (.getState this-gui)
        rng (.random sim)
        field-portrayal (get-field-portrayal this-gui)
        field (.getField field-portrayal)
        socnet-portrayal (get-net-portrayal this-gui)
        socnet (get-net this-gui)
        display (get-display this-gui)
        communities (s/get-communities sim)
        population (s/get-population sim)
        indiv-portrayal (OrientedPortrayal2D.  ; what this represents is set in the Oriented2D part of Indiv in Sim.clj
                          (proxy [OvalPortrayal2D] [1.5]    ; note proxy auto-captures 'this'
                            (draw [indiv graphics info]                      ; override OvalPortrayal2D method
                              (let [shade (int (* (.getRelig indiv) 255))]
                                (set! (.-paint this) (Color. shade 0 (- 255 shade))) ; paint var is in OvalPortrayal2D
                                (proxy-super draw indiv graphics info))))
                          0 1.75 (Color. 0 0 0) OrientedPortrayal2D/SHAPE_LINE) ; color is of of orientation line/shape
        edge-portrayal (SimpleEdgePortrayal2D. (Color. 140 140 140) nil)]
    ;; set up node display
    (.clear field)
    (lay/set-indiv-locs! rng indiv-position-jitter field communities) ; jitter makes easier to distinguish links that just happen to cross a node
    (.setPortrayalForClass field-portrayal intermit.Sim.Indiv indiv-portrayal)
    ;; set up network link display:
    (.clear socnet)
    (lay/set-links! socnet population) ; set-links! sets edges' info fields to nil (null): edges have no weight, so weight defaults to 1.0
    (.setShape edge-portrayal SimpleEdgePortrayal2D/SHAPE_LINE_BUTT_ENDS) ; Default SHAPE_THIN_LINE doesn't allow changing thickness. Other differences don't matter, if thinner than nodes.
    (.setBaseWidth edge-portrayal 0.15) ; line width
    ;(.setAdjustsThickness edge-portrayal true) ;(.setBaseWidth edge-portrayal 1.0) ; trying to set line thicknesses
    (.setPortrayalForAll socnet-portrayal edge-portrayal)
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
    (set-display this display)
    (doto display
      (.setClipping false)
      (.attach (get-net-portrayal this) "Net")
      (.attach (get-field-portrayal this) "Field"))
    ;; set up display frame:
    (set-display-frame this display-frame)
    (.registerFrame controller display-frame)
    (doto display-frame 
      (.setTitle "Intermittent")
      (.setVisible true))))


(defn -quit
  [this]
  (.superQuit this)  ; combine in doto?
  (when-let [display-frame (get-display-frame this)]
    (.dispose display-frame))
  (doto this
    (set-display-frame nil)
    (set-display nil)))

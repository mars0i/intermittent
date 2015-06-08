(ns intermit.Test)

(defprotocol P (getFriendX [this]))

;(import [intermit.Yo Bar])

(deftype Bar [x friend]
  P
  (getFriendX [this] (.x ^Bar @friend))) ; I didn't think this would work!

(def bar1 (intermit.Yo.Bar. 5 (atom nil)))
(def bar2 (intermit.Yo.Bar. 7 (atom nil)))
(reset! (.friend bar1) bar2)
(reset! (.friend bar2) bar1)

(println (getFriendX bar1)) ;=> 7
(println (getFriendX bar2)) ;=> 5

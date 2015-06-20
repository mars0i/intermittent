
;; poss define other linkers 
;; http://www.drdobbs.com/architecture-and-design/simulating-small-world-networks/184405611
;; https://compuzzle.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/
;; https://codepretty.wordpress.com/2015/02/03/generating-barabasi-albert-model-graphs-in-clojure/

;; Simple version
;; Doesn't guarantee that all members of community are connected (that's a harder test)
;; DOESN'T SEEM TO WORK RIGHT.
(defn add-until-min-links!
  "Given indivs, some of whom may have neighbors, makes sure that everyone has
  at least one neighbor by randomly adding neighbors to anyone who has none."
  [rng min-links indivs]
  (when-let [islands (seq (filter #(empty? (.getNeighbors %)) indivs))] ; seq turns () into nil
    (doseq [island islands]
      (loop [remaining-links min-links
             indivs indivs]
        (when (pos? remaining-links)
          (let [indiv (nth indivs (.nextInt rng (count indivs)))]
            (add-neighbor! island indiv)
            (recur (dec remaining-links) (remove #(identical? indiv %) indivs))))))))

;; Need to randomly permute islands or butterflies (sim.Engine.RandomSequence seems to work only on Steppables)
(defn let-no-indiv-be-an-island
  "Given indivs, some of whom may have neighbors, makes sure that everyone has
  at least one neighbor by stealing links (rewiring) from those with the most
  links, and giving those links to the lonely.  If there are not enough links
  for everyone to have at least one, adds links.  (Not particularly efficient.)"
  [rng prob indivs]
  (when-let [islands (seq (filter #(empty? (.getNeighbors %)) indivs))] ; seq turns () into nil
    (let [max-degree (reduce #(max %1 (count (.getNeighbors 2%))) 0 indivs)]
      (if (> max-degree 1)
        (doseq [butterfly (filter #(== max-degree (count (.getNeighbors %))) indivs)
                :let [indiv (first indivs)
                      rest-indivs (rest-indivs)]]
          (let [neighbors (.getNeighbors butterfly)
                moving-neighbor (nth neighbors (.nextInt rng (count neighbors)))
                staying-neighbors (remove #(identical? moving %) neighbors)]
            (set-neighbors! butterfly staying-neighbors)
            (set-neighbors! (first indivs) [moving-neighbor])


(defn update-fields!
  "Set the success and relig fields of the community and the success field of
  each of its members to the value of avg-relig for the members.  (Indiv success
  is the same as community success, and community relig is the average of its
  indivs' religs.)"
  [^Community community]
  (let [comm-members (.members community)
        ^double avg-rel (avg-relig comm-members)]
    (reset! (.relig community) avg-rel)
    (reset! (.success community) avg-rel)
    (doseq [^Indiv indiv comm-members]
      (reset! (.success indiv) avg-rel))))
;; TODO This way of determining indiv success is problematic.
;; It means (a) all members of comm have same success, so there's
;; no point in surveying neighbors; they should be chosen randomly
;; if they don't beat out the global candidate(s).

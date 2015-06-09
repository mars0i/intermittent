

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

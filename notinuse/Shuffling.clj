
            ^MersenneTwisterFast rng (.random sim)
            pop-bag (sim.util.Bag. ^Collection population)] ; THIS is where most of the order-of-magnitude hit comes
        (.shuffle pop-bag rng)                                         ; CONSIDER using a Clojure shuffle routine, or use Bags more of the time to avoid conversion
        ;(copy-relig! this sim (vec pop-bag))))
        ;(copy-relig! this sim (shuffle population)))) ; JUST AS slow as the Bag version. Clojure shuffle throws coll into an ArrayList and then calls java.util.Collections/shuffle on it.

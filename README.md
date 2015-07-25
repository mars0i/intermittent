# Intermittent
Experiments with intermittent between-group communication.  
Generalizes BaliPlus in the bali repo.  

In this model, within-group communication is common, and between-group
communication is uncommon.  Transmission is success-biased, where
success depends on group state.

In this version (originally on git branch pllupd), communication happens
"in parallel": Each indiv has a cultural variant variable, and a "new"
cultural variant variable.  Communication takes place by updating an
indiv's "new" variable based on the non-new vars in others.  Then all
of the new values are copied to the regular variables in one step.

This became the master git branch on 7/14/2015.

## How to run it:

#### From the command line with Leiningen:

First change to Intermittent's root directory.

Show Intermittent's help:

	lein run -- -?

Show MASON's help:

	lein run -- -help

Run simulation:

	lein run

or

	lein run -- <Intermittent-and-MASON-options>

(Parameters before `--` will be passed to Leiningen.)


#### GUI from the Clojure REPL:

First change to Intermittent's root directory, then:

	lein repl

This will give you a Clojure prompt:

	intermit.Sim=>

Here's one way to start the GUI version of Intermittent:

	intermit.Sim=> (require '[intermit.SimWithUI :as ui])
	intermit.Sim=> (def sim (ui/repl-gui))



`sim` now comtains the `intermit.Sim` object.  After starting the
simulation in the GUI, you can inspect the simulation's data e.g. like
this (displaying returned values after `==>`):

	intermit.Sim=> (def data (.instanceState sim))
	intermit.Sim=> (.numCommunities data)
	==> #object[clojure.lang.Atom 0x1267fb9f {:status :ready, :val 12}]
	intermit.Sim=> @(.numCommunities data)
	==> 12
	intermit.Sim=> (def indivs @(.population data)
	intermit.Sim=> (pprint indivs)
	intermit.Sim=> (map getRelig indivs)

See e.g. the definitions of `InstanceState` and `Indiv` in Sim.clj for
more things that you can inspect.

You can also modify fields, of course, if their data is contained in
an atom, as is the case for fields of `InstanceState` (what's in
`data` above).  Most of the fields of `Indiv` are mutable, which means
that you can modify them from the repl only if there's a setter
function that's been defined.

For example, you might want to clear out accumulated history:

	intermit.Sim=> (reset! (.meanReligSeries data) [])
	intermit.Sim=> (reset! (.meanSuccessSeries data) [])


#### Without GUI in the Clojure REPL:

	intermit.Sim=> (def sim (Sim. <seed>))
	intermit.Sim=> (-start sim)

(TBD. What's next?)


#### With `java` at the command line:

(TBD.  `lein jar` or `lein uberjar` is part of the story, but I haven't
worked out the right classpath specifications for all of the libraries
that MASON needs.)



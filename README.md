# intermittent
Experiments with intermittent between-group communication.  
Generalizes BaliPlus in the bali repo.  

In this model, within-group communication is common, and between-group
communication is uncommon.  Transmission is success-biased, where
success depends on group state.

In this version, indivs have a single cultural variant variable, and
communication from others' cultvar variables happens in random
order--i.e. the indivs who listen to their neighbors and others are
selected in random orders.  This allows per-tick path dependencies,
but each tick will have different ones.

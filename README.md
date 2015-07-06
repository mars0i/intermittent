# intermittent
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

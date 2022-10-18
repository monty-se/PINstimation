# Version 0.1.0.900

## New Features

 * The functions `pin()`, `pin_*()`, `mpin_ml()`, `mpin_ecm()`, `adjpin()`,
 `vpin()`, and `aggregate_trades()` accept now, for their arguments `data`,
 datasets of type `matrix`. In the previous version, it only accepted 
 dataframes, which did not allow users, for instance, to use `rollapply()` of
 the package `zoo`.

  * Introduction of the function `pin_bayes()` that estimates the original pin
  model using a bayesian approach as described in Griffin et al.(2021).    
 
## Bug Fixes

 * Fixed an error in the function `initials_pin_ea()` as it used to produce
 some parameter sets with negative values for trade intensity rates. The negative
 trade intensity rates are set to zero.

 * Fixed two errors in the function `vpin()`: (1) A bug in the calculation steps
 of vpin (2) The argument `verbose` does not work properly.
 
 * Fixed an issue with resetting the plan for the future (`future::plan`) 
 used for parallel processing.

# Version 0.1.0

* Added a `NEWS.md` file to track changes to the package.

# PINstimation 0.1.3.9000 (28.07.2024)

## New Features
- **`adjpin()`**: The function now includes the time spent on generating initial
  parameter sets in the total time displayed in the output. This enhancement
  provides a more comprehensive view of the time taken for the entire process.

## Updates
- **`initials_adjpin_rnd()`**: Updated the implementation for generating random
  initial parameter sets to align with the algorithm described in Ersan and
  Ghachem (2024).
- **`initials_adjpin()`**: Aligned the function, which generates initial parameter
  sets for the adjusted PIN model, with the algorithm outlined in Ersan and
  Ghachem (2024).
- **`solve_eqx`**: Enhanced the format and performance of polynomial root
  calculations within the conditional-maximization steps of the ECM algorithm.

## Bugfixes
- **`initials_adjpin_cl()`**: Fixed an issue with the calculation of the
  likelihood value according to the algorithm of Cheng and Lai (2021).
- **`detectlayers_eg()`**: Corrected the return value to a single number when
  the number of information days is equal to 1. Previously, it incorrectly
  returned a vector.
- **`mpin_ecm()`**: Rectified an issue where observations with zero probability
  in the E-step of the ECM algorithm were assigned a fixed number of clusters
  (`6`). Now, the function assigns a uniform probability of `1/cls`, where `cls`
  is the total number of clusters, to each cluster.

## Dependency Management
- **Future Package**: Addressed two concerns related to the `future` package.
  The updated code now uses lexical scoping with the `local()` function to manage
  the variable `.lwbound` between parallel function calls, preventing unexpected
  results. Additionally, the maximum size of futures is no longer set to `+Inf`
  upon package loading, leaving this option adjustable by the user.

## Documentation
- Replaced the deprecated '@doctype package' tag with '_Package' to ensure
  proper generation of package documentation.

# PINstimation 0.1.2

## New Features

* We introduce a new function called `classify_trades()` that enables users to
classify high-frequency (HF) trades individually, without aggregating them.  
For each HF trade, the function assigns a variable that is set to `TRUE` if the
trade is buyer-initiated, or `FALSE` if it is seller-initiated.

* The `aggregate_trades()` function enables users to aggregate high-frequency
(HF) trades at different frequencies. In the previous version, HF trades were
automatically aggregated into daily trade data. However, with the updated
version, users can now specify the desired frequency, such as every 15 minutes.

## Bugfixes

* We identified and corrected an error in the `mpin_ecm()` function. Previously,
the function would sometimes produce inconsistent results as the posterior
distribution allowed for the existence of information layers with a probability
of zero. We have now fixed this issue and the function produces correct results.

* We have made some updates to the `mpin_ml()` function to better handle cases
where the MPIN estimation fails for all initial parameter sets. Specifically,
we have fixed an error in the display of the estimation results when such failure
occurs. With these updates, the function should now be able to handle such
failures more robustly and provide appropriate feedback.

* We have simplified the ECM estimation functions, with a particular focus on
the adjpin() function. We have improved the convergence condition of the
iterative process used in the ECM estimation. Moreover, we rounded the values
of the parameters at each iteration to a relevant number of decimals. This
shall result in a faster convergence and prevent issues with decreasing
likelihood values.


# PINstimation 0.1.1

## New Features

* The functions `pin()`, `pin_*()`, `mpin_ml()`, `mpin_ecm()`, `adjpin()`,
`vpin()`, and `aggregate_trades()` accept now, for their arguments `data`,
datasets of type `matrix`. In the previous version, it only accepted
dataframes, which did not allow users, for instance, to use `rollapply()` of
the package `zoo`.

* Introduction of the function `pin_bayes()` that estimates the original pin
model using a bayesian approach as described in Griffin et al.(2021).

## Bugfixes

* Fixed an error in the function `initials_pin_ea()` as it used to produce
some parameter sets with negative values for trade intensity rates. The negative
trade intensity rates are set to zero.

* Fixed two errors in the function `vpin()`: (1) A bug in the calculation steps
of vpin (2) The argument `verbose` does not work properly.

* Fixed an issue with resetting the plan for the future (`future::plan`)
used for parallel processing.

# PINstimation 0.1.0

* Added a `NEWS.md` file to track changes to the package.

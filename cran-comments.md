## Resubmission 3 (version 0.2.0)

This is a resubmission. In this version, I added new functionality, improved performance, and fixed bugs from previous versions.

## Main changes

### New features
- Added `ivpin()`, an improved VPIN estimator (maximum-likelihood based) intended to provide more stable VPIN estimates for small volume buckets and sparse informed trading.
- Updated `initials_adjpin()` to align with the initialization procedure described in Ersan and Ghachem (2024).
- Extended `classify_trades()` and `aggregate_trades()` to support negative `timelag` values (quote leads) and clarified that `timelag` is specified in microseconds.

### Updates, performance improvements, and fixes
- Refactored/optimized `vpin()` and fixed the bucket `duration` vector used to compute duration-weighted daily VPIN (`dwvpin`).
- Improved `adjpin()` output timing by including the time spent generating initial parameter sets in the reported runtime.
- Updated `initials_adjpin_rnd()` to align with Ersan and Ghachem (2024).
- Improved performance/format of polynomial root calculations in `solve_eqx()`.
- Fixed `mpin_ecm()` behavior when an observation receives zero probability for all clusters in the E-step: it now assigns uniform probabilities `1/cls`.
- Fixed `detectlayers_eg()` to return a scalar when the number of information days equals 1.
- Fixed `initials_adjpin_cl()` likelihood calculation to match Cheng and Lai (2021).

### CRAN check-related maintenance
- Replaced the deprecated roxygen2 `@docType package` tag with `_PACKAGE` to ensure correct package documentation generation.
- Added missing `dplyr` imports to NAMESPACE (`arrange`, `group_by`, `summarise`).
- Declared global variables via `utils::globalVariables()` to address R CMD check notes.
- The package no longer modifies the global `future` maximum size option on load; this option is left to the user.

## Test environments
- local Windows 11, R 4.2.1
- GitHub Actions: windows-latest (release), windows-latest (4.1),
  ubuntu-latest (devel), ubuntu-latest (release), ubuntu-latest (oldrel-1),
  ubuntu-latest (oldrel-2), macOS-latest (devel), macOS-latest (oldrel-1)


## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
There are currently no downstream dependencies for this package.


-----
-----


## Resubmission 2

This is a resubmission. In this version I have added some new features and fixed a few bugs. 


### New Features

* We introduce a new function called `classify_trades()` that enables users to
classify high-frequency (HF) trades individually, without aggregating them.  
For each HF trade, the function assigns a variable that is set to `TRUE` if the
trade is buyer-initiated, or `FALSE` if it is seller-initiated.

* The `aggregate_trades()` function enables users to aggregate high-frequency
(HF) trades at different frequencies. In the previous version, HF trades were 
automatically aggregated into daily trade data. However, with the updated 
version, users can now specify the desired frequency, such as every 15 minutes.

### Bugfixes

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
likelihood values."

### Test environments

* local windows 11 , R 4.2.1
* macOS-latest (release) (on GitHub)
* windows-latest (release) (on GitHub)
* windows-latest (4.1) (on GitHub)
* ubuntu-latest (devel) (on GitHub)
* ubuntu-latest (release) (on GitHub)
* ubuntu-latest (oldrel-1) (on GitHub)
* ubuntu-latest (oldrel-2) (on GitHub)
* Windows Server 2022, R-release, 64 bit - R 4.2.1
* Windows Server 2022, R-devel, 64 bit - R 4.3
* macOS 10.13.6 High Sierra, R-release, brew -  R 4.3
* macOS 10.13.6 High Sierra, R-release, CRAN's setup - R 4.3

### R CMD check results

0 errors | 0 warnings | 0 notes

### Reverse dependencies

There are currently no downstream dependencies for this package.


-----
-----



## Resubmission 1

This is a resubmission. In this version I have added some new features and fixed a few bugs. 


### New Features

 * The functions `pin()`, `pin_*()`, `mpin_ml()`, `mpin_ecm()`, `adjpin()`,
 `vpin()`, and `aggregate_trades()` accept now, for their arguments `data`,
 datasets of type `matrix`. In the previous version, it only accepted 
 dataframes, which did not allow users, for instance, to use `rollapply()` of
 the package `zoo`.

  * Introduction of the function `pin_bayes()` that estimates the original pin
  model using a bayesian approach as described in Griffin et al.(2021).  
 
### Bugfixes

 * Fixed an error in the function `initials_pin_ea()` as it used to produce
 some parameter sets with negative values for trade intensity rates. The negative
 trade intensity rates are set to zero.

 * Fixed two errors in the function `vpin()`: one is related to an estimation 
 error, and the argument `verbose` did not work properly.
 
 * Fixed an issue with resetting the plan for the future (`future::plan`) 
 used for parallel processing.

### Test environments

* local windows 10 , R 4.2.1
* Fedora Linux (on R-hub) R-devel
* macOS-latest (release) (on GitHub)
* windows-latest (release) (on GitHub)
* ubuntu-latest (devel) (on GitHub)
* ubuntu-latest (release)
* ubuntu-latest (oldrel-1)
* Windows Server 2022, R-release, 64 bit - R 4.2.1
* Windows Server 2022, R-devel, 64 bit - R 4.3
* macOS 10.13.6 High Sierra, R-release, brew -  R 4.3
* macOS 10.13.6 High Sierra, R-release, CRAN's setup - R 4.3

### R CMD check results

There were no ERRORs or WARNINGs. 

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.
-----
----

## R CMD check results

### Local, using devtools::check()


SUCCESS!

-- R CMD check results ------------------------------------------------------------------------------------ PINstimation 0.1.0 ----
Duration: 3m 46.8s

0 errors √ | 0 warnings √ | 0 notes √

### GitHub Actions 

- macOS-latest (release)
- windows-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (release)
- ubuntu-latest (oldrel-1)

All SUCCESS!√

### Platform checks using rhub

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

#### Note 1

---

Maintainer: 'Montasser Ghachem <montasser.ghachem@pinstimation.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  AdjPIN (11:232)
  Duarte (11:243)
  Easley (11:128, 11:157, 11:306)
  Ersan (11:204)
  MPIN (11:195)
  Multilayer (11:179)
  VPIN (11:297)
  al (11:167, 11:316)
  et (11:164, 11:313)
  
Explanation: 

The presumably misspelled words are either:

- concepts, or abbreviations related to statistical models in Finance
(e.g. AdjPIN stands for Adjusted Probability of informed trading). This
also concerns AdjPIN, MPIN, Multilayer, and VPIN.

- Researcher names: Ersan, Duarte, Easley

- 'et', and 'al', together form 'et al.', which is an abbreviation 
meaning “and others.”; used to shorten lists of author names in text 
citations.

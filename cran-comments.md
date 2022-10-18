## Resubmission

This is a resubmission. In this version I have added some new features and fixed a few bugs. 


### New Features

 * The functions `pin()`, `pin_*()`, `mpin_ml()`, `mpin_ecm()`, `adjpin()`,
 `vpin()`, and `aggregate_trades()` accept now, for their arguments `data`,
 datasets of type `matrix`. In the previous version, it only accepted 
 dataframes, which did not allow users, for instance, to use `rollapply()` of
 the package `zoo`.

  * Introduction of the function `pin_bayes()` that estimates the original pin
  model using a bayesian approach as described in Griffin et al.(2021).  
 
### Bug Fixes

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
* macOS-latest (release) (on Github)
* windows-latest (release) (on Github)
* ubuntu-latest (devel) (on Github)
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

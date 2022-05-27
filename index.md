# PINstimation Package

[![R-CMD-check](https://github.com/monty-se/PINstimation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/monty-se/PINstimation/actions/workflows/R-CMD-check.yaml)
[![Lint Code Base](https://github.com/monty-se/PINstimation/actions/workflows/super-linter.yml/badge.svg)](https://github.com/monty-se/PINstimation/actions/workflows/super-linter.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![GitHub R package version](https://img.shields.io/github/r-package/v/monty-se/pinstimation)
[![SSRN - 4117946](https://img.shields.io/static/v1?label=SSRN&message=4117946&color=2ea44f)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4117946)


## Overview

----

PINstimation provides utilities for the estimation of probability of informed trading models:
original PIN (PIN) in Easley and O'Hara (1992) and Easley et al. (1996); multilayer
PIN (MPIN) in Ersan (2016); Adjusted PIN (AdjPIN) in Duarte and Young (2009); and volume-
synchronized PIN (VPIN) in Easley et al. (2011, 2012). Various computation methods suggested
in the literature are included. Data simulation tools and trade classification algorithms
are among the supplementary utilities. The package enables fast and precise solutions
for the sophisticated, error-prone and time-consuming estimation procedure of the probability
of informed trading measures, and it is compact in the sense detailed estimation results
can be achieved by solely the use of raw trade level data.


## Installation

----

The easiest way to get PINstimation is the following:

```r
install.packages("PINstimation")
```

To get a bugfix or to use a feature from the development version, you
can install the development version of PINstimation from GitHub.

```r
# install.packages("devtools")
# library(devtools)
devtools::install_github("monty-se/PINstimation")
```

Loading the package

```r
library(PINstimation)
```
<div class="bs-callout bs-callout-primary btn-secondary"> 
<h4 class="alert-heading">Get Started!</h4>
For a smooth introduction to the main functionalities of the package, as well as useful tips how to get started with <strong>PINstimation</strong>, please refer to the section <a href="articles/PINstimation.html" class="alert-link">Get Started</a>.
<hr> 
A detailed description of the package functions is available in <a href="reference/index.html" class="alert-link">online documentation</a>
</div>


## Note to frequent users

----

If you are a frequent user of PINstimation, you might want to avoid repetitively
loading the package PINstimation whenever you open a new R session. You can do
that by adding PINstimation to `.R profile` either manually, or using the function
`load_pinstimation_for_good()`.

To automatically load PINstimation, run `load_pinstimation_for_good()`,
and the following code will be added to your .R profile.

```r
if (interactive()) suppressMessages(require(PINstimation))
```

After restart of the R session, PINstimation will be loaded automatically, whenever a new R
session is started. To remove the automatic loading of PINstimation, just open the
.R profile for editing `usethis::edit_r_profile()`, find the code above, and delete it.

## Resources

----

For a smooth introduction to, and useful tips on the main functionalities of the package, please refer to:

* The sections [Get Started](https://pinstimation.com/articles/PINstimation.html), and  [Online documentation](https://pinstimation.com/reference/index.html") on the package site.
* The package documentation in PDF format is available for download [here](https://pinstimation.com/documents/PINstimation_0.1.0.pdf).
* An overview of the scientific research underlying the package is available [here](https://pinstimation.com/research.html).


## Contributions

----

The package makes a series of original contributions to the literature:

- An **efficient, user-friendly, and comprehensive implementation** of the standard models of probability of informed trading.

- A **first implementation of the estimation of the multilayer probability of informed trading (MPIN)** as developed by Ersan (2016).

- A comprehensive treatment of the **estimation of the adjusted probability of informed trading** as introduced by Duarte and Young (2009). This includes the implementation of the factorization of the AdjPIN likelihood function, various algorithms to generate initial parameter sets, and MLE method.

- The introduction of **the expectation-conditional maximization (ECM) algorithm** as an alternative method to estimate the models of probability of informed trading. The contribution is both theoretical and computational. The theoretical contribution is included in the paper by Ghachem and Ersan (2022b). The implementation of the ECM algorithm allows the estimation of PIN, MPIN, as well as the adjusted PIN model.

- Implementation of three **layer-detection algorithms**, namely of preexistent algorithm of Ersan (2016), as well as two newly developed algorithms, described in Ersan and Ghachem (2022a), and Ghachem and Ersan (2022b), respectively.

- A **first implementation of the estimation of the volume-synchronized probability of informed trading (VPIN)** as introduced by
Easley et al (2011, 2012).

- **One do-it-all function for trade classification** in buyer-initiated or seller-initiated trades that implements the standard algorithms in the field, namely `Tick`, `Quote`, `LR`, and `EMO`.

## Alternative packages

----

To our knowledge, there are two preexisting R packages for the estimation of the probability of informed trading: <a href="https://cran.r-project.org/package=pinbasic" target="_blank">pinbasic</a>, and
<a href="https://cran.r-project.org/package=InfoTrad" target="_blank">InfoTrad</a>.

## Getting help

----

If you encounter a clear bug, please file an issue with a minimal
reproducible example on <a href="https://github.com/monty-se/PINstimation/issues" target="_blank">GitHub</a>.

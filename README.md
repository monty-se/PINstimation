# PINstimation <img src="man/figures/small_logo.png" width="140" height="140" align="right" />

[![R-CMD-check](https://github.com/monty-se/PINstimation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/monty-se/PINstimation/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CRAN](https://www.r-pkg.org/badges/version-ago/PINstimation)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/PINstimation)](https://cranlogs.r-pkg.org/badges/grand-total/PINstimation)

PINstimation provides utilities for the estimation of probability of informed trading models:
original PIN (PIN) in Easley and O'Hara (1992) and Easley et al. (1996); multilayer
PIN (MPIN) in Ersan (2016); Adjusted PIN (AdjPIN) in Duarte and Young (2009); and volume-
synchronized PIN (VPIN) in Easley et al. (2011, 2012). Various computation methods suggested
in the literature are included. Data simulation tools and trade classification algorithms
are among the supplementary utilities. The package enables fast and precise solutions
for the sophisticated, error-prone and time-consuming estimation procedure of the probability
of informed trading measures, and it is compact in the sense detailed estimation results
can be achieved by solely the use of raw trade level data.

## Table of contents
<!--ts-->
* [Main functionalities](#main-functionalities)
* [Installation](#installation)
* [Examples](#examples)
  * [PIN model estimation](#example-1-estimate-the-pin-model)
  * [MPIN model estimation](#example-2-estimate-the-multilayer-pin-model)
  * [AdjPIN model estimation](#example-3-estimate-the-adjusted-pin-model)
  * [VPIN model estimation](#example-4-estimate-the-volume-adjusted-pin-model)
  * [Data classification](#example-5-estimate-the-adjpin-model-using-aggregated-high-frequency-data)
* [Resources](#resources)
* [Note to frequent users](#note-to-frequent-users)
* [Contributions](#contributions)
* [Alternative packages](#alternative-packages)
* [Getting help](#getting-help)
<!--te-->

## Main functionalities

The functionalities that the package offers are summarized below:

* **PIN model**
  * estimate the PIN model using the functions `pin()`, `pin_yz()`, `pin_gwj()`, and `pin_ea()`.
  * compute initial parameter sets using the functions `initials_pin_yz()`, `initials_pin_gwj()`, and `initials_pin_ea()`.
  * generate simulation data following the PIN model using `generatedata_mpin(layers=1)`.
  * evaluate factorizations of the PIN likelihood functions using `fact_pin_eho()`, `fact_pin_lk()`, `fact_pin_e()`.
  * estimate the PIN model by **the Bayesian approach** (Gibbs Sampler) using `pin_bayes()` **(*)** .

* **MPIN model**
  * estimate the MPIN model using the functions `mpin_ml()` and `mpin_ecm()`.
  * compute initial parameter sets using `initials_mpin()`.
  * detect the number of layers in data using `detectlayers_e()`, `detectlayers_eg()`, and `detectlayers_ecm()`.
  * generate simulation data following the MPIN model using `generatedata_mpin()`.
  * evaluate the factorization of the MPIN likelihood function through `fact_mpin()`.

* **AdjPIN model**
  * estimate the AdjPIN model using the function `adjpin()`.
  * compute initial parameter sets using functions `initials_adjpin()`, `initials_adjpin_cl()`, and `initials_adjpin_rnd()`.
  * generate simulation data following the AdjPIN model using `generatedata_adjpin()`.
  * evaluate the factorization of the AdjPIN likelihood function through `fact_adjpin()`.

* **VPIN**
  * estimate the VPIN model using the function `vpin()`

* **Data classification**
  * Classify high-frequency data through `tick`, `quote`, `LR` and `EMO` algorithms using the function `aggregate_trades()`

## Installation

The easiest way to get PINstimation is the following:

```r
install.packages("PINstimation")
```

To get a bugfix or to use a feature from the development version, you
can install the development version of PINstimation from GitHub.

```r
# install.packages("devtools")
# library(devtools)
devtools::install_github("monty-se/PINstimation", build_vignettes = TRUE)
```

Loading the package

```r
library(PINstimation)
```

## Examples

### Example 1: Estimate the PIN model

We estimate the PIN model on preloaded dataset `dailytrades` using the initial parameter sets of Ersan & Alici (2016).

```r
estimate <- pin_ea(dailytrades)
```

```r
## [+] PIN Estimation started 
##   |[1] Likelihood function factorization: Ersan (2016)
##   |[2] Loading initial parameter sets   : 5 EA initial set(s) loaded
##   |[3] Estimating PIN model (1996)      : Using Maximum Likelihood Estimation
##   |+++++++++++++++++++++++++++++++++++++| 100% of PIN estimation completed
## [+] PIN Estimation completed
```

### Example 2: Estimate the Multilayer PIN model

We run the estimation of the MPIN model on preloaded dataset `dailytrades` using:

* the maximum-likelihood method.

```r
ml_estimate <- mpin_ml(dailytrades)
```

```r
## [+] MPIN estimation started
##   |[1] Detecting layers from data       : using Ersan and Ghachem (2022a)
##   |[=] Number of layers in the data     : 3 information layer(s) detected
##   |[2] Computing initial parameter sets : using algorithm of Ersan (2016)
##   |[3] Estimating the MPIN model        : Maximum-likelihood standard estimation
##   |+++++++++++++++++++++++++++++++++++++| 100% of mpin estimation completed
## [+] MPIN estimation completed
```

* the ECM algorithm.

```r
ecm_estimate <- mpin_ecm(dailytrades)
```

```r
## [+] MPIN estimation started
##   |[1] Computing the range of layers    : information layers from 1 to 8
##   |[2] Computing initial parameter sets : using algorithm of Ersan (2016)
##   |[=] Selecting initial parameter sets : max 100 initial sets per estimation
##   |[3] Estimating the MPIN model        : Expectation-Conditional Maximization algorithm
##   |+++++++++++++++++++++++++++++++++++++| 100% of estimation completed [8 layer(s)]
##   |[3] Selecting the optimal model      : using lowest Information Criterion (BIC)
## [+] MPIN estimation completed
```

Compare the aggregate parameters obtained from the ML, and ECM estimations.

```r
mpin_comparison <- rbind(ml_estimate@aggregates, ecm_estimate@aggregates)
rownames(mpin_comparison) <- c("ML", "ECM")
cat("Probabilities of ML, and ECM estimations of the MPIN model\n")
print(mpin_comparison)
```

Display the summary of the model estimates for all number of layers.

```r
summary <- getSummary(ecm_estimate)
show(summary)
```

```r
##          layers em.layers  MPIN Likelihood    AIC    BIC    AWE
## Model[1]      1         1 0.566  -3226.469 6462.9 6473.4 6508.9
## Model[2]      2         2 0.577   -800.379 1616.8 1633.5 1690.3
## Model[3]      3         3 0.574   -643.458 1308.9 1332.0 1410.0
## Model[4]      4         3 0.574   -643.458 1308.9 1332.0 1410.0
## Model[5]      5         3 0.574   -643.458 1308.9 1332.0 1410.0
## Model[6]      6         3 0.574   -643.458 1308.9 1332.0 1410.0
## Model[7]      7         4 0.575   -642.631 1313.3 1342.6 1441.9
## Model[8]      8         4 0.575   -642.631 1313.3 1342.6 1441.9
```

### Example 3: Estimate the Adjusted PIN model

We estimate the adjusted PIN model on preloaded dataset `dailytrades` using `20` initial parameter sets computed by the algorithm of Ersan and Ghachem (2022b).

```r
estimate_adjpin <- adjpin(dailytrades, initialsets = "GE")
show(estimate_adjpin)
```

```r
## [+] AdjPIN estimation started
##   |[1] Computing initial parameter sets : 20 GE initial sets generated
##   |[2] Estimating the AdjPIN model      : Maximum-likelihood Standard Estimation
##   |+++++++++++++++++++++++++++++++++++++| 100% of AdjPIN estimation completed
## [+] AdjPIN estimation completed
```

### Example 4: Estimate the Volume-adjusted PIN model

We run a VPIN estimation on preloaded dataset `hfdata` with `timebarsize` of `5` minutes (`300` seconds).

```r
estimate.vpin <- vpin(hfdata, timebarsize = 300)
show(estimate.vpin)
```

```r
## ----------------------------------
## VPIN estimation completed successfully.
## ----------------------------------
## Type object@vpin to access the VPIN vector.
## Type object@bucketdata to access data used to construct the VPIN vector.
## Type object@dailyvpin to access the daily VPIN vectors.
## 
## [+] VPIN descriptive statistics
## 
## |      | Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.  | NA's |
## |:-----|:-----:|:-------:|:------:|:-----:|:-------:|:-----:|:----:|
## |value | 0.101 |  0.185  | 0.238  | 0.244 |  0.29   | 0.636 |  49  |
## 
## 
## [+] VPIN parameters
## 
## | tbSize | buckets | samplength |   VBS    | #days |
## |:------:|:-------:|:----------:|:--------:|:-----:|
## |  300   |   50    |     50     | 36321.25 |  77   |
## 
## -------
## Running time: 3.753 seconds
```

### Example 5: Estimate the AdjPIN model using aggregated high-frequency data

We use the preloaded high-frequency dataset `hfdata`, prepare it for aggregation.

```r
data <- hfdata
data$volume <- NULL
```

We classify data using the LR algorithm with a time lag of `500` milliseconds (`0.5 s`), using the function `aggregate_data()`.

```r
daytrades <- aggregate_trades(data, algorithm = "LR", timelag = 500)
```

```r
## [+] Trade classification started
##   |[=] Classification algorithm         : LR algorithm
##   |[=] Number of trades in dataset      : 100 000 trades
##   |[=] Time lag of lagged variables     : 500 milliseconds
##   |[1] Computing lagged variables       : using parallel processing
##   |+++++++++++++++++++++++++++++++++++++| 100% of variables computed
##   |[=] Computed lagged variables        : in 7.68 seconds
##   |[2] Computing aggregated trades      : using lagged variables
## [+] Trade classification completed                
```

We use the obtained dataset to estimate the (adjusted) probability of informed trading via the standard Maximum-likelihood method.

```r
adjpin_ml <- adjpin(daytrades, method = "ML", initialsets = "GE")
```

```r
## [+] AdjPIN estimation started
##   |[1] Computing initial parameter sets : 20 GE initial sets generated
##   |[2] Estimating the AdjPIN model      : Maximum-likelihood Standard Estimation
##   |+++++++++++++++++++++++++++++++++++++| 100% of AdjPIN estimation completed
## [+] AdjPIN estimation completed
```

## Note to frequent users

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

For a smooth introduction to, and useful tips on the main functionalities of the package, please refer to:

* The sections [Get Started](https://pinstimation.com/articles/PINstimation.html), and  [Online documentation](https://pinstimation.com/reference/index.html) on the package site.
* The package documentation in PDF format is available for download [here](https://pinstimation.com/documents/PINstimation_0.1.0.pdf).
* An overview of the scientific research underlying the package is available [here](https://pinstimation.com/research.html).

## Contributions

The package makes a series of original contributions to the literature:

* An **efficient, user-friendly, and comprehensive implementation** of the standard models of probability of informed trading.

* A **first implementation of the estimation of the multilayer probability of informed trading (MPIN)** as developed by Ersan (2016).

* A comprehensive treatment of the **estimation of the adjusted probability of informed trading** as introduced by Duarte and Young (2009).
This includes the implementation of the factorization of the AdjPIN likelihood function, various algorithms to generate initial parameter
sets, and MLE method.

* The introduction of **the expectation-conditional maximization (ECM) algorithm** as an alternative method to estimate the models of
probability of informed trading. The contribution is both theoretical and computational. The theoretical contribution is included in the
paper by Ghachem and Ersan (2022b). The implementation of the ECM algorithm allows the estimation of PIN, MPIN, as well as the adjusted PIN model.

* Implementation of three **layer-detection algorithms**, namely of preexistent algorithm of Ersan (2016), as well as two newly developed
algorithms, described in Ersan and Ghachem (2022a), and Ghachem and Ersan (2022b), respectively.

* A **first implementation of the estimation of the volume-synchronized probability of informed trading (VPIN)** as introduced by
Easley et al. (2011, 2012).

* **One do-it-all function for trade classification** in buyer-initiated or seller-initiated trades that implements the standard algorithms
in the field, namely `Tick`, `Quote`, `LR`, and `EMO`.

## Alternative packages

To our knowledge, there are three preexisting R packages for the estimation of models of the probability of informed trading: [pinbasic](https://cran.r-project.org/package=pinbasic), [InfoTrad](https://cran.r-project.org/package=InfoTrad), and [FinAsym](https://cran.r-project.org/package=FinAsym).

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [GitHub](https://github.com/monty-se/PINstimation/issues).

#' @title An R package for estimating the probability of informed trading
#'
#' @description
#' The package provides utilities for the estimation
#' of probability of informed trading measures: original PIN (`PIN`) as
#' introduced by \insertCite{Easley1992;textual}{PINstimation} and
#' \insertCite{Easley1996;textual}{PINstimation}
#' , multilayer PIN (`MPIN`) as introduced by
#' \insertCite{Ersan2016;textual}{PINstimation}, adjusted PIN (`AdjPIN`) model
#' as introduced in \insertCite{Duarte09;textual}{PINstimation}, and
#' volume-synchronized PIN (`VPIN`) as introduced by
#' \insertCite{Easley2011;textual}{PINstimation} and
#' \insertCite{Easley2012;textual}{PINstimation}. Estimations of
#' `PIN`, `MPIN`, and `adjPIN` are subject to floating-point exception
#' error, and are sensitive to the choice of initial values.
#' Therefore, researchers developed factorizations of the model likelihood
#' functions as well as algorithms for determining initial parameter sets for
#' the maximum likelihood estimation - (MLE henceforth).
#' \cr\cr\cr As for the factorizations, the package includes three
#' different factorizations of the `PIN` likelihood function :`fact_pin_eho()`
#' as in \insertCite{Easley2010;textual}{PINstimation}, `fact_pin_lk()` as in
#' \insertCite{WilliamLin2011;textual}{PINstimation}, and `fact_pin_e()` as in
#' \insertCite{Ersan2016;textual}{PINstimation};
#' one factorization for `MPIN` likelihood function: `fact_mpin()` as in
#' \insertCite{Ersan2016;textual}{PINstimation}; and one factorization for
#' `AdjPIN` likelihood function: `fact_adjpin()` as in
#' \insertCite{Ersan2022b;textual}{PINstimation}.
#' \cr\cr The package implements three algorithms to generate initial
#' parameter sets for the MLE of the `PIN` model in: `initials_pin_yz()`
#' for the algorithm of \insertCite{Yan2012;textual}{PINstimation},
#' `initials_pin_gwj()` for the algorithm of
#' \insertCite{Gan2015;textual}{PINstimation}, and `initials_pin_ea()` for the
#' algorithm of \insertCite{ErsanAlici2016;textual}{PINstimation}. As for the
#' initial parameter sets for the MLE of the `MPIN` model, the function
#' `initials_mpin()` implements a multilayer extension of the algorithm of
#' \insertCite{ErsanAlici2016;textual}{PINstimation}. Finally, three functions
#' implement three algorithms of initial parameter sets for the MLE of
#' the `AdjPIN` model, namely `initials_adjpin()` for the algorithm in
#' \insertCite{Ersan2022b;textual}{PINstimation}, `initials_adjpin_cl()`
#' for the algorithm of \insertCite{ChengLai2021;textual}{PINstimation}; and
#' `initials_adjpin_rnd()` for randomly generated initial parameter sets.
#' The choice of the initial parameter sets can be done directly, either using
#' specific functions implementing MLE for the PIN model, such as, `pin_yz()`,
#' `pin_gwj()`, `pin_ea()`; or through the argument `initialsets` in generic
#' functions implementing MLE for the `MPIN` and `AdjPIN` models, namely
#' `mpin_ml()`, and `adjpin()`.
#' Besides, `PIN`, `MPIN` and `AdjPIN` models can be estimated using custom
#' initial parameter set(s) provided by the user and fed through
#' the argument `initialsets` for the functions `pin()`, `mpin_ml()` and
#' `adjpin()`. Through the function `get_posteriors()`, the package also
#' allows users to assign, for each day in the sample, the posterior
#' probability that the day is a no-information day, good-information day
#' and bad-information day.
#' \cr\cr As an alternative to the standard maximum likelihood estimation,
#' estimation via expectation conditional maximization algorithm (`ECM`)
#' is suggested in \insertCite{Ghachem2022;textual}{PINstimation}, and is
#' implemented through the function `mpin_ecm()` for the `MPIN` model, and
#' the function `adjpin()` for the `AdjPIN` model.
#' \cr\cr Dataset(s) of daily aggregated numbers of buys and sells with user
#' determined number of information layers can be simulated with the function
#' `generatedata_mpin()` for the `MPIN` (`PIN`) model;
#' and `generatedata_adjpin()`
#' for the `AdjPIN` model. The output of these functions contains the
#' theoretical parameters used in the data generation, empirical parameters
#' computed from the generated data, alongside the generated data itself.
#' Data simulation functions allow for broad customization
#' to produce data that fit the user's preferences. Therefore, simulated data
#' series can be utilized in comparative analyses for the applied methods in
#' different scenarios. Alternatively, the user can use two example datasets
#' preloaded in the package: `dailytrades` as a representative of a quarterly
#' trade data with daily buys and sells; and `hfdata` as a simulated
#' high-frequency dataset comprising `100 000` trades.
#' \cr\cr Finally, the package provides two functions to deal with
#' high-frequency data.
#' First, the function `vpin()` estimates and provides detailed output on the
#' order flow toxicity metric, volume-synchronized probability of informed
#' trading, as developed in \insertCite{Easley2011;textual}{PINstimation} and
#' \insertCite{Easley2012;textual}{PINstimation}. Second, the function
#' `aggregate_trades()` aggregates the high-frequency trade-data into daily
#' data using several trade classification algorithms, namely the `tick`
#' algorithm, the `quote` algorithm, `LR` algorithm
#' \insertCite{LeeReady1991}{PINstimation} and the `EMO`
#' algorithm \insertCite{Ellis2000}{PINstimation}.
#' \cr\cr The package provides fast, compact, and precise utilities to tackle
#' the sophisticated, error-prone, and time-consuming estimation procedure of
#' informed trading, and this solely using the raw trade-level data.
#' \insertCite{Ghachem2022b;textual}{PINstimation}
#' provides comprehensive overview of the package: it first
#' details the underlying theoretical background, provides a thorough
#' description of the functions, before using them to tackle relevant
#' research questions.
#'
#' @section Functions:
#' \itemize{
#' \item \link{adjpin} estimates the adjusted probability of informed trading
#' (`AdjPIN`) of the model of \insertCite{Duarte09;textual}{PINstimation}.
#' \item \link{aggregate_trades} aggregates the trading data per day using
#' different trade classification algorithms.
#' \item \link{detectlayers_e} detects the number of information layers present
#' in the trade-data using the algorithm in
#' \insertCite{Ersan2016;textual}{PINstimation}.
#' \item \link{detectlayers_eg} detects the number of information layers present
#' in the trade-data using the algorithm in
#' \insertCite{Ersan2022a;textual}{PINstimation}.
#' \item \link{detectlayers_ecm} detects the number of information layers
#' present in the trade-data using the expectation-conditional maximization
#' algorithm in \insertCite{Ghachem2022;textual}{PINstimation}.
#' \item \link{fact_adjpin} returns the `AdjPIN` factorization of the likelihood
#' function by \insertCite{Ersan2022b;textual}{PINstimation} evaluated at the
#' provided data and parameter sets.
#' \item \link{fact_pin_e} returns the `PIN` factorization of the likelihood
#' function by \insertCite{Ersan2016;textual}{PINstimation} evaluated at
#' the provided data and parameter sets.
#' \item \link{fact_pin_eho} returns the `PIN` factorization of the likelihood
#' function by \insertCite{Easley2010;textual}{PINstimation} evaluated at the
#' provided data and parameter sets.
#' \item \link{fact_pin_lk} returns the `PIN` factorization of the likelihood
#' function by \insertCite{WilliamLin2011;textual}{PINstimation} evaluated
#' at the provided data and parameter sets.
#' \item \link{fact_mpin} returns the `MPIN` factorization of the likelihood
#' function by \insertCite{Ersan2016;textual}{PINstimation} evaluated at the
#' provided data and parameter sets.
#' \item \link{generatedata_adjpin} generates a dataset object or a list of
#' dataset objects generated according to the assumptions of the `AdjPIN` model.
#' \item \link{generatedata_mpin} generates a dataset object or a list of
#' dataset objects generated according to the assumptions of the `MPIN` model.
#' \item \link{get_posteriors} computes, for each day in the sample, the
#' posterior probabilities that it is a no-information day, good-information day
#' and bad-information day respectively.
#' \item \link{initials_adjpin} generates the initial parameter sets for the
#' `ML`/`ECM` estimation of the adjusted probability of informed trading using
#' the algorithm of \insertCite{Ersan2022b;textual}{PINstimation}.
#' \item \link{initials_adjpin_cl} generates the initial parameter sets for the
#' `ML`/`ECM` estimation of the adjusted probability of informed trading using
#' an extension of the algorithm of
#' \insertCite{ChengLai2021;textual}{PINstimation}.
#' \item \link{initials_adjpin_rnd} generates random parameter sets for the
#' estimation of the `AdjPIN` model.
#' \item \link{initials_mpin} generates initial parameter sets for the maximum
#' likelihood estimation of the multilayer
#' probability of informed trading (`MPIN`) using the
#' \insertCite{Ersan2016;textual}{PINstimation} generalization of the algorithm
#' in \insertCite{ErsanAlici2016;textual}{PINstimation}.
#' \item \link{initials_pin_ea} generates the initial parameter sets for the
#' maximum likelihood estimation of the probability of informed trading (`PIN`)
#' using the algorithm of \insertCite{ErsanAlici2016;textual}{PINstimation}.
#' \item \link{initials_pin_gwj} generates the initial parameter set for the
#' maximum likelihood estimation of the probability of informed trading (`PIN`)
#' using the algorithm of \insertCite{Gan2015;textual}{PINstimation}.
#' \item \link{initials_pin_yz} generates the initial parameter sets for the
#' maximum likelihood estimation of the probability of informed trading (`PIN`)
#' using the algorithm of \insertCite{Yan2012;textual}{PINstimation}.
#' \item \link{mpin_ecm} estimates the multilayer probability of informed
#' trading (`MPIN`) using the expectation-conditional maximization algorithm
#' (`ECM`) as in \insertCite{Ghachem2022;textual}{PINstimation}.
#' \item \link{mpin_ml} estimates the multilayer probability of informed trading
#' (`MPIN`) using layer detection algorithms in
#' \insertCite{Ersan2016;textual}{PINstimation}, and
#' \insertCite{Ersan2022a;textual}{PINstimation}; and standard maximum
#' likelihood estimation.
#' \item \link{pin} estimates the probability of informed trading (`PIN`) using
#' custom initial parameter set(s) provided by the user.
#' \item \link{pin_bayes} estimates the probability of informed trading (`PIN`) using
#' the Bayesian approach in \insertCite{griffin2021;textual}{PINstimation}.
#' \item \link{pin_ea} estimates the probability of informed trading (`PIN`)
#' using the initial parameter sets from the algorithm of
#' \insertCite{ErsanAlici2016;textual}{PINstimation}.
#' \item \link{pin_gwj} estimates the probability of informed trading (`PIN`)
#' using the initial parameter set from the algorithm of
#' \insertCite{Gan2015;textual}{PINstimation}.
#' \item \link{pin_yz} estimates the probability of informed trading (`PIN`)
#' using the initial parameter sets from the grid-search algorithm of
#' \insertCite{Yan2012;textual}{PINstimation}.
#' \item \link{vpin} estimates the volume-synchronized probability of informed
#' trading (`VPIN`).
#' }
#'
#' @section Datasets:
#' \itemize{
#'   \item \link{dailytrades} A dataframe representative of quarterly (60
#'   trading days) data of simulated daily buys and sells.
#'   \item \link{hfdata} A dataframe containing simulated high-frequency
#'   trade-data on 100 000 timestamps with the variables
#'   `{timestamp, price, volume, bid, ask}`.
#' }
#'
#' @section Estimation results:
#' \itemize{
#'   \item \link{estimate.adjpin-class} The class `estimate.adjpin` stores the
#'   estimation results of the function \code{adjpin()}.
#'   \item \link{estimate.mpin-class} The class `estimate.mpin` stores the
#'   estimation results of the `MPIN` model as estimated by the function
#'   `mpin_ml()`.
#'   \item \link{estimate.mpin.ecm-class} The class `estimate.mpin.ecm` stores
#'   the estimation results of the `MPIN` model as estimated by the function
#'   `mpin_ecm()`.
#'   \item \link{estimate.pin-class} The class `estimate.pin` stores the
#'   estimation results of the following `PIN` functions: \code{pin(), pin_yz(),
#'   pin_gwj()}, and \code{pin_ea()}.
#'   \item \link{estimate.vpin-class} The class `estimate.vpin` stores the
#'   estimation results of the `VPIN` model using the function `vpin()`.
#' }
#'
#' @section Data simulation:
#' \itemize{
#'   \item \link{dataset-class} The class `dataset` stores the result of
#'   simulation of the aggregate daily trading data.
#'   \item \link{data.series-class} The class `data.series` stores a list of
#'   `dataset`.
#' }
#'
#' @author Montasser Ghachem <montasser.ghachem@pinstimation.com> \cr
#' Department of Economics at Stockholm University, Stockholm, Sweden. \cr
#'
#' Oguz Ersan <oguz.ersan@pinstimation.com> \cr
#' Department of International Trade and Finance at Kadir Has University,
#' Istanbul, Turkey.\cr
#'
#' @references
#'
#' \insertAllCited
#'
#' @docType package
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom nloptr neldermead
#' @importFrom methods new show is
#' @importFrom stats aggregate complete.cases quantile cutree dist ppois rpois
#' weighted.mean qpois dpois hclust optim sd setNames runif na.omit qnorm pnorm
#' rbeta rbinom rgamma rmultinom
#' @importFrom utils head read.delim tail
#' @importFrom Rdpack reprompt
#' @importFrom skellam qskellam
#' @importFrom furrr future_map
#' @importFrom future plan multisession sequential
#' @importFrom dplyr %>% summarize group_by
#' @importFrom coda geweke.diag mcmc
#' @import rmarkdown
#'
#'
#' @name PINstimation-package
"_PACKAGE"
NULL

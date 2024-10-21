## - | FILE  HEADER |
##
## Script name:
##    output_classes.R
##
## Purpose of script:
##    Set up S4 classes to contain estimation results for the different
##    PIN models, as well as the output of the data simulation functions.
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2022-11-17
##
## License:
##    GPL 3
##
## Email:
##    montasser.ghachem@pinstimation.com
##
##
##
## S4 classes:
## ++++++++++++++++++
##
## estimate.pin-class:
##    The class estimate.pin is a blueprint of S4 objects
##    that store the results of the different PIN functions:
##    pin(), pin_yz(), pin_gwj(), and pin_ea().
##
## estimate.mpin-class:
##    The class estimate.mpin is the blueprint of S4 objects
##    that store the results of the estimation of the MPIN
##    model, using the function mpin_ml().
##
## estimate.mpin.ecm-class:
##    The class estimate.mpin.ecm is the blueprint of S4 objects
##    that store the results of the estimation of the MPIN model
##    using the Expectation-Conditional Maximization method using
##    the function mpin_ecm().
##
## estimate.adjpin-class:
##    The class estimate.adjpin is a blueprint of the S4 objects
##    that store the results of the estimation of the AdjPIN model
##    using adjpin().
##
## estimate.vpin-class:
##    The class estimate.vpin is a blueprint for S4 objects that
##    store the results of the VPIN estimation method using the
##    function vpin().
##
## dataset-class:
##    The class dataset is a blueprint of S4 objects that store
##    the result of simulation of the aggregate daily trading data.
##
## data.series-class:
##    The class data.series is the blueprint of S4 objects that
##    store a list of dataset objects.
##
## ++++++++++++++++++
##
##
## --
## Package: PINstimation
## website: www.pinstimation.com
## Authors: Montasser Ghachem and Oguz Ersan


##       +++++++++++++++++++++++++
## ++++++| |   PUBLIC CLASSES  | |
##       +++++++++++++++++++++++++

# ---------------------------------------------------------------------------- #
# PIN Classes                                                                  #
# ---------------------------------------------------------------------------- #

#' @title PIN estimation results
#'
#' @description The class \code{estimate.pin} is a blueprint of `S4` objects
#' that store the results of the different `PIN` functions: `pin()`, `pin_yz()`,
#' `pin_gwj()`, and `pin_ea()`.
#'
#' @slot success (`logical`) takes the value \code{TRUE} when the estimation has
#' succeeded, \code{FALSE} otherwise.
#' @slot errorMessage (`character`)  contains an error message if the `PIN`
#' estimation has failed, and is empty otherwise.
#' @slot convergent.sets (`numeric`) returns the number of initial parameter
#' sets at which the likelihood maximization converged.
#' @slot algorithm (`character`) returns the algorithm used to determine the set
#' of initial parameter sets for the maximum likelihood estimation.
#' It takes one of the following values:
#' \itemize{
#'  \item `"YZ"`: Yan and Zhang (2012)
#'  \item `"GWJ"`: Gan, Wei and Johnstone (2015)
#'  \item `"YZ*"`: Yan and Zhang (2012) as modified by Ersan and Alici (2016)
#'  \item `"EA"`: Ersan and Alici (2016)
#'  \item `"CUSTOM"`: Custom initial parameter sets
#' }
#' @slot factorization (`character`) returns the factorization of the `PIN`
#' likelihood function as used in the maximum likelihood estimation.
#' It takes one of the following values:
#' \itemize{
#'  \item `"NONE"`: No factorization
#'  \item `"EHO"`: Easley, Hvidkjaer and O'Hara (2010)
#'  \item `"LK"`: Lin and Ke (2011)
#'  \item `"E"`: Ersan (2016)
#' }
#' @slot parameters (`list`) returns the list of the maximum likelihood
#' estimates (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es)
#' @slot likelihood (`numeric`) returns the value of (the factorization of)
#' the likelihood function evaluated at the optimal set of parameters.
#' @slot pin (`numeric`) returns the value of the probability of informed
#' trading.
#' @slot pin.goodbad (`list`) returns a list containing a decomposition
#' of `PIN` into good-news, and bad-news `PIN` components. The decomposition has
#' been suggested in \insertCite{Brennan2016;textual}{PINstimation}. The list
#' has two elements: `pinG`, and `pinB` are the good-news, and bad-news
#' components of `PIN`, respectively.
#' @slot dataset (`dataframe`) returns the dataset of buys and sells used
#' in the maximum likelihood estimation of the PIN model.
#' @slot initialsets (`dataframe`) returns the initial parameter sets used
#' in the maximum likelihood estimation of the PIN model.
#' @slot details (`dataframe`) returns a dataframe containing the estimated
#' parameters by the `MLE` method for each initial parameter set.
#' @slot runningtime (`numeric`) returns the running time of the estimation
#' of the `PIN` model in seconds.
#'
setClass(
  "estimate.pin", slots = list(
    success = "logical", errorMessage = "character",
    convergent.sets = "numeric", algorithm = "character",
    factorization = "character", parameters = "numeric",
    likelihood = "numeric", pin.goodbad = "list", pin = "numeric",
    dataset = "data.frame", initialsets = "data.frame",
    details = "data.frame", method = "character",
    runningtime = "numeric"
  ),
  prototype = list(
    success = TRUE, errorMessage = "", convergent.sets = 0,
    algorithm = "", factorization = "", parameters = 0,
    likelihood = 0, pin.goodbad = list(), pin = 0,
    dataset = data.frame(), initialsets = data.frame(),
    details = data.frame(), method = "ML", runningtime = 0
  )
)

#' @rdname estimate.pin-class
#' @param  object an object of class \code{estimate.pin}

setMethod(
  "show", signature(object = "estimate.pin"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$pin(object)

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$method)
    ux$show(m = interface$algorithm)
    ux$show(m = interface$factorization)
    ux$show(m = interface$line)
    ux$show(m = interface$initialsets)
    ux$show(c = (object@method == "BAYES"), m = interface$summary)
    ux$show(c = (object@method == "BAYES"), m = interface$markov)

    if (object@success) {

      ux$show(nrow(object@initialsets) > object@convergent.sets,
                   m = interface$failedsets, warning = TRUE)

      # Build table of results
      # ----------------------------------------------------------------
      ux$show(m = interface$badge, skip = FALSE)

      variables <- interface$tablevars
      values <- interface$tablevalues
      results <- data.frame(cbind(variables, values))

      colnames(results) <- interface$tableheaders
      rownames(results) <- NULL
      show(knitr::kable(results, "rst"))

      if (object@method == "BAYES") {
        maximizer <- which(object@details$likelihood == object@likelihood)
        summary <- object@details$summary[[maximizer]]
        show(knitr::kable(
          summary, format = "simple",
          caption = "Summary statistics for the Monte Carlo simulation"))

      }

    } else {

      ux$show(m = interface$error, warning = TRUE)
    }

    ux$show(m = interface$runningtime)
  }
)


# ---------------------------------------------------------------------------- #
# VPIN Classes                                                                 #
# ---------------------------------------------------------------------------- #

#' @title VPIN estimation results
#'
#' @description The class \code{estimate.vpin} is a blueprint for `S4` objects
#' that store the results of the `VPIN` estimation method using the function
#' \code{vpin()}.
#' @slot success (`logical`) returns the value \code{TRUE} when the estimation
#' has succeeded, \code{FALSE} otherwise.
#' @slot errorMessage (`character`) returns an error message if the `VPIN`
#' estimation has failed, and is empty otherwise.
#' @slot improved (`logical`) returns the value \code{TRUE} when the model used
#' is the improved volume-synchronized probability of informed trading of Ke and
#' Lin (2017), and \code{FALSE} when the model used is the volume-synchronized
#' probability of informed trading of Easley et al.(2011,2012).
#' @slot parameters (`numeric`) returns a numeric vector of estimation
#' parameters (tbSize, buckets, samplength, VBS, #days), where `tbSize` is the
#' size of timebars (in seconds); `buckets` is the number of buckets per average
#' volume day; `VBS` is  Volume Bucket Size (daily average volume/number of
#' buckets `buckets`); `samplength` is the length of the window used to estimate
#' `VPIN`; and  `#days` is the number of days in the dataset.

#' @slot bucketdata (`dataframe`) returns the dataframe containing detailed
#' information about buckets. Following the output of
#' \insertCite{abad2012;textual}{PINstimation}, we report for each bucket its
#' identifier (\code{bucket}), the aggregate buy
#' volume (\code{agg.bVol}), the aggregate sell volume (\code{agg.sVol}), the
#' absolute order imbalance (\code{AOI=|agg.bVol-agg.sVol|}),

#' the start time (\code{starttime}), the end time (\code{endtime}), the
#' duration in seconds (\code{duration}) as well as
#' the `VPIN` vector.
#' @slot vpin (`numeric`) returns the vector of the volume-synchronized
#' probabilities of informed trading.
#' @slot ivpin (`numeric`) returns the vector of the improved volume-
#' synchronized probabilities of informed trading as in Ke and Lin (2017).
#' @slot dailyvpin (`dataframe`) returns the daily `VPIN` values. Two
#' variants are provided for any given day: \code{dvpin} corresponds to
#' the unweighted average of vpin values, and \code{dvpin.weighted}
#' corresponds to the average of vpin values weighted by bucket duration.
#' @slot runningtime (`numeric`) returns the running time of the `VPIN`
#' estimation in seconds.
setClass(
  "estimate.vpin",
  slots = list(
    success = "logical", errorMessage = "character", improved = "logical",
    parameters = "numeric", bucketdata = "data.frame", vpin = "numeric",
    ivpin = "numeric", dailyvpin = "data.frame", runningtime = "numeric"
  ),
  prototype = list(
    success = TRUE, errorMessage = "", improved = FALSE, parameters = 0,
    bucketdata = data.frame(), vpin = 0, ivpin = NaN, dailyvpin = data.frame(),
    runningtime = 0
  )
)

#' @rdname estimate.vpin-class
#' @description  The function show() displays a description of the
#' estimate.vpin object: descriptive statistics of the `VPIN` variable,
#' the set of relevant parameters, and the running time.
#' @param  object an object of class \code{estimate.vpin}
setMethod(
  "show", signature(object = "estimate.vpin"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$vpin(object)

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$vpinfunctions)

    if (object@success) {

      ux$show(m = interface$badge, skip = FALSE)

      xsummary <- interface$vpinsummary

      show(knitr::kable(xsummary, "simple", padding = 1L, align = "c",
        caption = interface$summarycaption
      ))

      xparams <- interface$vpinparams

      show(knitr::kable(xparams, "simple", padding = 1L, align = "c",
        caption = interface$paramscaption
      ))

    } else {

      ux$show(m = interface$error, warning = TRUE)
    }

    ux$show(m = interface$runningtime)

  }
)


# ---------------------------------------------------------------------------- #
# MPIN Classes                                                                 #
# ---------------------------------------------------------------------------- #

#' @title MPIN estimation results
#'
#' @description The class \code{estimate.mpin} is the blueprint of `S4` objects
#' that store the results of the estimation of the `MPIN` model, using the
#' function `mpin_ml()`.
#'
#' @slot success (`logical`) returns the value \code{TRUE} when the
#' estimation has succeeded, \code{FALSE} otherwise.
#' @slot errorMessage (`character`) returns an error message if the estimation
#' of the `MPIN` model has failed, and is empty otherwise.
#' @slot convergent.sets (`numeric`) returns the number of initial parameter
#' sets at which the likelihood maximization converged.
#' @slot method (`character`) returns the method of estimation used, and is
#' equal to 'Maximum Likelihood Estimation'.
#' @slot layers (`numeric`) returns the number of layers detected in the trading
#' data, or provided by the user.
#' @slot detection (logical) returns a reference to the layer-detection
#' algorithm used (`"E"`, `"EG"`, `"ECM"`), if any algorithm is used. If the
#' number of layers is provided by the user, detection takes the value `"USER"`.
#' @slot parameters (`list`) returns the list of the maximum likelihood
#' estimates (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es), where
#' \eqn{\alpha}, \eqn{\delta}, and \eqn{\mu} are numeric vectors of length
#' \code{layers}.
#' @slot aggregates (`numeric`) returns an aggregation of information layers'
#' estimated parameters alongside with \eb, and \es. The aggregated parameters
#' are calculated as follows:
#' \eqn{\alpha_{agg} = \sum \alpha_j}{}\if{html}{\eqn{\alpha*= \sum
#' \alpha}\subit{j}}
#' \eqn{\delta_{agg} = \sum \alpha_j \times \delta_j}{}\if{html}{\eqn{\delta*=
#' \sum \alpha}\subit{j}\eqn{\delta}\subit{j}},
#' and \eqn{\mu_{agg} = \sum \alpha_j \times \mu_j}{}\if{html}{\eqn{\mu*= \sum
#' \alpha}\subit{j}\eqn{\mu}\subit{j}}.
#' @slot likelihood (`numeric`) returns the value of the (log-)likelihood
#' function evaluated at the optimal set of parameters.
#' @slot mpinJ (`numeric`) returns the values of the multilayer probability of
#' informed trading per layer, calculated using the layer-specific estimated
#' parameters.
#' @slot mpin (`numeric`) returns the global value of the multilayer probability
#' of informed trading. It is the sum of the multilayer probabilities of
#' informed trading per layer stored in the slot `mpinJ`.
#' @slot mpin.goodbad (`list`) returns a list containing a decomposition of
#' `MPIN` into good-news, and bad-news `MPIN` components. The decomposition
#' has been suggested for PIN measure in
#' \insertCite{Brennan2016;textual}{PINstimation}. The list has four elements:
#' `mpinG`, and `mpinB` are the global good-news, and bad-news components of
#' `MPIN`, while `mpinGj`, and `mpinBj` are two vectors containing  the
#' good-news (bad-news) components of `MPIN` computed per layer.
#' @slot dataset (`dataframe`) returns the dataset of buys and sells used
#' in the maximum likelihood estimation of the MPIN model.
#' @slot initialsets (`dataframe`) returns the initial parameter sets used
#' in the maximum likelihood estimation of the MPIN model.
#' @slot details (`dataframe`) returns a dataframe containing the estimated
#' parameters of the `MLE` method for each initial parameter set.
#' @slot runningtime (`numeric`) returns the running time of the estimation of
#' the `MPIN` model in seconds.
setClass(
  "estimate.mpin",
  slots = list(
    success = "logical", errorMessage = "character",
    convergent.sets = "numeric", method = "character", layers = "numeric",
    detection = "character", parameters = "list", likelihood = "numeric",
    mpin = "numeric", mpinJ = "numeric", mpin.goodbad = "list",
    aggregates = "numeric", parallel = "logical",
    dataset = "data.frame", initialsets = "data.frame",
    details = "data.frame", runningtime = "numeric"
  ),
  prototype = list(
    success = TRUE, errorMessage = "", convergent.sets = 0,
    method = "", layers = 1, detection = "USER", parameters = list(),
    aggregates = 0, likelihood = 0, mpin.goodbad = list(),  mpin = 0,
    mpinJ = 0, parallel = FALSE, dataset = data.frame(),
    initialsets = data.frame(), details = data.frame(),
    runningtime = 0
  )
)

#' @rdname estimate.mpin-class
#' @param  object an object of class \code{estimate.mpin}
setMethod(
  "show", signature(object = "estimate.mpin"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$mpin(object)

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$factorization)
    ux$show(m = interface$method)
    ux$show(m = interface$algorithm)
    ux$show(m = interface$layers)
    ux$show(m = interface$line)
    ux$show(m = interface$initialsets)

    # Display a warning if estimation failed for some initial sets
    ux$show(nrow(object@initialsets) > object@convergent.sets,
            m = interface$failedsets, warning = TRUE)

    ux$show(m = interface$badge, skip = FALSE)
    ux$show(m = interface$parallel, skip = TRUE)


    if (object@success) {

      variables <- interface$tablevars
      values <- interface$tablevalues
      results <- data.frame(cbind(variables, values))

      colnames(results) <- interface$tableheaders
      rownames(results) <- NULL
      show(knitr::kable(results, "rst"))

    } else {

      ux$show(m = interface$error, warning = TRUE)
    }

    ux$show(m = interface$runningtime)
  }
)



#' @title MPIN estimation results (ECM)
#'
#' @description The class \code{estimate.mpin.ecm} is the blueprint of
#' `S4` objects that store the results of the estimation of the `MPIN`
#' model using the Expectation-Conditional Maximization method, as
#' implemented in the function \code{mpin_ecm()}.
#'
#' @slot success (`logical`) returns the value \code{TRUE} when the
#' estimation has succeeded, \code{FALSE} otherwise.
#' @slot errorMessage (`character`) returns an error message if the `MPIN`
#' estimation has failed, and is empty otherwise.
#' @slot convergent.sets (`numeric`) returns the number of initial parameter
#' sets at which the likelihood maximization converged.
#' @slot method (`character`) returns the method of estimation, and is equal
#' to 'Expectation-Conditional Maximization Algorithm'.
#' @slot layers (`numeric`) returns the number of layers estimated by the
#' Expectation-Conditional Maximization algorithm, or provided by the user.
#' @slot optimal (`logical`) returns whether the number of layers used for
#' the estimation is provided by the user \code{(optimal=FALSE)}, or determined
#' by the `ECM` algorithm \code{(optimal=TRUE)}.
#' @slot parameters (`list`) returns the list of the maximum likelihood
#' estimates (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es), where
#' \eqn{\alpha}, \eqn{\delta}, and \eqn{\mu} are numeric vectors of
#' length \code{layers}.
#' @slot aggregates (`numeric`) returns an aggregation of information layers'
#' parameters alongside with \eb and \es. The aggregated parameters are
#' calculated as follows:
#' \eqn{\alpha_{agg} = \sum \alpha_j}{}\if{html}{\eqn{\alpha*= \sum
#' \alpha}\subit{j}} \eqn{\delta_{agg} = \sum \alpha_j \times \delta_j}{}
#' \if{html}{\eqn{\delta*= \sum \alpha}\subit{j}\eqn{\delta}\subit{j}},
#' and \eqn{\mu_{agg} = \sum \alpha_j \times \mu_j}{}\if{html}{\eqn{\mu*= \sum
#' \alpha}\subit{j}\eqn{\mu}\subit{j}}.
#' @slot likelihood (`numeric`) returns the value of the (log-)likelihood
#' function evaluated at the optimal set of parameters.
#' @slot mpinJ (`numeric`) returns the values of the multilayer probability of
#' informed trading per layer, calculated using the layer-specific estimated
#' parameters.
#' @slot mpin (`numeric`) returns the global value of the multilayer probability
#' of informed trading. It is the sum of the multilayer probabilities of
#' informed trading per layer stored in the slot `mpinJ`.
#' @slot mpin.goodbad (`list`) returns a list containing a decomposition of
#' `MPIN` into good-news, and bad-news `MPIN` components. The decomposition
#' has been suggested for PIN measure in
#' \insertCite{Brennan2016;textual}{PINstimation}. The list has four elements:
#' `mpinG`, and `mpinB` are the global good-news, and bad-news components of
#' `MPIN`, while `mpinGj`, and `mpinBj` are two vectors containing  the
#' good-news (bad-news) components of `MPIN` computed per layer.
#' @slot dataset (`dataframe`) returns the dataset of buys and sells used
#' in the ECM estimation of the MPIN model.
#' @slot initialsets (`dataframe`) returns the initial parameter sets used
#' in the ECM estimation of the MPIN model.
#' @slot details (`dataframe`) returns a dataframe containing the estimated
#' parameters of the `ECM` method for each initial parameter set.
#' @slot models (`list`) returns the list of `estimate.mpin.ecm` objects
#' storing the results of estimation using the function `mpin_ecm()` for
#' different values of the argument `layers`. It returns `NULL` when the
#' argument `layers` of the function `mpin_ecm()` take a specific value.
#' @slot AIC (`numeric`) returns the value of the Akaike Information Criterion
#' (AIC).
#' @slot BIC (`numeric`) returns the value of the Bayesian Information Criterion
#' (BIC).
#' @slot AWE (`numeric`) returns the value of the Approximate Weight of
#' Evidence.
#' @slot criterion (`character`) returns the model selection criterion used to
#' find the optimal estimate for the `MPIN` model. It takes one of these values
#' `'BIC'`, `'AIC'`, `'AWE'`; which stand for Bayesian Information Criterion,
#' Akaike Information Criterion, and Approximate Weight of Evidence,
#' respectively.
#' @slot hyperparams (`list`) returns the hyperparameters of the `ECM`
#' algorithm, which are `minalpha`, `maxeval`, `tolerance`, and `maxlayers`.
#' Check the details section of \code{mpin_ecm()} to know more about these
#' parameters.
#' @slot runningtime (`numeric`) returns the running time of the estimation
#' in seconds.
setClass(
  "estimate.mpin.ecm",
  contains = "estimate.mpin",
  slots = list(
    AIC = "numeric", BIC = "numeric", AWE = "numeric",
    optimal = "numeric", criterion = "character",
    hyperparams = "list", models = "list"
  ), prototype = list(
    AIC = 0, BIC = 0, AWE = 0, optimal = 0,
    criterion = "BIC", hyperparams = list(),
    models = list()
  )
)


#' @rdname estimate.mpin.ecm-class
#' @param  object an object of class \code{estimate.mpin.ecm}
setMethod(
  "show", signature(object = "estimate.mpin.ecm"), function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$mpin(object)

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$factorization)
    ux$show(m = interface$method)
    ux$show(m = interface$algorithm)
    ux$show(m = interface$layers)
    ux$show(m = interface$criterion)
    ux$show(m = interface$line)
    ux$show(m = interface$eminitialsets)
    ux$show(object@success && !object@optimal &&
              nrow(object@initialsets) > object@convergent.sets,
            m = interface$failedsets, warning = TRUE)
    ux$show(m = interface$emfunctions)

    if (object@success) {

      # Display a warning if estimation failed for some initial sets
      ux$show(m = interface$badge, skip = FALSE)
      ux$show(m = interface$header, skip = FALSE)
      ux$show(m = interface$parallel, skip = FALSE)

      variables <- interface$tablevars
      values <- interface$tablevalues
      results <- data.frame(cbind(variables, values))

      colnames(results) <- interface$tableheaders
      rownames(results) <- NULL

      show(knitr::kable(results, "rst", padding = 1L))

      if (object@optimal)
        show(knitr::kable(interface$summary, "simple",
                          caption = interface$tablecaption,
                          padding = 0L, align = "ccccrr"))

    } else {

      ux$show(m = interface$emerror, warning = TRUE)
    }

    ux$show(m = interface$runningtime)
  }

)



#' @rdname estimate.mpin.ecm-class
#' @param object an object of class \code{estimate.mpin.ecm}.
#' @param criterion a character string specifying the model selection criterion.
#' `criterion` should take one of these values `{"BIC", "AIC", "AWE"}`.
#' They stand for Bayesian Information Criterion, Akaike Information Criterion,
#' and Approximate Weight of Evidence, respectively.
setGeneric(name = "selectModel", def = function(object, criterion)
  standardGeneric("selectModel"))



#' @docType methods
#' @describeIn estimate.mpin.ecm-class returns the optimal model among
#' the estimated models, i.e., the model having the lowest information
#' criterion, provided by the user.
#' @export
setMethod(f = "selectModel", signature = "estimate.mpin.ecm",
          definition = function(object, criterion)
            return(.xmpin$selectModel(object, criterion))
)



#' @rdname estimate.mpin.ecm-class
#' @param object an object of class \code{estimate.mpin.ecm}.
setGeneric(name = "getSummary",
           def = function(object) standardGeneric("getSummary"))



#' @docType methods
#' @describeIn estimate.mpin.ecm-class returns a summary of
#' the estimation of the `MPIN` model using the `ECM` algorithm for different
#' values of the argument `layers`. For each estimation, the number of layers,
#' the `MPIN` value, the log-likelihood value, as well as the values of the
#' different information criteria, namely `AIC`, `BIC` and `AWE` are displayed.
#' @export
setMethod(f = "getSummary", signature = "estimate.mpin.ecm",
  definition = function(object) {

    interface <- uiclasses$emsummary(object)
    is_optimal <- !(object@detection == "ECM")
    if (is_optimal) return(ux$show(m = interface$nothing))

    xsummary <- interface$tablevalues
    colnames(xsummary) <- interface$tableheaders
    rownames(xsummary) <- interface$tablerows

    xsummary <- data.frame(xsummary)

    return(xsummary)
  }
)


# ---------------------------------------------------------------------------- #
# Data Generation Classes                                                      #
# ---------------------------------------------------------------------------- #

#' @title List of dataset objects
#'
#' @description The class `data.series` is the blueprint of `S4` objects that
#' store a list of `dataset` objects.
#'
#' @slot series (`numeric`) returns the number of `dataset` objects stored.
#' @slot days (`numeric`) returns the length of the simulated data in days
#' common to all `dataset` objects stored. The default value is \code{60}.
#' @slot model (`character`) returns a character string, either `'MPIN'` or
#' `'adjPIN'`.
#' @slot layers (`numeric`)  returns the number of information layers in
#' all `dataset` objects stored. It takes the value `1` for the adjusted PIN
#' model, i.e. when `model` takes the value `'adjPIN'`.
#' @slot datasets (`list`) returns the list of the `dataset` objects stored.
#' @slot restrictions (`list`) returns a binary list that contains the set of
#' parameter restrictions on the original AdjPIN model in the estimated AdjPIN
#' model. The restrictions are imposed equality constraints on model parameters.
#' If the value of the parameter  `restricted` is the empty list `(list())`,
#' then the model has no restrictions, and the estimated model is the
#' unrestricted, i.e., the original AdjPIN model. If not empty, the list
#' contains one or multiple of the following four elements
#' `{theta, mu, eps, d}`. For instance, If `theta` is set to `TRUE`,
#' then the estimated model has assumed the equality of the probability of
#' liquidity shocks in no-information, and information days, i.e.,
#' \thetaB`=`\thetaS. If any of the remaining rate elements
#' `{mu, eps, d}` is equal to `TRUE`, (say  `mu=TRUE`), then the
#' estimated model imposed equality of the concerned parameter on the buy
#' side, and on the sell side (\mub`=`\mus). If more than one element is
#' equal to `TRUE`,  then the restrictions are combined. For instance,
#' if  the slot `restrictions` contains `list(theta=TRUE, eps=TRUE, d=TRUE)`,
#' then the estimated AdjPIN model has three restrictions \thetaB`=`\thetaS,
#' \eb`=`\es, and \Db`=`\Ds, i.e., it has been estimated with just `7`
#' parameters, in comparison to `10` in the original unrestricted model.
#' `[i]` This slot only concerns datasets generated by the function
#' `generatedata_adjpin()`.
#' @slot warnings (`numeric`) returns numbers referring to the warning errors
#' caused by a conflict between the different arguments used to call the
#' function `generatedata_mpin()`.
#' @slot runningtime (`numeric`) returns the running time of the data
#' simulation in seconds.
#'
#'
setClass(
  "data.series",
  slots = list(
    series = "numeric", days = "numeric", model = "character",
    layers = "numeric", datasets = "list", warnings = "numeric",
    restrictions = "list", runningtime = "numeric"
  ),
  prototype = list(
    series = 1, days = 60, model = "MPIN", layers = 1, datasets = list(),
    warnings = numeric(0), restrictions = list(), runningtime = 0
  )
)


#' @rdname data.series-class
#' @param  object an object of class \code{data.series}
setMethod(
  "show", signature(object = "data.series"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")


    interface <- uiclasses$dataset(object)

    is_mpin <- (object@model == "MPIN")

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$model)
    ux$show(is_mpin, m = interface$layers)
    ux$show(!is_mpin, m = interface$restrictions)
    ux$show(m = interface$datasets)
    ux$show(m = interface$days)
    ux$show(m = interface$line)
    ux$show(m = interface$getdata)
    ux$show(m = interface$badge, skip = FALSE)
    ux$show(m = interface$runningtime)

    # Only show if the object has warnings
    uiconflicts <- (sum(object@warnings) > 0)

    if (uiconflicts) {
      ux$show(m = interface$line, warning = TRUE)
      ux$show(m = interface$warnings, warning = TRUE)
    }

  }
)



#' @title Simulated data object
#'
#' @description The class `dataset` is a blueprint of `S4` objects that store
#' the result of simulation of the aggregate daily trading data.
#'
#' @slot model (`character`) returns the model being simulated, either `"MPIN"`,
#' or `"adjPIN"`.
#' @slot days (`numeric`) returns the length of the generated data in days.
#' @slot layers (`numeric`)  returns the number of information layers in the
#' simulated data. It takes the value `1` for the adjusted PIN
#' model, i.e. when `model` takes the value `'adjPIN'`.
#' @slot theoreticals (`list`) returns the list of the theoretical parameters
#' used to generate the data.
#' @slot empiricals (`list`) returns the list of the empirical parameters
#' computed from the generated data.
#' @slot aggregates (`numeric`) returns an aggregation of information layers'
#' empirical parameters alongside with \eb and \es. The aggregated parameters
#' are calculated as follows:
#' \eqn{\alpha_{agg} = \sum \alpha_j}{}\if{html}{\eqn{\alpha*= \sum
#' \alpha}\subit{j}} \eqn{\delta_{agg} = \sum \alpha_j \times \delta_j}{}
#' \if{html}{\eqn{\delta*= \sum \alpha}\subit{j}\eqn{\delta}\subit{j}},
#' and \eqn{\mu_{agg} = \sum \alpha_j \times \mu_j}{}\if{html}{\eqn{\mu*= \sum
#' \alpha}\subit{j}\eqn{\mu}\subit{j}}.
#' @slot emp.pin (`numeric`) returns the `PIN/MPIN/AdjPIN` value derived from
#' the empirically estimated parameters of the generated data.
#' @slot data (`dataframe`) returns a dataframe containing the generated data.
#' @slot likelihood (`numeric`) returns the value of the (log-)likelihood
#' function evaluated at the empirical parameters.
#' @slot warnings (`character`) stores warning messages for events that occurred
#' during the data generation, such as conflict between two arguments.
#'
#' @slot restrictions (`list`) returns a binary list that contains the set of
#' parameter restrictions on the original AdjPIN model in the estimated AdjPIN
#' model. The restrictions are imposed equality constraints on model parameters.
#' If the value of the parameter  `restricted` is the empty list `(list())`,
#' then the model has no restrictions, and the estimated model is the
#' unrestricted, i.e., the original AdjPIN model. If not empty, the list
#' contains one or multiple of the following four elements
#' `{theta, mu, eps, d}`. For instance, If `theta` is set to `TRUE`,
#' then the estimated model has assumed the equality of the probability of
#' liquidity shocks in no-information, and information days, i.e.,
#' \thetaB`=`\thetaS. If any of the remaining rate elements
#' `{mu, eps, d}` is equal to `TRUE`, (say  `mu=TRUE`), then the
#' estimated model imposed equality of the concerned parameter on the buy
#' side, and on the sell side (\mub`=`\mus). If more than one element is
#' equal to `TRUE`,  then the restrictions are combined. For instance,
#' if  the slot `restrictions` contains `list(theta=TRUE, eps=TRUE, d=TRUE)`,
#' then the estimated AdjPIN model has three restrictions \thetaB`=`\thetaS,
#' \eb`=`\es, and \Db`=`\Ds, i.e., it has been estimated with just `7`
#' parameters, in comparison to `10` in the original unrestricted model.
#' `[i]` This slot only concerns datasets generated by the function
#' `generatedata_adjpin()`.
#'
#' @details `theoreticals` are the parameters used to generate the daily buys
#' and sells. `empiricals` are computed from the generated daily buys and sells.
#' If we generate data for a 60 days using \eqn{\alpha}=0.1, the most likely
#' outcome is to obtain 6 days (0.1 x 60) as
#' information event days. In this case, the theoretical value of
#' \eqn{\alpha}`=0.1` is equal to the empirically estimated value of
#' \eqn{\alpha}`=6/60=0.1`.
#' The number of generated information days can, however, be different from `6`;
#' say `5`. In this case, empirical (actual) \eqn{\alpha} parameter derived
#' from the generated numbers would be `5/60=0.0833`, which differs from the
#' theoretical \eqn{\alpha}`=0.1`.
#' The weak law of large numbers ensures the empirical parameters (`empiricals`)
#' converge towards the theoretical parameters (`theoreticals`) when the number
#' of days becomes very large.
#' To detect the estimation biases from the models/methods, comparing the
#' estimates with `empiricals` rather than `theoreticals` would yield more
#' realistic results.
setClass(
  "dataset",
  slots = list(
    model = "character", days = "numeric", layers = "numeric",
    theoreticals = "list", empiricals = "list", emp.pin = "numeric",
    data = "data.frame", aggregates = "list", likelihood = "numeric",
    restrictions = "list", warnings = "list", runningtime = "numeric"
  ),
  prototype = list(
    model = "MPIN", days = 60, layers = 1, theoreticals = list(),
    empiricals = list(), emp.pin = 0, data = data.frame(),
    aggregates = list(), likelihood = 0, restrictions = list(),
    warnings = list(), runningtime = 0
  )
)

#' @rdname dataset-class
#' @param  object an object of class \code{dataset}
setMethod(
  "show", signature(object = "dataset"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$dataset(object)

    is_mpin <- (object@model == "MPIN")

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$model)
    ux$show(is_mpin, m = interface$layers)
    ux$show(!is_mpin, m = interface$restrictions)
    ux$show(m = interface$days)
    ux$show(m = interface$line)
    ux$show(m = interface$getdata)

    ux$show(m = interface$badge, skip = FALSE)

    variables <- interface$tablevars
    values <- interface$tablevalues
    results <- data.frame(cbind(variables, values))
    colnames(results) <- interface$tableheaders
    rownames(results) <- NULL

    show(knitr::kable(results, "rst"))

    ux$show(m = interface$runningtime)

    # Only show if the object has warnings
    uiconflicts <- (length(object@warnings$ids) > 0)

    if (uiconflicts) {
      ux$show(m = interface$line, warning = TRUE)
      ux$show(m = interface$warnings, warning = TRUE)
    }

  }
)



# ---------------------------------------------------------------------------- #
# adjPIN Model Classes                                                         #
# ---------------------------------------------------------------------------- #

#' @title AdjPIN estimation results
#'
#' @description The class \code{estimate.adjpin} is a blueprint of the `S4`
#' objects that store the results of the estimation of the `AdjPIN` model using
#' \code{adjpin()}.
#'
#' @slot success (`logical`) takes the value \code{TRUE} when the estimation has
#' succeeded, \code{FALSE} otherwise.
#' @slot errorMessage (`character`)  contains an error message if the estimation
#' of the `AdjPIN` model has failed, and is empty otherwise.
#' @slot convergent.sets (`numeric`) returns the number of initial parameter
#' sets, for which the likelihood maximization converged.
#' @slot method (`character`) contains a reference to the estimation method:
#' `"ECM"` for expectation-conditional maximization algorithm and '`"ML"`'
#' for standard maximum likelihood estimation.
#' @slot factorization (`character`) contains a reference to the factorization
#' of the likelihood function used: `"GE"`for the factorization in
#' \insertCite{Ersan2022b;textual}{PINstimation}, and `"NONE"` for the
#' original likelihood function in \insertCite{Duarte09;textual}{PINstimation}.
#'
#' @slot restrictions (`list`) returns a binary list that contains the set of
#' parameter restrictions on the original AdjPIN model in the estimated AdjPIN
#' model. The restrictions are imposed equality constraints on model parameters.
#' If the value of the parameter  `restricted` is the empty list `(list())`,
#' then the model has no restrictions, and the estimated model is the
#' unrestricted, i.e., the original AdjPIN model. If not empty, the list
#' contains one or multiple of the following four elements
#' `{theta, mu, eps, d}`. For instance, If `theta` is set to `TRUE`,
#' then the estimated model has assumed the equality of the probability of
#' liquidity shocks in no-information, and information days, i.e.,
#' \thetaB`=`\thetaS. If any of the remaining rate elements
#' `{mu, eps, d}` is equal to `TRUE`, (say  `mu=TRUE`), then the
#' estimated model imposed equality of the concerned parameter on the buy
#' side, and on the sell side (\mub`=`\mus). If more than one element is
#' equal to `TRUE`,  then the restrictions are combined. For instance,
#' if  the slot `restrictions` contains `list(theta=TRUE, eps=TRUE, d=TRUE)`,
#' then the estimated AdjPIN model has three restrictions \thetaB`=`\thetaS,
#' \eb`=`\es, and \Db`=`\Ds, i.e., it has been estimated with just `7`
#' parameters, in comparison to `10` in the original unrestricted model.
#'
#' @slot algorithm (`character`) returns the implemented initial parameter
#' set determination algorithm. `"GE"` is for
#' \insertCite{Ersan2022b;textual}{PINstimation},
#' `"CL"` is for \insertCite{ChengLai2021;textual}{PINstimation},
#' `"RANDOM"` for random initial parameter sets, and `"CUSTOM"` for
#' custom initial parameter sets.
#' @slot parameters (`numeric`) returns the vector of the optimal
#' maximum-likelihood estimates ( \eqn{\alpha}, \eqn{\delta}, \eqn{\theta},
#' \eqn{\theta'}, \eb, \es, \mub, \mus, \Db, \Ds).
#' @slot likelihood (`numeric`) returns the value (of the factorization) of the
#' likelihood function, as in \insertCite{Ersan2022b;textual}{PINstimation},
#' evaluated at the set of optimal parameters.
#' @slot adjpin (`numeric`) returns the value of the adjusted probability of
#' informed trading \insertCite{Duarte09}{PINstimation}.

#' @slot psos (`numeric`) returns the probability of symmetric order flow shock
#' \insertCite{Duarte09}{PINstimation}.
#' @slot dataset (`dataframe`) returns the dataset of buys and sells used
#' in the estimation of the AdjPIN model.
#' @slot initialsets (`dataframe`) returns the initial parameter sets used
#' in the estimation of AdjPIN model.
#' @slot details (`dataframe`) returns a dataframe containing the estimated
#' parameters for each initial parameter set.
#' @slot hyperparams (`list`) returns the hyperparameters of the `ECM`
#' algorithm, which are `maxeval`, and `tolerance`.
#' @slot runningtime (`numeric`) returns the running time of the `AdjPIN`
#' estimation in seconds.
#'
setClass(
  "estimate.adjpin",
  slots = list(
    success = "logical", errorMessage = "character", method = "character",
    factorization = "character", restrictions = "list", algorithm = "character",
    parameters = "numeric", likelihood = "numeric", adjpin = "numeric",
    psos = "numeric", dataset = "data.frame", initialsets = "data.frame",
    details = "data.frame", hyperparams = "list", runningtime = "numeric",
    convergent.sets = "numeric"
  ),
  prototype = list(
    success = TRUE, errorMessage = "", method = "ML", factorization = "GE",
    adjpin = 0, psos = 0, hyperparams = list(), restrictions = list(),
    algorithm = "GE", parameters = 0, likelihood = 0, convergent.sets = 0,
    dataset = data.frame(), initialsets = data.frame(),
    details = data.frame(), runningtime = 0
  )
)

#' @rdname estimate.adjpin-class
#' @param object (estimate.adjpin-class)
#' @export
setMethod(
  "show", signature(object = "estimate.adjpin"),
  function(object) {

    # load the digits for display of decimals
    digits <- getOption("PIN-digits")

    interface <- uiclasses$adjpin(object)

    ux$show(m = interface$line)
    ux$show(m = interface$outcome)
    ux$show(m = interface$line)
    ux$show(m = interface$factorization)
    ux$show(m = interface$method)
    ux$show(m = interface$algorithm)
    ux$show(m = interface$restrictions)
    ux$show(m = interface$line)
    ux$show(m = interface$initialsets)


    if (object@success) {

      # Display a warning if estimation failed for some initial sets
      ux$show(nrow(object@initialsets) > object@convergent.sets,
                   m = interface$failedsets, warning = TRUE)


      ux$show(m = interface$badge, skip = FALSE)

      variables <- interface$tablevars
      values <- interface$tableparams
      results <- data.frame(cbind(variables, values))
      colnames(results) <- interface$tableheaders
      rownames(results) <- NULL
      show(knitr::kable(results, "rst", padding = 1L, align = "ll"))

    } else {

      ux$show(m = interface$error, warning = TRUE)
    }

    ux$show(m = interface$runningtime)
  }

)

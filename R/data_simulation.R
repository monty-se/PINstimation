## - | FILE  HEADER |
##
## Script name:
##    data_simulation.R
##
## Purpose of script:
##    Implement custom algorithms for data simulation for both the
##    MPIN model, and the AdjPIN model
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2022-05-26
##
## License:
##    GPL 3
##
## Email:
##    montasser.ghachem@pinstimation.com
##
## Copyright (c) Montasser Ghachem, 2022
##
## Public functions:
## ++++++++++++++++++
##
## generatedata_mpin():
##    Generates a dataset object or a data.series object storing
##    simulation parameters as well as aggregate daily buys and
##    sells simulated following the assumption of the MPIN model
##    of Ersan (2016).
##
## generatedata_adjpin():
##    Generates a dataset object or a data.series object storing
##    simulation parameters as well as aggregate daily buys and
##    sells simulated following the assumption of the AdjPIN model
##    of Duarte and Young (2009).
##
## ++++++++++++++++++
##
##
## --
## Package: PINstimation
## website: www.pinstimation.com
## Authors: Montasser Ghachem and Oguz Ersan


##       +++++++++++++++++++++++++
## ++++++| |  PUBLIC FUNCTIONS | |
##       +++++++++++++++++++++++++


#' @title Simulation of MPIN model data
#'
#' @description Generates a `dataset` object or a `data.series` object (a list
#' of `dataset` objects) storing simulation parameters as well as aggregate
#' daily buys and sells simulated following the assumption of the `MPIN` model
#' of \insertCite{Ersan2016}{PINstimation}.
#'
#' @usage generatedata_mpin(series = 1, days = 60, layers = NULL,
#'                          parameters = NULL, ranges = list(), ...,
#'                          verbose = TRUE)
#'
#' @param series The number of datasets to generate.
#' @param days The number of trading days for which aggregated buys and
#' sells are generated. Default value is \code{60}.
#' @param layers The number of information layers to be included in the
#' simulated data. Default value is \code{NULL}.
#' If `layers` is omitted or set to `NULL`, the number of layers is uniformly
#' selected from the set \code{{1, ..., maxlayers}}.
#' @param parameters A vector of model parameters of size \code{3J+2}
#' where \code{J} is the number of information layers and it has the
#' following form
#' \{\ifelse{html}{\eqn{\alpha}\subit{1}}{\eqn{\alpha_1}{}},
#' ...,\ifelse{html}{\eqn{\alpha}\subit{J}}{\eqn{\alpha_J}{}},
#' \ifelse{html}{\eqn{\delta}\subit{1}}{\eqn{\delta_1}{}},...,
#' \ifelse{html}{\eqn{\delta}\subit{J}}{\eqn{\delta_J}{}},
#' \ifelse{html}{\eqn{\mu}\subit{1}}{\eqn{\mu_1}{}},...,
#' \ifelse{html}{\eqn{\mu}\subit{J}}{\eqn{\mu_J}{}}, \eb, \es\}.
#' @param ranges A list of ranges for the different simulation
#' parameters having named elements \eqn{\alpha}, \eqn{\delta}, \eb, \es,
#' and \eqn{\mu}.
#' The value of each element is a vector of two numbers: the first one is the
#' minimal value `min_v` and the second one is the maximal value `max_v`.
#' If the element corresponding to a given parameter is missing, the default
#' range for that parameter is used. If the argument `ranges` is an empty
#' list and `parameters` is `NULL`, the default ranges for the parameters
#' are used. The simulation parameters are uniformly drawn from the interval
#' (`min_v`, `max_v`) for the specified parameters.
#' The default value is `list()`.
#'
#' @param ... Additional arguments passed on to the function
#'  `generatedata_mpin()`. The recognized arguments are `confidence`,
#'  `maxlayers`, `eps_ratio`, `mu_ratio`.
#' \itemize{
#' \item `confidence` (`numeric`) denotes the range of the confidence interval
#' associated with each layer such that all observations within the layer `j`
#' lie in the theoretical confidence interval of the Skellam distribution
#' centered on the mean order imbalance, at the level `'confidence'`.
#' The default value is `0.99`.
#' \item `maxlayers` (`integer`) denotes the upper limit of number of layers
#' for the generated datasets. If the argument `layers` is missing, the layers
#' of the simulated datasets will be uniformly drawn from
#' \code{{1,..., maxlayers}}. When missing, `maxlayers` takes the default
#' value of `5`.
#' \item `eps_ratio` (`numeric`) specifies the admissible range for the value
#' of the ratio \es/\eb, It can be a two-value vector or just a single value.
#' If `eps_ratio` is a vector of two values: the first one is the minimal value
#' and the second one is the maximal value; and the function tries to generate
#' \es and \eb satisfying that their ratios \es/\eb lies within the interval
#' `eps_ratio`. If `eps_ratio` is a single number, then the function tries to
#' generate \es and \eb satisfying \es = \eb x `eps_ratio`. If this range
#' conflicts with other arguments such as `ranges`, a warning is displayed.
#' The default value is `c(0.75, 1.25)`.
#' \item `mu_ratio` (`numeric`) it is the minimal value of the ratio between
#' two consecutive values of the vector `mu`. If `mu_ratio = 1.25` e.g., then
#' \ifelse{html}{\eqn{\mu}\subit{j+1}}{\eqn{\mu_{j+1}}{}} should be larger than
#' `1.25`* \ifelse{html}{\eqn{\mu}\subit{j}}{\eqn{\mu_{j}}{}} for all
#' `j = 1, .., J`. If `mu_ratio` conflicts with other arguments such as `ranges`
#' or `confidence`, a warning is displayed. The default value is `NULL`.
#' }
#'
#' @param verbose (`logical`) a binary variable that determines whether detailed
#' information about the progress of the data generation is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @return Returns an object of class \code{dataset} if `series=1`, and an
#' object of class `data.series` if `series>1`.
#'
#' @details
#' An information layer refers to a given type of information event existing
#' in the data. The `PIN` model assumes a single type of information events
#' characterized by three parameters for \eqn{\alpha}, \eqn{\delta}, and
#' \eqn{\mu}. The `MPIN` model relaxes the assumption, by relinquishing the
#' restriction on the number of information event types. When `layers = 1`,
#' generated data fit the assumptions of the `PIN` model.
#'
#' If the argument `parameters` is missing, then the simulation parameters are
#' generated using the ranges specified in the argument `ranges`.
#' If the argument `ranges` is `list()`, default ranges are used. Using the
#' default ranges, the simulation parameters are obtained using the following
#' procedure:
#' \itemize{
#' \item \eqn{\alpha()}: a vector of length `layers`, where each
#' \ifelse{html}{\eqn{\alpha}\subit{j}}{\eqn{\alpha_j}{}} is uniformly
#' distributed on \code{(0, 1)} subject to the condition:
#' \ifelse{html}{\eqn{\sum \alpha}\subit{j}\eqn{< 1}}{
#' \eqn{\sum_j \alpha_j \leq1}{}}.
#' \item \eqn{\delta()}: a vector of length `layers`, where each
#' \ifelse{html}{\eqn{\delta}\subit{j}}{\eqn{\delta_j}{}} uniformly distributed
#' on \code{(0, 1)}.
#' \item \eqn{\mu()}: a vector of length `layers`, where each
#' \ifelse{html}{\eqn{\mu}\subit{j}}{\eqn{\mu_j}{}} is uniformly distributed
#' on the interval `(0.5 max(`\eb`,` \es`), 5 max(`\eb`,` \es`))`.
#' The \eqn{\mu}:s are then sorted so the excess trading increases in the
#' information layers, subject to the condition that the ratio of two
#' consecutive \eqn{\mu}'s should be at least \code{1.25}.
#' \item \eb: an integer drawn uniformly from the interval \code{(100, 10000)}
#' with step \code{50}.
#' \item \es: an integer uniformly drawn from (`(3/4)`\eb, `(5/4)`\eb) with step
#' \code{50}.
#' }
#' Based on the simulation parameters `parameters`, daily buys and sells are
#' generated by the assumption that buys and sells
#' follow Poisson distributions with mean parameters (\eb, \es) on days with no
#' information; with mean parameters
#' (\eb + \ifelse{html}{\eqn{\mu}\subit{j}}{ \eqn{\mu_j}{}}, \es) on days
#' with good information of layer \eqn{j} and
#' (\eb, \es + \ifelse{html}{\eqn{\mu}\subit{j}}{\eqn{\mu_j}{}}) on days
#' with bad information of layer \eqn{j}.
#'
#' __Considerations for the ranges of simulation parameters:__ While
#' `generatedata_mpin()` function enables the user to simulate data series
#' with any set of theoretical parameters,
#' we strongly recommend the use of parameter sets satisfying below conditions
#' which are in line with the nature of empirical data and the theoretical
#' models used within this package.
#' When parameter values are not assigned by the user, the function, by default,
#' simulates data series that are in line with these criteria.
#' \itemize{
#' \item _Consideration 1_: any \eqn{\mu}'s value separable from \eb and \es
#' values, as well as other \eqn{\mu} values. Otherwise, the `PIN` and `MPIN`
#' estimation would not yield expected results.\cr
#'  \[x\] Sharp example.1: \eb\eqn{ = 1000}; \eqn{\mu = 1}. In this case, no
#' information layer can be captured in a healthy way by the use of the models
#' which relies on Poisson distributions.\cr
#' \[x\] Sharp example.2: \es\eqn{ = 1000},
#' \ifelse{html}{\eqn{\mu}\subit{1}\eqn{ = 1000}}{\eqn{\mu_1 = 1000}{}},
#' and \ifelse{html}{\eqn{\mu}\subit{2}\eqn{ = 1001}}{\eqn{\mu_2 = 1001}{}}.
#' Similarly, no distinction can be
#' made on the two simulated layers of informed trading. In real life, this
#' entails that there is only one type of information which would also be the
#' estimate of the `MPIN` model. However, in the simulated data properties,
#' there would be 2 layers which will lead the user to make a wrong
#' evaluation of model performance.
#'
#' \item _Consideration 2_: \eb and \es being relatively close to each other.
#' When they are far from each other, that would indicate that there is
#' substantial asymmetry between buyer and seller initiated trades, being a
#' strong signal for informed trading.
#' There is no theoretical evidence to indicate that the uninformed trading in
#' buy and sell sides deviate much from each other in real life.
#' Besides, numerous papers that work with `PIN` model provide close to
#' each other uninformed intensities.
#' when no parameter values are assigned by the user, the function generates
#' data with the condition of sell side uninformed trading to be in the range of
#' `(4/5):=80%` and `(6/5):=120%` of buy side uninformed rate.\cr
#' \[x\] Sharp example.3: \eb\eqn{ = 1000}, \es\eqn{ = 10000}. In this
#' case, the `PIN` and `MPIN` models would tend to consider some of the trading
#' in sell side to be informed (which should be the actual case).
#' Again, the estimation results would deviate much from the simulation
#' parameters being a good news by itself but a misleading factor in model
#' evaluation.
#' See for example \insertCite{ChengLai2021;textual}{PINstimation} as a
#' misinterpretation of comparative performances. The paper's findings highly
#' rely on the simulations with extremely different \eb and \es values
#' (813-8124 pair and 8126-812).
#'}
#'
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # ------------------------------------------------------------------------ #
#' # There are different scenarios of using the function generatedata_mpin()  #
#' # ------------------------------------------------------------------------ #
#'
#' # With no arguments, the function generates one dataset object spanning
#' # 60 days, containing a number of information layers uniformly selected
#' # from `{1, 2, 3, 4, 5}`, and where the parameters are chosen as
#' # described in the details.
#'
#' sdata <- generatedata_mpin()
#'
#' # The number of layers can be deduced from the simulation parameters, if
#' # fed directly to the function generatedata_mpin() through the argument
#' # 'parameters'. In this case, the output is a dataset object with one
#' # information layer.
#'
#' givenpoint <- c(0.4, 0.1, 800, 300, 200)
#' sdata <- generatedata_mpin(parameters = givenpoint)
#'
#' # The number of layers can alternatively be set directly through the
#' # argument 'layers'.
#'
#' sdata <- generatedata_mpin(layers = 2)
#'
#' # The simulation parameters can be randomly drawn from their corresponding
#' # ranges fed through the argument 'ranges'.
#'
#' sdata <- generatedata_mpin(ranges = list(alpha = c(0.1, 0.7),
#'                                         delta = c(0.2, 0.7),
#'                                         mu = c(3000, 5000)))
#'
#' # The value of a given simulation parameter can be set to a specific value by
#' # setting the range of the desired parameter takes a unique value, instead of
#' # a pair of values.
#'
#' sdata <- generatedata_mpin(ranges = list(alpha = 0.4, delta = c(0.2, 0.7),
#'                                         eps.b = c(100, 7000),
#'                                         mu = c(8000, 12000)))
#'
#' # If both arguments 'parameters', and 'layers' are simultaneously provided,
#' # and the number of layers detected from the length of the argument
#' # 'parameters' is different from the argument 'layers', the former is used
#' # and a warning is displayed.
#'
#' sim.params <- c(0.4, 0.2, 0.9, 0.1, 400, 700, 300, 200)
#' sdata <- generatedata_mpin(days = 120, layers = 3, parameters = sim.params)
#'
#' # Display the details of the generated data
#'
#' show(sdata)
#' \donttest{
#' # ------------------------------------------------------------------------ #
#' # Use generatedata_mpin() to compare the accuracy of estimation methods    #
#' # ------------------------------------------------------------------------ #
#'
#' # The example below illustrates the use of the function 'generatedata_mpin()'
#' # to compare the accuracy of the functions 'mpin_ml()', and 'mpin_ecm()'.
#'
#' # The example will depend on three variables:
#' # n: the number of datasets used
#' # l: the number of layers in each simulated datasets
#' # xc : the number of extra clusters used in initials_mpin
#'
#' # For consideration of speed, we will set n = 2, l = 2, and xc = 2
#' # These numbers can change to fit the user's preferences
#' n <- l <- xc <- 2
#'
#' # We start by generating n datasets simulated according to the
#' # assumptions of the MPIN model.
#'
#' dataseries <- generatedata_mpin(series = n, layers = l, verbose = FALSE)
#'
#' # Store the estimates in two different lists: 'mllist', and 'ecmlist'
#'
#' mllist <- lapply(dataseries@datasets, function(x)
#'   mpin_ml(x@data, xtraclusters = xc, layers = l, verbose = FALSE))
#'
#' ecmlist <- lapply(dataseries@datasets, function(x)
#'   mpin_ecm(x@data, xtraclusters = xc, layers = l, verbose = FALSE))
#'
#' # For each estimate, we calculate the absolute difference between the
#' # estimated mpin, and empirical mpin computed using dataset parameters.
#' # The absolute differences are stored in 'mldmpin' ('ecmdpin') for the
#' # ML (ECM) method,
#'
#' mldpin <- sapply(1:n,
#'  function(x) abs(mllist[[x]]@mpin - dataseries@datasets[[x]]@emp.pin))
#'
#' ecmdpin <- sapply(1:n,
#'  function(x) abs(ecmlist[[x]]@mpin - dataseries@datasets[[x]]@emp.pin))
#'
#' # Similarly, we obtain vectors of running times for both estimation methods.
#' # They are stored in 'mltime' ('ecmtime') for the ML (ECM) method.
#'
#' mltime <- sapply(mllist, function(x) x@runningtime)
#' ecmtime <- sapply(ecmlist, function(x) x@runningtime)
#'
#' # Finally, we calculate the average absolute deviation from empirical PIN
#' # as well as the average running time for both methods. This allows us to
#' # compare them in terms of accuracy, and speed.
#'
#' accuracy <- c(mean(mldpin), mean(ecmdpin))
#' timing <- c(mean(mltime), mean(ecmtime))
#' comparison <- as.data.frame(rbind(accuracy, timing))
#' colnames(comparison) <- c("ML", "ECM")
#' rownames(comparison) <- c("Accuracy", "Timing")
#'
#' show(round(comparison, 6))
#' }
#' @export
generatedata_mpin <- function(
  series = 1, days = 60, layers = NULL, parameters = NULL,
  ranges = list(), ..., verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  allvars <- allvars[-6]
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$mpindata()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(series, days, layers, parameters, ranges, 0, verbose)
  names(largs) <- names(formals())
  largs[["..."]] <- NULL
  largs$fn <- "mpindata"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpindata()$fn)

  rst <- .xcheck$ranges(ranges, adj = FALSE)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpindata()$fn)
  ranges <- rst$ranges

  # Recover the arguments from the dot-dot-dot argument
  # -------------------------------------------------------------------------
  controls <- list()
  vargs <- list(...)
  unknown <- setdiff(names(vargs), c(
    "confidence", "maxlayers", "eps_ratio", "mu_ratio"))
  ux$stopnow(length(unknown) > 0, s = uierrors$mpindata()$fn,
             m = uierrors$arguments()$unknown(u = unknown))

  if (length(vargs) > 0) {
    if ("confidence" %in% names(vargs))
      controls$confidence <- confidence <- vargs$confidence
    if ("maxlayers" %in% names(vargs))
      controls$maxlayers <- maxlayers <- vargs$maxlayers
    if ("eps_ratio" %in% names(vargs))
      controls$eps_ratio <- eps_ratio <- vargs$eps_ratio
    if ("mu_ratio" %in% names(vargs))
      controls$mu_ratio <- mu_ratio <- vargs$mu_ratio
  }

  rst <- .xcheck$controls(controls, vargs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpindata()$fn)
  controls <- rst$controls

  if (series == 1) {

    sdata <- .generate_mpin_dataset(days, layers, parameters, ranges, controls)

    ux$stopnow(!is(sdata, "dataset"), m = sdata,
            s = uierrors$mpindata()$fn)

    return(sdata)

  }

  # The argument series is higher than 1 then we create an object dataseries()
  simulation <- new("data.series", series = series,
                    days = days, layers = ifelse(is.null(layers), 0, layers)
  )

  time_on <- Sys.time()
  datalist <- list()
  slayers <- c()
  comments <- c()

  # Display a generation message to users
  # --------------------------------------------------------------------------
  simulation_ms <- uix$mpindata(ndata = series)
  ux$show(verbose, m = simulation_ms$start)
  ux$show(verbose, m = simulation_ms$nsimulation)

  if (verbose) pb <- ux$progressbar(minvalue = 0, maxvalue = series)


  for (k in 1:series) {

    newdata <- .generate_mpin_dataset(days, layers, parameters, ranges,
                                      control = controls)

    ux$stopnow(!is(newdata, "dataset"), m = newdata,
            s = uierrors$mpindata()$fn)

    # Collect the warnings, the number of layers, before adding
    # the new simulated dataset to the datalist
    if (length(newdata@warnings) > 0)
      comments <- c(comments, newdata@warnings[[1]])
    slayers <- c(slayers, newdata@layers)
    datalist <- c(datalist, newdata)

    if (verbose) setTxtProgressBar(pb, k)

  }

  ux$show(verbose, m = simulation_ms$complete)

  time_off <- Sys.time()

  if (length(comments) > 0) comments <- unique(comments) else comments <- 0
  simulation@runningtime <- ux$timediff(time_on, time_off)
  simulation@datasets <- datalist
  simulation@layers <- slayers
  simulation@warnings <- comments

  return(simulation)
}





#' @title Simulation of AdjPIN model data.
#'
#' @description Generates a `dataset` object or a `data.series` object (a list
#' of `dataset` objects) storing simulation parameters as well as aggregate
#' daily buys and sells simulated following the assumption of the `AdjPIN` model
#' of \insertCite{Duarte09;textual}{PINstimation}.
#'
#' @usage
#' generatedata_adjpin(series=1, days = 60, parameters = NULL, ranges = list(),
#' restricted = list(), verbose = TRUE)
#'
#' @param series The number of datasets to generate.
#' @param days The number of trading days, for which aggregated
#' buys and sells are generated. The default value is \code{60}.
#' @param parameters A vector of model parameters of size `10` and it has
#' the following form \{\eqn{\alpha}, \eqn{\delta}, \eqn{\theta}, \eqn{\theta'},
#' \eb, \es, \mub, \mus, \Db, \Ds\}.
#'
#' @param restricted A binary list that allows estimating restricted
#' AdjPIN models by specifying which model parameters are assumed to be equal.
#' It contains one or multiple of the following four elements
#' `{theta, mu, eps, d}`. For instance, If `theta` is set to `TRUE`,
#' then the probability of liquidity shock in no-information days, and in
#' information days is assumed to be the same (\thetaB`=`\thetaS). If any of
#' the remaining rate elements `{mu, eps, d}` is set to `TRUE`,
#' (say  `mu=TRUE`), then the rate is assumed to be the same on the buy side,
#' and on the sell side (\mub`=`\mus). If more than one element is set to
#' `TRUE`, then the restrictions are combined. For instance, if the argument
#' `restricted` is set to `list(theta=TRUE, eps=TRUE, d=TRUE)`, then the
#' restricted AdjPIN model is estimated, where \thetaB`=`\thetaS, \eb`=`\es,
#' and \Db`=`\Ds. If the value of the argument `restricted` is the empty list
#' (`list()`), then all parameters of the model are assumed to be independent,
#' and the unrestricted model is estimated. The default value is the empty
#' list `list()`.
#'
#' @param ranges A list of ranges for the different simulation
#' parameters having named elements `alpha` \eqn{(\alpha)},
#' `delta` \eqn{(\delta)}, `theta` \eqn{(\theta)}, `thetap` \eqn{(\theta')},
#' `eps.b` (\eb), `eps.s` (\es), `mu.b` (\mub), `mu.s` (\mus), `d.b` (\Db),
#' `d.s` (\Ds).
#' The value of each element is a vector of two numbers: the first one is the
#' minimal value `min_v` and the second one is the maximal value `max_v`.
#' If the element corresponding to a given parameter is missing, the default
#' range for that parameter is used, otherwise, the simulation parameters are
#' uniformly drawn from the interval (`min_v`, `max_v`). The default value
#' is `list()`.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the progress of the data generation is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @return Returns an object of class \code{dataset} if `series=1`, and an
#' object of class `data.series` if `series>1`.
#'
#' @details
#' If the argument `parameters` is missing, then the parameters are
#' generated using the ranges specified in the argument `ranges`.
#' If the argument `ranges` is set to `list()`, default ranges are used. Using
#' the default ranges, the simulation parameters are obtained using the
#' following procedure:
#' \itemize{
#' \item \eqn{\alpha}, \eqn{\delta}: \code{(alpha, delta)} uniformly
#' distributed on \code{(0, 1)}.
#' \item \eqn{\theta}, \eqn{\theta'}: \code{(theta,thetap)} uniformly
#' distributed on \code{(0, 1)}.
#' \item \eb: \code{(eps.b)} an integer uniformly drawn from the interval
#' \code{(100, 10000)} with step \code{50}.
#' \item \es: \code{(eps.s)} an integer uniformly drawn from (`(4/5)`\eb,
#' `(6/5)`\eb) with step \code{50}.
#' \item \Db: \code{(d.b)} an integer uniformly drawn from (`(1/2)`\eb,
#' `2`\eb).
#' \item \Ds: \code{(d.s)} an integer uniformly drawn from (`(4/5)`\Db,
#' `(6/5)`\Db).
#' \item \mub: \code{(mu.b)} uniformly distributed on the interval
#' `((1/2) max`(\eb, \es)`, 5 max`(\eb, \es)`)`.
#' \item \mus: \code{(mu.s)} uniformly distributed on the interval
#' (`(4/5)`\mub, `(6/5)`\mub)..
#' }
#'
#' Based on the simulation parameters `parameters`, daily buys and sells are
#' generated by the assumption that buys and sells follow Poisson
#' distributions with mean parameters:
#' \itemize{
#' \item (\eb, \es) in a day with no information and no liquidity shock;
#' \item (\eb+\Db, \es+\Ds) in a day with no information and with liquidity
#' shock;
#' \item (\eb+\mub, \es) in a day with good information and no liquidity
#' shock;
#' \item (\eb+\mub+\Db, \es+\Ds) in a day with good information and
#' liquidity shock;
#' \item (\eb, \es+\mus) in a day with bad information and no liquidity
#' shock;
#' \item (\eb+\Ds, \es+\mus+\Ds) in a day with bad information and
#' liquidity shock;
#' }
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # ------------------------------------------------------------------------ #
#' # Generate data following the AdjPIN model using generatedata_adjpin()     #
#' # ------------------------------------------------------------------------ #
#'
#' # With no arguments, the function generates one dataset object spanning
#' # 60 days, and where the parameters are chosen as described in the section
#' # 'Details'.
#'
#' sdata <- generatedata_adjpin()
#'
#' # Alternatively, simulation parameters can be provided. Recall the order of
#' # parameters (alpha, delta, theta, theta', eps.b, eps.s, mub, mus, db, ds).
#'
#' givenpoint <- c(0.4, 0.1, 0.5, 0.6, 800, 1000, 2300, 4000, 500, 500)
#' sdata <- generatedata_adjpin(parameters = givenpoint)
#'
#' # Data can be generated following restricted AdjPIN models, for example, with
#' # restrictions 'eps.b = eps.s', and 'mu.b = mu.s'.
#'
#' sdata <- generatedata_adjpin(restricted = list(eps = TRUE, mu = TRUE))
#'
#' # Data can be generated using provided ranges of simulation parameters as fed
#' # to the function using the argument 'ranges', where thetap corresponds to
#' # theta'.
#'
#' sdata <- generatedata_adjpin(ranges = list(
#'   alpha = c(0.1, 0.15), delta = c(0.2, 0.2),
#'   theta = c(0.2, 0.6), thetap = c(0.2, 0.4)
#' ))
#'
#' # The value of a given simulation parameter can be set to a specific value by
#' # setting the range of the desired parameter takes a unique value, instead of
#' # a pair of values.
#'
#' sdata <- generatedata_adjpin(ranges = list(
#'   alpha = 0.4, delta = c(0.2, 0.7),
#'   eps.b = c(100, 7000), mu.b = 8000
#' ))
#'
#' # Display the details of the generated simulation data
#'
#' show(sdata)
#'
#' # ------------------------------------------------------------------------ #
#' # Use generatedata_adjpin() to check the accuracy of adjpin()              #
#' # ------------------------------------------------------------------------ #
#'
#' model <- adjpin(sdata@data, verbose = FALSE)
#'
#' summary <- cbind(
#'   c(sdata@emp.pin['adjpin'], model@adjpin, abs(model@adjpin -
#'   sdata@emp.pin['adjpin'])),
#'   c(sdata@emp.pin['psos'], model@psos, abs(model@psos -
#'   sdata@emp.pin['psos']))
#' )
#' colnames(summary) <- c('adjpin', 'psos')
#' rownames(summary) <- c('Data', 'Model', 'Difference')
#'
#' show(knitr::kable(summary, 'simple'))
#'
#' @export
generatedata_adjpin <- function(
  series = 1, days = 60, parameters = NULL, ranges = list(),
  restricted = list(), verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$adjpindata()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(series, days, parameters, ranges, restricted, verbose)
  names(largs) <- names(formals())
  largs$fn <- "adjpindata"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$adjpindata()$fn)

  restricted <- .xadjpin$allrestrictions(restricted)

  rst <- .xcheck$ranges(ranges, adj = TRUE)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpindata()$fn)
  ranges <- rst$ranges

  # If series == 1 return a 'dataset' object
  # --------------------------------------------------------------------------
  if (series == 1) {
    sdata <- .generate_adjpin_dataset(days, parameters, restricted, ranges)

    ux$stopnow(!is(sdata, "dataset"), m = sdata,
            s = uierrors$mpindata()$fn)

    return(sdata)

  }

  # If series >1  return a 'data.series' object
  # --------------------------------------------------------------------------
  simulation <- new("data.series", series = series, model = "adjPIN",
                    days = days, layers = 1)
  time_on <- Sys.time()
  dataseries <- list()



  # Display a generation message to users
  # --------------------------------------------------------------------------
  simulation_ms <- uix$adjpindata(ndata = series)
  ux$show(verbose, m = simulation_ms$start)
  ux$show(verbose, m = simulation_ms$nsimulation)

  if (verbose) pb <- ux$progressbar(minvalue = 0, maxvalue = series)

  for (k in 1:series) {
    sim_data <- .generate_adjpin_dataset(days, parameters, restricted, ranges)

    ux$stopnow(!is(sim_data, "dataset"), m = sdata,
            s = uierrors$mpindata()$fn)

    dataseries <- c(dataseries, sim_data)

    if (verbose) setTxtProgressBar(pb, k)

  }
  ux$show(verbose, m = simulation_ms$complete)

  time_off <- Sys.time()
  simulation@runningtime <- ux$timediff(time_on, time_off)
  simulation@datasets <- dataseries
  simulation@layers <- 1
  simulation@restrictions <- restricted
  return(simulation)
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


# ---------------------------------------------------------------------------- #
# Main function to generate data for the MPIN model                            #
# ---------------------------------------------------------------------------- #

.generate_mpin_dataset <- function(days = 60, layers = NULL, parameters = NULL,
                          ranges = list(), control = list()) {
  # generate data following the MPIN model of Ersan(2016)
  #
  # Args:
  #   days    : number of trading days in the generated dataset
  #   layers  : number of information layers in the generated dataset
  #   parameters  : a set of values for the simulation parameters
  #   ranges  : a list of ranges for the different simulation parameters
  #   control : a list of variables controlling the relation between the
  #             different simulation parameters
  #
  # Returns:
  #   An object of type 'dataset'

  # initialize the local variables
  # --------------------------------------------------------------------------
  a <- d <- mu <- eb <- es <-  mu_range <-  NULL
  maxlayers <- eratio <-  time_on <- time_off <-  NULL
  kmin_eb <- 100
  kmax_eb <- 10000

  # Managing conflicts between different rules
  # --------------------------------------------------------------------------
  conflicts <- list(ids = NULL, msgs = NULL)

  # (A) Check the argument 'ranges'
  # --------------------------------------------------------------------------
  rng <- ranges
  mu_range <- rng$mu

  # (b) Check the argument 'control'
  # --------------------------------------------------------------------------
  ctrl <- control

  # If valid, create individual controls
  # ------------------------------------
  eratio <- ctrl$eps_ratio
  maxlayers <- ctrl$maxlayers

  if (!is.null(parameters)) {

    # If valid, create individual variables
    # -------------------------------------
    xlayers <- (length(unlist(parameters)) - 2) / 3
    ux$show(c= (!is.null(layers) && (xlayers != layers)),
            m = uierrors$layers(code = 1),
            warning = TRUE
            )
    layers <- xlayers

    vars <- .xmpin$varnames()
    values <- unname(
      split(parameters, rep(1:5, c(layers, layers, layers, 1, 1))))
    for (i in seq_len(length(values))) assign(vars[i], unlist(values[[i]]))

  } else {

    # (D) find the number of layers
    # ------------------------------------------------------------------------
    # 4 types of layers are relevant:
    # layers    : provided by the user
    # xlayers   : derived from parameters
    # maxlayers : provided by user or default value is 5
    # alayers   : derived from the minimum alpha value min_alpha
    # where alayers = 1/minalpha when minalpha != 0
    # ------------------------------------------------------------------------
    minalpha <- rng$alpha[1]
    xlayers <-  length(a)

    layers <- .xcheck$layers(minalpha, xlayers, layers, maxlayers, days)

    if (!is.numeric(layers)) return(layers)

    # (E.1) sample values for 'alpha' and 'delta'
    # ------------------------------------------------------------------------

    a <- .xgenerate$alpha(rng$alpha, layers, days)

    d <- .xgenerate$number(rng$delta, size = layers)

    # (E.2) sample values for 'eb' and 'es'
    # ------------------------------------------------------------------------
    dfrngeb <- all(rng$eps.b == c(kmin_eb, kmax_eb))

    get_eps <- .xgenerate$eps(conflicts, rng, dfrngeb, eratio)
    if (!is.list(get_eps)) return(get_eps)

    conflicts <- get_eps$conflicts
    eb <- get_eps$eb
    es <- get_eps$es

    # (E.3) sample values for 'mu'
    # ------------------------------------------------------------------------

    get_mu <- .xgenerate$mu(conflicts, mu_range, eb, es, layers, ctrl)

    conflicts <- get_mu$conflicts
    mu <- get_mu$mu

  }

  time_on <- Sys.time()

  # (F) Parameters obtained - Start Simulation
  # ------------------------------------------------------------------------
  prob <- c(1 - sum(a), a)

  lyr <- .xgenerate$layers(layers, days, prob)

  trades <- data.frame(lyr = lyr)
  badinfo <- rep(0, days)

  # Apply for each value of lyr different from zero, the function
  # set_bad_days, which samples the value 0 (good day) with probability
  # 1 - d[layer], and 1 (bad day) with probability d[layer]

  set_bad_days <- function(layer) {
    if (layer == 0) return(0)
    return(sample(c(0, 1), 1, prob = c(1 - d[layer], d[layer])))
  }


  trades$badinfo <- unlist(lapply(trades$lyr, set_bad_days))
  fullbox <- data.frame(lyr = 1:layers, badinfo = 0)

  info_days <- aggregate(badinfo~lyr, trades, length, drop = FALSE)
  info_days <- info_days[info_days$lyr != 0, ]$badinfo
  info_days[is.na(info_days)] <- 0

  badinfo_days <- fullbox$badinfo

  if (sum(trades$badinfo) > 0) {

    badinfo_days <- aggregate(badinfo~lyr,
                              trades[trades$badinfo == 1, ],
                              length, drop = FALSE)
    badinfo_days <- merge(badinfo_days, fullbox,
                          by = c("lyr"), all = TRUE)[, 2]
    badinfo_days[is.na(badinfo_days)] <- 0

  }

  valid_empiricals <- FALSE

  while (!valid_empiricals) {

    # (G) Different layers are identified - simulate rates
    # ------------------------------------------------------------------------
    # Obtain empirical parameters

    empiricals <- list()
    emp_a <- info_days / days
    emp_d <- badinfo_days / info_days
    mu <- c(0, mu)

    trades$b <- with(trades, rpois(days, lambda = eb +
                                           mu[lyr + 1] * (1 - badinfo)))
    trades$s <- with(trades, rpois(days, lambda = es +
                                           mu[lyr + 1] * badinfo))
    mu <- tail(mu, -1)
    emp_eb <- mean(
      trades$b[which(trades$lyr == 0 | trades$badinfo == 1)])
    emp_es <- mean(
      trades$s[which(trades$lyr == 0 | trades$badinfo == 0)])

    # Find excess Buys and excess Sells
    trades$exb <- trades$b - emp_eb
    trades$exs <- trades$s - emp_es

    exbuys <- trades[trades$lyr != 0 & trades$badinfo == 0, c("lyr", "exb")]
    exsells <- trades[trades$lyr != 0 & trades$badinfo == 1, c("lyr", "exs")]
    colnames(exbuys) <- colnames(exsells) <- c("lyr", "excess")
    extrades <- rbind(exbuys, exsells)
    rm(exbuys, exsells)

    # Compute the empirical estimates of the different parameters
    emp_mu <- aggregate(excess~lyr, extrades, mean)$excess
    emp_mpin <- sum(emp_a * emp_mu) / (sum(emp_a * emp_mu) + emp_eb + emp_es)

    # Make sure that all empirical trading rates are positive
    if (all(c(emp_mu, emp_eb, emp_es) > 0)) valid_empiricals <- TRUE

  }


  # (H) Collect the parameters and return the simulation object
  # ------------------------------------------------------------------------
  theoreticals <- list(a, d, mu, eb, es)
  empiricals <- list(emp_a, emp_d, emp_mu, emp_eb, emp_es)

  names(theoreticals) <- names(empiricals) <- .xmpin$varnames(2)

  aggregates <- c(list(sum(emp_a)), list(sum(emp_a * emp_d) / sum(emp_a)),
                  list(sum(emp_a * emp_mu) / sum(emp_a)),
                  list(emp_eb), list(emp_es))
  names(aggregates) <- .xmpin$varnames(2)

  data <- trades[, c("b", "s")]


  liklihood <- -factorizations$mpin(data)(unlist(empiricals))
  time_off <- Sys.time()

  simulation <- new("dataset",
                    days = days,
                    layers = layers,
                    theoreticals = theoreticals,
                    empiricals = empiricals,
                    emp.pin = setNames(c(emp_mpin), c("MPIN")),
                    data = data, aggregates = aggregates,
                    likelihood = liklihood, warnings = conflicts,
                    runningtime = ux$timediff(time_on, time_off))

  return(simulation)
}

# ---------------------------------------------------------------------------- #
# Main function to generate data for the AdjPIN model                          #
# ---------------------------------------------------------------------------- #

.generate_adjpin_dataset <- function(days = 60, parameters = NULL,
                                     restricted = list(), ranges = list()) {
  # Generates a `dataset` object storing aggregate daily buys and sells
  # and  simulation parameters for the AdjPIN model. It feeds the function
  # generatedata_adjpin with 'dataset' objects.
  #
  # Args:
  #   days : number of trading days for which aggregated buys and
  #          sells are simulated. Default value is 60.
  #   parameters: a vector of model parameters of size `10` and it has
  #           the following form (alpha, delta, theta, thetap, eps.b,
  #           eps.s, mu.b, mu.s, d.b, d.s)
  #   restricted  : a list specifying whether the model parameters are equal.
  #           It can take the value `NULL`, or contain one or more keys from
  #           the following keys (theta, mu, eps, d)
  #   ranges : a list of ranges for the different simulation parameters
  #            having named elements (alpha, delta, theta, thetap, eps.b,
  #           eps.s, mu.b, mu.s, d.b, d.s) of the form (minv, maxv) or
  #           a unique value.
  # Returns:
  #   an object of class dataset.

  rstc <- restricted <- .xadjpin$allrestrictions(restricted)


  # Initialize local variables
  # --------------------------------------------------------------------------
  ds <- NULL

  # Get ranges for the parameters
  # --------------------------------------------------------------------------
  rng <- ranges

  # --------------------------------------------------------------------------
  some_na_results <- TRUE

  while (some_na_results) {

    # ------------------------------------------------------------------------
    # | (1) |  Generate theoretical parameters, if not provided
    # ------------------------------------------------------------------------
    if (!is.null(parameters)) {

      ux$stopnow(c = (length(parameters) != 10),
             m = uierrors$adjpindata()$wrongdim,
             s = uierrors$adjpindata()$fn)

      vars <- .xadjpin$vars(all = TRUE)
      values <- unname(split(parameters, 1:10))

      for (i in seq_len(length(values)))
        assign(vars[i], unname(unlist(values[[i]])))


    } else {

      # Collect ranges in variables
      # Generate probabilities alpha, delta, theta and theta'
      # ----------------------------------------------------------------------
      a <- .xgenerate$number(rng$alpha)
      d <- .xgenerate$number(rng$delta)
      t <- .xgenerate$number(rng$theta)
      tp <- (rstc$theta) * t  + (!rstc$theta) * .xgenerate$number(rng$thetap)

      # Generate rates eb, es , db, and ds
      # ----------------------------------------------------------------------
      # eb in (100,10000), es in  by default
      # The default range for es is (4/5 eb, 6/5 eb)
      eb <- .xgenerate$number(rng$eps.b, int = TRUE)
      es <- .xgenerate$number(rng$eps.s, dfrange = c(0.8 * eb, 1.2 * eb),
                              int = TRUE)

      # find bounds on rates allows to find the minimum value of db/ds, and
      # mub/mus that makes the cluster trade rates eb + db, and eb + mub
      # (es + ds and es + mus) sufficiently different
      rbounds <- .xfind$bounds(eb, es)
      rmin <- rbounds[1]
      rmax <- rbounds[2]

      db <- .xgenerate$number(rng$d.b, dfrange = c(rmin, rmax),
                              int = TRUE)
      ds <- .xgenerate$number(rng$d.s, dfrange = c(0.8 * db, 1.2 * db),
                              int = TRUE)

      # Generate rates mub and mus
      # ----------------------------------------------------------------------
      # mub in (0.5,5)*max(eb,es), mus in (0.5,5)*max(eb,es) by default
      # We know from above that mu shall be at least rmin
      # We will update these minimum using the 0.5 * max(eb, es).
      # We preserve the maximum 5 * max(eb, es)

      minmub <- max(rmin, round(0.5 * max(eb, es)))
      maxmub <- max(3 * rmin, 5 * max(eb, es))

      # default range for mus (dfrng) is  (4/5 mub, 6/5 mub)
      mub <- .xgenerate$number(rng$mu.b, dfrange = c(minmub, maxmub),
                               int = TRUE)
      mus <- .xgenerate$number(rng$mu.s, dfrange =  c(0.8, 1.2) * mub,
                               int = TRUE)

    }

    # Adjust for equality conditions (the vector restricted)
    # ----------------------------------------------------------------------
    es <- (rstc$eps) * eb + (!rstc$eps) * es
    mus <- (rstc$mu) * mub + (!rstc$mu) * mus
    ds <- (rstc$d) * db + (!rstc$d) * ds

    # Store the theoretical parameters in the list 'theoreticals'
    # ----------------------------------------------------------------------
    theoreticals <- list(a, d, t, tp, eb, es, mub, mus, db, ds)
    names(theoreticals) <- c(.xadjpin$varnames())

    # ------------------------------------------------------------------------
    # | (2) |  Generate data using theoretical parameters
    # ------------------------------------------------------------------------

    time_on <- Sys.time()
    prob <- c((1 - a) * (1 - t), (1 - a) * t, a * (1 - d) * (1 - tp),
              a * (1 -  d) * tp, a * d * (1 - tp), a * d * tp)

    # This is the theoretical distribution of AdjPIN model
    # ------------------------------------------------------------------------
    #
    #----------------------------------------
    #       |[buys]         |[sells]        |
    #----------------------------------------
    # C1    |[eb]           |[es]           |
    # C2    |[eb+db]        |[es+ds]        |
    # C3    |[eb+mub]       |[es]           |
    # C4    |[eb+mub+db]    |[es+ds]        |
    # C5    |[eb]           |[es+mus]       |
    # C6    |[eb+db]        |[es+mus+ds]    |
    #----------------------------------------

    clusters <- sample(c("C1", "C2", "C3", "C4", "C5", "C6"),
                       size = days, replace = TRUE, prob = prob)

    # The function .xfind$clusters makes sure that the distribution of
    # clusters allows for the calculation of all empirical parameters
    welldistributed <- .xfind$clusters(days, prob, parameters, ranges, clusters)

    trades <- welldistributed$days
    distrib <- welldistributed$distrib

    # ------------------------------------------------------------------------
    # | (3) |  Compute the empirical parameters from the generated data
    # ------------------------------------------------------------------------


    # (3.1) Estimate probabilities: alpha, delta, theta and theta'
    # ------------------------------------------------------------------------
    empiricals <- list()
    empiricals$a <- sum(distrib[3:6]) / (sum(distrib))
    empiricals$d <- sum(distrib[5:6]) / sum(distrib[3:6])
    empiricals$t <- sum(distrib[2]) / sum(distrib[1:2])
    empiricals$tp <- sum(distrib[c(4, 6)]) / sum(distrib[3:6])

    # (3.2) Estimate the empirical trading rates
    # ------------------------------------------------------------------------
    trades$badinfo <- as.numeric((trades$clusters %in% c("C5", "C6")))
    trades$lqshock <- as.numeric((trades$clusters %in% c("C2", "C4", "C6")))
    trades$info <- as.numeric(!(trades$clusters %in% c("C1", "C2")))

    # Any empirical probability can take zero values only when the corresponding
    # theoretical value is also zero. Otherwise, it is declared invalid, and the
    # simulation is done again.

    valid_empiricals <- FALSE

    while (!valid_empiricals) {


      trades$b <- suppressWarnings(with(trades, rpois(
        days, lambda = eb + mub * info * (1 - badinfo) +  db * lqshock)))

      trades$s <- suppressWarnings(with(trades, rpois(
        days, lambda = es + mus * info * badinfo + ds * lqshock)))

      means <- aggregate(.~clusters, trades, mean)

      meanna <- function(x) return(ifelse(is.null(x), 0, mean(x)))

      `%-b-%` <- function(cluster1, cluster2 = NULL)
        return(.xfind$diff(means, cluster1, cluster2))

      `%-s-%` <- function(cluster1, cluster2 = NULL)
        return(.xfind$diff(means, cluster1, cluster2,
                           buy = FALSE))

      buy <- function(cluster) return(.xfind$diff(means, cluster))
      sell <- function(cluster) return(.xfind$diff(means, cluster, buy = FALSE))


      getempiricals <- function() {

        # This is the theoretical distribution of AdjPIN model
        # ----------------------------------------------------------------------
        #
        #----------------------------------------
        #       |[buys]         |[sells]        |
        #----------------------------------------
        # C1    |[eb]           |[es]           |
        # C2    |[eb+db]        |[es+ds]        |
        # C3    |[eb+mub]       |[es]           |
        # C4    |[eb+mub+db]    |[es+ds]        |
        # C5    |[eb]           |[es+mus]       |
        # C6    |[eb+db]        |[es+mus+ds]    |
        #----------------------------------------

        empiricals$eb <- meanna(c(buy("C1"), buy("C5")))

        empiricals$es <- meanna(c(sell("C1"), sell("C3")))

        # empiricals$mub <- meanna(c(rdiffb("C3", "C1"), rdiffb("C3", "C5"),
        #                     rdiffb("C4", "C2"), rdiffb("C4", "C6")))

        empiricals$mub <- meanna(c("C3" %-b-% "C1", "C3" %-b-% "C5",
                                   "C4" %-b-% "C2", "C4" %-b-% "C6"))

        # empiricals$mus <- meanna(c(rdiffs("C5", "C1"), rdiffs("C5", "C3"),
        #                            rdiffs("C6", "C4"), rdiffs("C6", "C2")))

        empiricals$mus <- meanna(c("C5" %-s-% "C1", "C5" %-s-% "C3",
                                   "C6" %-s-% "C4", "C6" %-s-% "C2"))

        # empiricals$db <- meanna(
        #   c(rdiffb("C2", "C1"), rdiffb("C2", "C5"), rdiffb("C4", "C3"),
        #     rdiffb("C6", "C5"), rdiffb("C6", "C1")))

        empiricals$db <- meanna(
          c("C2" %-b-% "C1",  "C2" %-b-% "C5", "C4" %-b-% "C3",
            "C6" %-b-% "C5", "C6" %-b-% "C1"))

        # empiricals$ds <- meanna(
        #   c(rdiffs("C2", "C1"), rdiffs("C2", "C3"), rdiffs("C6", "C5"),
        #     rdiffs("C4", "C1"), rdiffs("C4", "C3")))

        empiricals$ds <- meanna(
          c("C2" %-s-% "C1", "C2" %-s-% "C3", "C6" %-s-% "C5",
            "C4" %-s-% "C1", "C4" %-s-% "C3"))

        return(empiricals)

      }

      empiricals <- getempiricals()

      if (restricted$d) {
        liq <- trades$clusters %in% c("C2", "C4", "C6")
        addin <- rpois(sum(liq), abs(empiricals$db - empiricals$ds))
        trades[liq, ]$s  <- trades[liq, ]$s +
          sign(empiricals$db - empiricals$ds) * addin
      }

      if (restricted$mu) {
        liq <- trades$clusters %in% c("C5", "C6")
        addin <- rpois(sum(liq), abs(empiricals$mub - empiricals$mus))
        trades[liq, ]$s  <- trades[liq, ]$s +
          sign(empiricals$mub - empiricals$mus) * addin
      }

      if (restricted$eps) {
        addin <- rpois(nrow(trades), abs(empiricals$eb - empiricals$es))
        trades$s  <- trades$s + sign(empiricals$eb - empiricals$es) * addin
      }

      if (restricted$d | restricted$mu | restricted$eps) {

        means <- aggregate(.~clusters, trades, mean)
        empiricals <- getempiricals()

      }

      # ------------------------------------------------------------------------
      # | (4) |  Find the theoretical, and empirical adjpin, and psos
      # ------------------------------------------------------------------------

      # Theoretical values

      theoreticals$adjpin <- (a * ((1 - d) * mub + d * mus)) *
        (1 / ((a * ((1 - d) * mub + d * mus)) +  (db + ds) *
                (a * tp + (1 - a) * t) + eb + es))

      theoreticals$psos <- ((db + ds) * (a * tp + (1 - a) * t)) *
        (1 / ((a * ((1 - d) * mub + d * mus)) +
                (db + ds) * (a * tp + (1 - a) * t) + eb + es))

      # Empirical values

      empiricals$adjpin <- with(
        empiricals, (a * ((1 - d) * mub + d * mus)) *
          (1 / ((a * ((1 - d) * mub + d * mus)) +  (db + ds) *
                  (a * tp + (1 - a) * t) + eb + es)))

      empiricals$psos <- with(
        empiricals, ((db + ds) * (a * tp + (1 - a) * t)) *
          (1 / ((a * ((1 - d) * mub + d * mus)) + (db + ds) *
                  (a * tp + (1 - a) * t) + eb + es)))

      emp_nonzero <- unlist(empiricals)[1:(4 - (restricted$theta))] != 0
      the_nonzero <- unlist(theoreticals)[1:(4 - (restricted$theta))] != 0

      valid_empiricals <- TRUE
      if (any(emp_nonzero != the_nonzero)) valid_empiricals <- FALSE

    }

    if (!anyNA(theoreticals) & !anyNA(empiricals)) some_na_results <- FALSE

  }
  time_off <- Sys.time()
  # Return the S4 object of generated data
  # ------------------------------------------------------------------------
  data <- trades[, c("b", "s")]

  names(empiricals) <- c(.xadjpin$varnames(restricted), "adjpin", "psos")

  adjpin_simulation <- new(
    "dataset", model = "adjPIN", days = days,
    theoreticals = lapply(theoreticals, round, 8),
    empiricals = lapply(empiricals, round, 8),
    likelihood = -factorizations$adjpin(data)(unlist(empiricals[1:10])),
    emp.pin = setNames(c(empiricals$adjpin, empiricals$psos),
                       c("adjpin", "psos")),
    aggregates = lapply(empiricals, round, 8), data = data,
    restrictions = restricted, runningtime = ux$timediff(time_on, time_off))

  return(adjpin_simulation)

}


# ---------------------------------------------------------------------------- #
# Two supporting lists of functions | .xfind() and .xgenerate()                #
# ---------------------------------------------------------------------------- #

.xfind <- list(

  clusters = function(days, prob, parameters, ranges, clusters) {


    #########
    # Alpha #
    #########
    # Check that there is a no-information cluster and an information cluster

    is_noinfo <- (sum(clusters %in% c("C1", "C2")) > 0)
    is_info  <- (sum(clusters %in% c("C3", "C4", "C5", "C6")) > 0)

    tempclusters <- clusters

    # The conditional probability of being in no-information cluster: probn
    if (!is_noinfo) {
      probn <- prob[1:2] / sum(prob[1:2])
      cluster1 <- sample(c("C1", "C2"), size = 1, prob = probn)
      tempclusters <- c(cluster1, tempclusters)
    }

    # The conditional probability of being in information layer: probi
    if (!is_info) {
      probi <- prob[3:6] / sum(prob[3:6])
      cluster2 <- sample(c("C3", "C4", "C5", "C6"), 1, prob = probi)
      tempclusters <- c(cluster2, tempclusters)
    }

    #########
    # emp_eb and emp_es >0
    #########

    # There should be enough data to calculate emp_eb and emp_es

    is_emp_eb <- (sum(tempclusters %in% c("C1", "C5")) > 0)
    is_emp_es  <- (sum(tempclusters %in% c("C1", "C3")) > 0)

    # Get the probabibility corresponding to cluster 1 and 5
    if (!is_emp_eb) {
      prob_eb <- c(prob[1], prob[5]) / sum(c(prob[1], prob[5]))
      tempclusters <- c(sample(c("C1", "C5"), 1, prob = prob_eb),
                        tempclusters)
    }

    # Get the probabibility corresponding to cluster 1 and 3
    if (!is_emp_es) {
      prob_es <- c(prob[1], prob[3]) / sum(c(prob[1], prob[3]))
      tempclusters <- c(sample(c("C1", "C3"), 1, prob = prob_es),
                        tempclusters)
    }


    #########
    # Delta #
    #########
    # If delta is not explicity equal to zero or to one, both good information
    # cluster and bad information cluster should be present.

    delta_nonzero <- ifelse(is.null(parameters), max(ranges$delta) != 0,
                            parameters[2] != 0)

    delta_notone <- ifelse(is.null(parameters), min(ranges$delta) != 1,
                           parameters[2] != 1)

    if (delta_nonzero & delta_notone) {

      badinfocluster <- (sum(tempclusters %in% c("C6", "C5")) > 0)
      goodinfocluster <- (sum(tempclusters %in% c("C4", "C3")) > 0)


      # Get the probabibility corresponding to cluster 5 and 6
      if (!badinfocluster) {
        prob_b <- c(prob[5], prob[6]) / sum(c(prob[5], prob[6]))
        tempclusters <- c(sample(c("C5", "C6"), 1, prob = prob_b),
                          tempclusters)
      }

      # Get the probabibility corresponding to cluster 3 and 4
      if (!goodinfocluster) {
        prob_g <- c(prob[3], prob[4]) / sum(c(prob[3], prob[4]))
        tempclusters <- c(sample(c("C3", "C4"), 1, prob = prob_g),
                          tempclusters)
      }

    }

    ##########
    # Thetas #
    ##########
    # If both thetas are not explicity equal to zero or to one, both shock and
    # no shock clusters should be present.

    t_nonzero <- ifelse(is.null(parameters), max(ranges$theta) > 0,
                        parameters[3] > 0)
    tp_nonzero <- ifelse(is.null(parameters), max(ranges$thetap) > 0,
                         parameters[4] > 0)

    thetas_nonzero <- (t_nonzero | tp_nonzero)

    t_notone <- ifelse(is.null(parameters), min(ranges$theta) < 1,
                       parameters[3] < 1)
    tp_notone <- ifelse(is.null(parameters), min(ranges$thetap) < 1,
                        parameters[4] < 1)

    thetas_notone <- (t_notone | tp_notone)

    if (thetas_nonzero & thetas_notone) {

      if (t_nonzero & t_notone) {
        nshock <- (sum(tempclusters %in% c("C2")) > 0)
        if (!nshock) {
          tempclusters <- c("C2", tempclusters)
        }
      }

      if (tp_nonzero & tp_notone) {
        shock <- (sum(clusters %in% c("C4", "C6")) > 0)
        if (!shock) {
          prob_s <- prob[c(4, 6)] / sum(prob[c(4, 6)])
          tempclusters <- c(sample(c("C4", "C6"), 1,
                                   prob = prob_s), tempclusters)
        }
      }

    }

    trades <- data.frame(days = 1, clusters = tempclusters)
    xtra <- nrow(trades) - days

    remain <- trades[(xtra + 1): length(trades$clusters), ]
    tempgrp <- aggregate(days ~ clusters, remain, length, drop = FALSE)
    tempgrp <- tempgrp[order(tempgrp$days, decreasing = TRUE), ]
    cls <- tempgrp$clusters[tempgrp$days > xtra]
    getcls <- which(remain$clusters %in% cls)
    if (xtra > 0) {
      getcls <- sample(getcls, size = xtra)
      remain <- remain[-c(getcls), ]
      trades <- rbind(trades[1:xtra, ], remain)
    }

    grouped_days <- aggregate(days ~ clusters, trades, length, drop = FALSE)
    alldays <- data.frame(clusters = c("C1", "C2", "C3", "C4", "C5",
                                       "C6"), days = 0)
    grouped_days <- merge(grouped_days, alldays, by = c("clusters"),
                          all.y = TRUE)[, 1:2]
    grouped_days[is.na(grouped_days)] <- 0
    colnames(grouped_days) <- c("clusters", "days")
    distrib <- grouped_days$days / sum(grouped_days$days)

    return(list(days = trades, distrib = distrib))
  },

  bounds = function(eb, es) {

    # Find the value of generated eb, es at probability 95%
    meb <- qpois(0.95, eb)
    mes <- qpois(0.95, es)

    # Now we find the lowest multiple of eb, es that makes sure that the
    # intervals eb, eb + mub, eb + db; and es, es + mus, es + ds are
    # sufficiently separated.
    for (m in 5:20) {
      peb <- (m * eb) / 4
      if (qpois(0.05, peb) >= meb) break
    }

    for (m in 5:20) {
      pes <- (m * es) / 4
      if (qpois(0.05, pes) >= mes) break
    }

    # The minimum of mub, and db should be peb - eb, and pes - es
    # The maximum shall be four times the minimum to preserve the
    # original bounds eb/2 and 2 * eb, in case eb is large
    rmin <- peb - eb
    rmax <- max(4 * eb, 2 * rmin)

    return(c(rmin, rmax))

  },

  diff = function(df, cluster1, cluster2 = NULL, buy = TRUE) {

    row1 <- df[df$clusters == cluster1, c("b", "s")]
    row2 <- df[1, c("b", "s")] * 0
    if (!is.null(cluster2))
      row2 <- df[df$clusters == cluster2, c("b", "s")]

    if (nrow(row1) * nrow(row2) == 0) return(NULL)

    xdiff <- unlist(row1 - row2)
    if (xdiff[2 - buy] < 0) return(NULL)
    return(xdiff[2 - buy])

  }

)


.xgenerate <- list(

  number = function(range, dfrange = NULL, int = FALSE, size = 1) {
    if (!is.null(range)) {
      xsim <- runif(size, min = range[1], max = range[2])
    } else {
      xsim <- runif(size, min = dfrange[1], max = dfrange[2])
    }
    if (int) xsim <- round(xsim)
    return(xsim)
  },

  alpha = function(a_rng, layers, days) {

    # If the range for alpha is a real, just replicate it.
    a <- rep(a_rng[1], layers)

    if (a_rng[2] > a_rng[1]) {

      min_alpha <- rep(a_rng[1], layers)
      remainder <- 1 - sum(min_alpha) - 1 / days
      dist <- runif(layers + 1, min = a_rng[1], max = a_rng[2])
      share <- (dist / sum(dist)) * remainder
      a <- min_alpha + share[1:layers]

    }

    return(a)
  },

  eps = function(conflicts, rng, dfrngeb, eratio) {

    eb_rng <- rng$eps.b
    es_rng <- rng$eps.s

    nlrng <- lapply(rng, function(x) is.null(x))

    eb <- .xgenerate$number(eb_rng, int = TRUE)

    # 5 cases
    mxerzero <- (max(eratio) == 0)
    ersame <- (eratio[1] == eratio[2])

    nlrnges <- nlrng$eps.s

    if (mxerzero & nlrnges) return(uierrors$mpindata()$esrange)

    if (mxerzero & !nlrnges) {
      es <- .xgenerate$number(es_rng, int = TRUE)

      conflicts <- uiconflicts$add(conflicts, conflict = 1,
                                   details = c(eb_rng, es_rng))
    }

    if (!mxerzero & nlrnges)
      es <- .xgenerate$number(c(min(eratio) * eb, max(eratio) * eb), int = TRUE)

    if (!mxerzero & !nlrnges & dfrngeb) {
      es <- .xgenerate$number(es_rng, int = TRUE)
      eb <- .xgenerate$number(c(es / max(eratio), es / min(eratio)), int = TRUE)
    }

    if (!mxerzero & !nlrnges & !dfrngeb) {
      min_es <- min(eb_rng) * eratio[1]
      max_es <- max(eb_rng) * eratio[2]

      lower_bd <- max(min_es, min(es_rng))
      upper_bd <- min(max_es, max(es_rng))

      if (lower_bd > upper_bd)
        return(uierrors$mpindata()$epsimpossible)

      es <- .xgenerate$number(c(lower_bd, upper_bd), int = TRUE)
      eb <- (ersame) * (es / eratio[1]) +
        (!ersame) * .xgenerate$number(eb_rng, int = TRUE)
    }

    return(list(conflicts = conflicts, eb = eb, es = es))

  },

  mu = function(conflicts, mu_range, eb, es, layers, ctrl) {

    conf <- overlap <- mu_ratio <- NULL
    ctrls <- c("eratio", "mu_ratio", "maxlayers", "conf", "overlap")
    for (i in seq_len(length(ctrls))) assign(ctrls[i], unlist(ctrl[[i]]))


    if (is.null(mu_range)) {
      lb <- 0.5 * max(eb, es)
      ub <- 5 * max(eb, es)
    } else {
      lb <- mu_range[1]
      ub <- mu_range[2]
    }

    # (1) Generate using 'mu_ratio'
    # ------------------------------------------------------------------------
    # We want to have mu's sufficiently different from one
    # another. The ratio between two consecutive mu's should be at least
    # equal to 1.25 if the range is sufficiently wide. Otherwise, it should
    # be equal to (1+tau). Let minV, maxV be the bounds of the range of mu.
    # Minimally, We should have maxV = minV*(1+tau)^layers and in this case,
    # we have one possible point satisfying that two consecutive mu's have
    # a ratio of (1+tau). This is why, we replace layers with (layers+1)
    # Now, maxV = minV*(1+tau)^(layers+1)
    # so ratio = 1+tau = (maxV/minV)^(1/(layers+1))

    if (conf == 0) {
      ratio <- min(mu_ratio, round((ub / lb)^ (1 / (layers + 1)), 2))

      # conflict (2): mu_ratio active, conf inactive
      conflicts <- uiconflicts$add(conflicts, 2, details = c(ratio))

      # conflict (4): mu_ratio too large for the mu_range
      if (ratio != mu_ratio & length(mu_range) == 2) {
        details <- c(mu_ratio, mu_range[1], mu_range[2], ratio)
        conflicts <- uiconflicts$add(conflicts, 4, details)
      }

      # Generate the mu's recursively
      mu <- c()
      z <- 0

      for (k in 1:layers) {
        tlb <- max(lb, z * ratio)
        tub <- ub * (ratio^ (k - layers))
        z <- runif(1, min = tlb, max = tub)
        z <- max(z, 1)
        mu <- c(mu, z)
      }
      mu <- round(mu)

    }

    # (2) Generate using 'conf'
    # ------------------------------------------------------------------------
    if (conf != 0) {

      ub <- rep(0, 1 + layers)
      x <- qnorm((conf + 1) / 2)
      ub[1] <- x * sqrt(eb + es)
      lb[1] <- - x * sqrt(eb + es)
      mu <- c(0)

      for (k in 1:layers) {

        if (!is.null(mu_range) & k == 1) {
          muk <- runif(1, min = mu_range[1], max = mu_range[1] +
                         (mu_range[2] - mu_range[1]) / layers)
          muk <- max(muk, 1)

        } else {

          min_distance <- ((
            (x + sqrt((x)^2 + 4 * (eb + es + mu[k] + ub[k]))) / 2) ^2 - eb - es)
          muk <- runif(1, min = 1, max = 1.5) * min_distance
          muk <- runif(1, min = overlap[1], max = overlap[2]) * min_distance
          muk <- max(muk, 1)

        }

        ub[1 + k] <- x * sqrt(eb + es + muk)
        mu <- c(mu, muk)
      }
      mu <- tail(round(mu), -1)
      # conflict (5): mu's outside the mu_range.
      if (!is.null(mu_range)) {
        if (max(mu) > max(mu_range)) {

          high_mus <- length(mu[mu > max(mu_range)])
          details <- c(high_mus, mu_range[1], mu_range[2])
          conflicts <- uiconflicts$add(conflicts, 5, details)

        }
      }
    }

    return(list(conflicts = conflicts, mu = mu))
  },

  layers = function(layers, days, prob) {

    # create a variable lyr so that each layer and no-information cluster
    # at least has 1 day create a vector that takes one value for each layer
    # and create the others randomly. We need to change the prob to take that
    # into consideration.
    iteration <- 0
    all_layers <- FALSE

    while (!all_layers) {

      lyr <- sample(0:layers, size = days, replace = TRUE, prob = prob)
      iteration <- iteration + 1

      if (iteration == 100) {
        temp <- 0:layers
        lyr <- c(temp, sample(0:layers, size = days - layers - 1,
                              replace = TRUE, prob = prob))
      }

      all_layers <- (length(unique(lyr)) == layers + 1)
    }

    return(lyr)
  }

)

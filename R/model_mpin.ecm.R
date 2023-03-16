## - | FILE  HEADER |
##
## Script name:
##    model_mpin.ecm.R
##
## Purpose of script:
##    Implement an ECM algorithm in order to estimate the Multilayer
##    PIN model of Ersan (2016).
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
## Public functions:
## ++++++++++++++++++
##
## mpin_ecm():
##    Estimates the multilayer probability of informed trading
##    (MPIN) using an expectation-conditional maximization algorithm.
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


#' @title MPIN model estimation via an ECM algorithm
#'
#' @description Estimates the multilayer probability of informed trading
#' (`MPIN`) using an Expectation Conditional Maximization algorithm, as in
#' \insertCite{Ghachem2022;textual}{PINstimation}.
#'
#' @usage mpin_ecm(data, layers = NULL, xtraclusters = 4, initialsets = NULL,
#'                       ..., verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param layers An integer referring to the assumed number of
#' information layers in the data. If the argument \code{layers} is given, then
#' the ECM algorithm will use the number of layers provided. If \code{layers} is
#' omitted, the function `mpin_ecm()` will simultaneously optimize the number
#' of layers as well as the parameters of the `MPIN` model.
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{#(1 + layers + xtraclusters)} clusters, thereby resulting in
#' \code{#comb((layers + xtraclusters, layers)} initial parameter sets in line
#' with \insertCite{ErsanAlici2016;textual}{PINstimation}, and
#' \insertCite{Ersan2016;textual}{PINstimation}. The default value is `4`
#' as chosen in \insertCite{Ersan2016;textual}{PINstimation}.
#'
#' @param initialsets A dataframe containing initial parameter
#' sets for estimation of the `MPIN` model. The default value is `NULL`.
#' If `initialsets` is `NULL`, the initial parameter sets are provided by
#' the function `initials_mpin()`.
#'
#' @param ... Additional arguments passed on to the function `mpin_ecm`. The
#' recognized arguments are `hyperparams`, and `is_parallel`.
#' \itemize{
#' \item `hyperparams` is a list containing the hyperparameters of the ECM
#' algorithm. When not empty, it contains one or more  of the following
#' elements: `minalpha`, `maxeval`, `tolerance`, `criterion`, and `maxlayers`.
#' More about these elements are in the details section.
#' \item  `is_parallel` is a logical variable that specifies whether
#' the computation is performed using parallel or sequential processing. The
#' default value is \code{FALSE}. For more details, please refer to the
#' vignette 'Parallel processing' in the package, or
#' \href{https://pinstimation.com/articles/parallel_processing.html}{online}.
#' }
#'
#' @param verbose (`logical`) a binary variable that determines whether detailed
#' information about the steps of the estimation of the MPIN model is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @details The argument 'data' should be a numeric dataframe, and contain
#' at least two variables. Only the first two variables will be considered:
#' The first variable is assumed to correspond to the total number of
#' buyer-initiated trades, while the second variable is assumed to
#' correspond to the total number of seller-initiated trades. Each row or
#' observation correspond to a trading day. `NA` values will be ignored.
#'
#' The initial parameters for the expectation-conditional maximization
#' algorithm are computed using the function `initials_mpin()`  with
#' default settings. The factorization of the `MPIN` likelihood function
#' used is developed by \insertCite{Ersan2016;textual}{PINstimation}, and
#' is implemented in \code{fact_mpin()}.\cr\cr
#' The argument `hyperparams` contains the hyperparameters of the ECM algorithm.
#' It is either empty or contains one or more of the following elements:
#' \itemize{
#'  \item `minalpha` (`numeric`) It stands for the minimum share of days
#' belonging  to a given layer, i.e., layers falling below this threshold are
#' removed during the iteration, and the model is estimated with a lower number
#' of layers. When missing, `minalpha` takes the default value of `0.001`.
#'
#'  \item `maxeval`: (`integer`) It stands for maximum number of iterations of
#' the ECM  algorithm for each initial parameter set. When missing, `maxeval`
#' takes the default value of `100`.
#'
#'  \item `tolerance` (`numeric`) The ECM algorithm is stopped when the
#' (relative) change of log-likelihood is  smaller than tolerance. When
#' missing, `tolerance` takes the default value of `0.001`.
#'
#'  \item `criterion` (`character`) It is the model selection criterion used to
#' find the optimal estimate  for the `MPIN` model. It take one of these values
#' `"BIC"`, `"AIC"` and `"AWE"`; which stand for Bayesian Information
#' Criterion,  Akaike Information Criterion and Approximate Weight of Evidence,
#' respectively \insertCite{Akogul2016}{PINstimation}. When missing,
#' `criterion` takes  the default value of `"BIC"`.
#'
#'  \item `maxlayers` (`integer`) It is the upper limit of number of layers used
#' for estimation in the ECM algorithm.  If the argument `layers` is missing,
#' the ECM algorithm will estimate `MPIN` models for all layers in the integer
#' set from `1` to `maxlayers`. When missing, `maxlayers` takes the default
#' value of `8`.
#'
#'  \item `maxinit` (`integer`) It is the maximum number of initial sets used
#' for each individual estimation in the ECM algorithm. When missing, `maxinit`
#' takes the default value of `100`.
#' }
#'
#' If the argument \code{layers} is given, then the Expectation Conditional
#' Maximization algorithm will use the number of  layers provided. If
#' \code{layers} is omitted, the function \code{mpin_ecm()} will simultaneously
#' optimize the number of layers as well as the parameters of the `MPIN` model.
#' Practically, the function `mpin_ecm()` uses the ECM algorithm to optimize
#' the `MPIN` model parameters for each number of layers within the integer
#' set from `1` to `8` (or to `maxlayers` if specified in the argument
#' `hyperparams`); and returns the optimal model with the lowest Bayesian
#' information  criterion (BIC) (or the lowest information criterion
#' `criterion` if specified in the argument `hyperparams`).
#'
#' @return Returns an object of class \code{estimate.mpin.ecm}.
#'
#' @references
#'  \insertAllCited
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations. Each observation corresponds to a day and contains the
#' # total number of buyer-initiated trades ('B') and seller-initiated
#' # trades ('S') on that day. To know more, type ?dailytrades
#'
#' xdata <- dailytrades
#'
#' # Estimate the MPIN model using the expectation-conditional maximization
#' # (ECM) algorithm.
#'
#' # ------------------------------------------------------------------------ #
#' # Estimate the MPIN model, assuming that there exists 2 information layers #
#' # in the dataset                                                           #
#' # ------------------------------------------------------------------------ #
#'
#' estimate <- mpin_ecm(xdata, layers = 2, verbose = FALSE)
#'
#' # Show the estimation output
#'
#' show(estimate)
#'
#' # Display the optimal parameters from the Expectation Conditional
#' # Maximization algorithm
#'
#' show(estimate@parameters)
#'
#' # Display the global multilayer probability of informed trading
#'
#' show(estimate@mpin)
#'
#' # Display the multilayer probability of informed trading per layer
#'
#' show(estimate@mpinJ)
#'
#' # Display the first five rows of the initial parameter sets used in the
#' # expectation-conditional maximization estimation
#'
#' show(round(head(estimate@initialsets, 5), 4))
#'
#' # ------------------------------------------------------------------------ #
#' # Omit the argument 'layers', so the ECM algorithm optimizes both the      #
#' # number of layers and the MPIN model parameters.                          #
#' # ------------------------------------------------------------------------ #
#' \donttest{
#' estimate <- mpin_ecm(xdata, verbose = FALSE)
#'
#' # Show the estimation output
#'
#' show(estimate)
#'
#' # Display the optimal parameters from the estimation of the MPIN model using
#' # the expectation-conditional maximization (ECM) algorithm
#'
#' show(estimate@parameters)
#'
#' # Display the multilayer probability of informed trading
#'
#' show(estimate@mpin)
#'
#' # Display the multilayer probability of informed trading per layer
#'
#' show(estimate@mpinJ)
#'
#' # Display the first five rows of the initial parameter sets used in the
#' # expectation-conditional maximization estimation.
#'
#' show(round(head(estimate@initialsets, 5), 4))
#' }
#' # ------------------------------------------------------------------------ #
#' # Tweak in the hyperparameters of the ECM algorithm                        #
#' # ------------------------------------------------------------------------ #
#'
#' # Create a variable ecm.params containing the hyperparameters of the ECM
#' # algorithm. This will surely make the ECM algorithm take more time to give
#' # results
#'
#' ecm.params <- list(tolerance = 0.0000001)
#'
#' # If we suspect that the data contains more than eight information layers, we
#' # can raise the number of models to be estimated to 10 as an example, i.e.,
#' # maxlayers = 10.
#'
#' ecm.params$maxlayers <- 10
#'
#' # We can also choose Approximate Weight of Evidence (AWE) for model
#' # selection instead of the default Bayesian Information Criterion (BIC)
#'
#' ecm.params$criterion <- 'AWE'
#'
#' # We can also increase the maximum number of initial sets to 200, in
#' # order to obtain higher level of accuracy for models with high number of
#' # layers.  We set the sub-argument 'maxinit' to `200`. Remember that its
#' # default value is `100`.
#'
#' ecm.params$maxinit <- 200
#' \donttest{
#' estimate <- mpin_ecm(xdata, xtraclusters = 2, hyperparams = ecm.params,
#'                                                       verbose = FALSE)
#'
#' # We can change the model selection criterion by calling selectModel()
#'
#' estimate <- selectModel(estimate, "AIC")
#'
#' # We get the mpin_ecm estimation results for the MPIN model with 2 layers
#' # using the slot models. We then show the first five rows of the
#' # corresponding slot details.
#'
#' models <- estimate@models
#' show(round(head(models[[2]]@details, 5), 4))
#'
#' # We can also use the function getSummary to get an idea about the change in
#' # the estimation parameters as a function of the number of layers in the
#' # MPIN model. The function getSummary returns a dataframe that contains,
#' # among others, the number of layers of the model, the number of layers in
#' # the optimal model,the MPIN value, and the values of the different
#' # information criteria, namely AIC, BIC and AWE.
#'
#' summary <- getSummary(estimate)
#'
#' # We can plot the MPIN value and the layers at the optimal model as a
#' # function of the number of layers to see whether additional layers in the
#' # model actually contribute to a better precision in the probability of
#' # informed trading. Remember that the hyperparameter 'minalpha' is
#' # responsible for dropping layers with "frequency" lower than 'minalpha'.
#'
#' plot(summary$layers, summary$MPIN,
#'    type = "o", col = "red",
#'    xlab = "MPIN model layers", ylab = "MPIN value"
#'  )
#'
#' plot(summary$layers, summary$em.layers,
#'    type = "o", col = "blue",
#'    xlab = "MPIN model layers", ylab = "layers at the optimal model"
#' )
#' }
#' @export
mpin_ecm <- function(data, layers = NULL, xtraclusters = 4, initialsets = NULL,
                    ..., verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  allvars <- allvars[-5]
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$mpin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, layers, xtraclusters, initialsets, 0, verbose)
  names(largs) <- names(formals())
  largs[["..."]] <- NULL

  # Assign the dot-dot-dot arguments
  hyperparams <- list()
  is_parallel <- .default$mpin_parallel
  maxlayers <- 5

  vargs <- list(...)
  # check for unknown keys in the argument "..."
  unknown <- setdiff(names(vargs), c("hyperparams", "is_parallel"))
  ux$stopnow(length(unknown) > 0, s = uierrors$mpin()$fn,
             m = uierrors$arguments()$unknown(u = unknown))

  # Collect the arguments in the dot-dot arguments
  if (length(vargs) > 0 && "hyperparams" %in% names(vargs))
    hyperparams <- vargs$hyperparams
  if (length(vargs) > 0 && "is_parallel" %in% names(vargs))
    is_parallel <- vargs$is_parallel
  largs$is_parallel <- is_parallel
  largs$hyperparams <- hyperparams

  rst <- .xcheck$args(arglist = largs, fn = "mpin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)

  rst <- .xcheck$hyperparams(hyperparams, nrow(data))
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  hyperparams <- rst$hyperparams
  hpn <- names(hyperparams)
  for (i in seq_len(length(hpn)))
    assign(hpn[i], unname(unlist(hyperparams[[i]])))

  # Prepare the data
  # ------------------------------------------------------------------------
  data <- ux$prepare(data)

  # If both layers and xtraclusters are given, check that they are compatible
  # If layers is NULL, then check that it compatible with maxlayers
  if (!is.null(layers)) {
    rst <- .xcheck$xclusters(n = nrow(data), lay = layers,
                             xtra = xtraclusters)
    ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  } else {
    rst <- .xcheck$xclusters(n = nrow(data), lay = maxlayers,
                             xtra = xtraclusters)
    ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  }

  # Call the function .estimate_ecm_mpin() responsible for the ECM estimation.
  # Pass the arguments as they are.

  results <- suppressWarnings(
    .estimate_ecm_mpin(data, xtraclusters = xtraclusters, layers = layers,
                     initialsets, hyperparams = hyperparams,
                     is_parallel = is_parallel, verbose = verbose))
  return(results)
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


.estimate_ecm <- function(data, layers, initialsets, hyperparams,
                         is_parallel, verbose = TRUE) {
# Implements the Expectation-Conditional Maximization algorithm for
# all parameter sets in the list 'initialsets' provided by the user
#
# Args:
#   data        : the dataset of buys and sells
#   layers      : the number of layers in the data
#   initialsets : a dataframe of initial parameter sets
#   hyperparams : the set of hyperparameters
#   verbose     : if TRUE, details about the progress are displayed
#
# Returns:
#   returns an object 'estimate.mpin.ecm' containing all relevant
#   optimal parameters

  # initialize the local variables
  # --------------------------------------------------------------------------
  criterion <- NULL

  time_on <- Sys.time()

  # Prepare 'data' and initialize variables
  # ----------------------------------------------------------------------------
  data <- ux$prepare(data)

  # Update 'hyperparams' by filling missing hyperparameters, and distribute the
  # new list to six different variables: 'criterion', 'minalpha', 'maxlayers',
  # 'maxeval', 'external', 'tolerance'.
  # ----------------------------------------------------------------------------
  hps <- hyperparams
  hpn <- names(hps)
  for (i in seq_len(length(hpn))) assign(hpn[i], unname(unlist(hps[[i]])))


  # Prepare the initial sets to be used by the function .ecm_mpin_oneset()
  # ----------------------------------------------------------------------------
  cls <- 2 * layers + 1
  initialpoints <- initialsets

  create_prob <- function(z) {
    a <- z[1:layers]
    d <- z[(layers + 1):(2 * layers)]
    return(unlist(c(1 - sum(a), as.vector(rbind(a * (1 - d), a * d)))))

  }

  params <- lapply(initialpoints, function(x) {
    list(distrib = create_prob(x),
         muj = x[(2 * layers + 1):(3 * layers)],
         eb = x[3 * layers + 1],
         es = x[3 * layers + 2])
  })


  # Initialize the variables MLE to be used in the loop of estimation: max_mle
  # ----------------------------------------------------------------------------
  convergent <- 0
  optimal <- list(likelihood = -Inf)
  runs <- data.frame(matrix(NA, ncol = 6 * layers + 8, nrow = 0))

  .get_run <- function(current) {

    # Prepare the current parameters, and the details dataframe
    # -----------------------------------------------------------------------
    currentparams <- params[[current]]
    temp_run <- c(layers, unlist(initialpoints[[current]]))
    full <- length(params)

    # Code for displaying the progress bar
    # --------------------------------------------------------------------------
    if (verbose)
      pb_mpin_ecm <- ux$progressbar(minvalue = current - 1, maxvalue = full)

    # Estimate the ECM model by calling the function .ecm_mpin_oneset().
    # If the output of .ecm_mpin_oneset() is valid, calculate its likelihood
    # using -factorizations$mpin(), and compare it to previous optimal estimate,
    # keep it only when it has higher likelihood.
    # --------------------------------------------------------------------------
    estimates <- .em_mpin_oneset(currentparams, layers = layers, data = data,
                                 hyperparams = hps)


    if (length(estimates$parameters) > 0) {

      # It is possible that the number of optimal layers is not the same
      # as the number of layers in the starting model. Since the dataframe
      # 'runs', which will appear in the slot 'details' of the estimation
      # result (S4 object), assumes a number of layers equal to the starting
      # one, we need to adjust the optimal results, in case the number of
      # optimal layers is lower than the starting number of layers.
      lost_layers <- layers - length(estimates$parameters$alpha)
      mpin_params <- unlist(estimates$parameters)

      if (lost_layers > 0) {
        mpin_params <- c(
          estimates$parameters$alpha, rep(0, lost_layers),
          estimates$parameters$delta, rep(0, lost_layers),
          estimates$parameters$mu, rep(0, lost_layers),
          estimates$parameters$eps.b, estimates$parameters$eps.s
        )
      }

      # The vector thisrun contains all optimal parameters, alongside the
      # list 'estimates' containing the results of the ECM estimation.
      thisrun <- c(
        list(c(temp_run, estimates$layers, mpin_params, estimates$likelihood,
               .xmpin$compute_pin(estimates$parameters), estimates$xtime,
               estimates$iterations)), I(list(estimates)))
    } else {

      thisrun <- c(list(c(temp_run, rep(0, 3 * layers + 3),
                          -Inf, 0), I(list(estimates))))

    }

    # Update the progress bar in the parent environment
    pe <- parent.env(environment())
    if (verbose)
      setTxtProgressBar(pe$pb_mpin_ecm, current)

    return(thisrun)
  }


  # Loop over the initial sets to find the optimal estimates
  # ----------------------------------------------------------------------------
  xs <- seq_len(length(initialpoints))

  ux <- ux
  uix <- uix
  .xmpin <- .xmpin

  mpin_ms <- uix$mpin(layers = layers)

  if (verbose) {
    pb_mpin_ecm <- ux$progressbar(minvalue = 0, maxvalue = length(params))
    cat(mpin_ms$emprogressbar)
  }

  if (is_parallel & length(params) >= .default$parallel_cap()) {

    oplan <- future::plan(multisession, gc = TRUE,
                          workers = .default$parallel_cores())

    on.exit(plan(oplan), add = TRUE)

    runs <- furrr::future_map(xs, function(x) .get_run(x))

  } else {

    runs <- lapply(xs, .get_run)

  }

  if (length(runs) > 1) {


    # Receive the list of all runs, each list element contains two list elements
    # the first one is all parameters relative to the run, and the second is the
    # list object, outcome of the ECM estimation.
    estimates <- lapply(runs, "[[", 2)

    # Get and format the dataframe 'runs' which will be contained in the slot
    # 'details' of the optimal estimate
    xruns <- lapply(runs, "[[", 1)
    runs <- data.frame(do.call(rbind, xruns))

    colnames(runs) <- .xmpin$varnames(6, layers)
    rownames(runs) <- paste("set.", seq_len(nrow(runs)), sep = "")

    # After naming the columns, only keep the columns that are not all zeros
    # This reduces the size of the dataframe, and removes empty layers
    runs <- runs[, colSums(runs, na.rm = TRUE) != 0]

    # Get the list of all likelihood. The number 'convergent' is the number of
    # all runs, for which the likelihood value is finite. If convergent is
    # different from zero, then there is an optimal estimate corresponding to
    # the one with highest likelihood value.
    lkdruns <- unlist(runs$likelihood)
    convergent <- length(lkdruns[is.finite(lkdruns)])
    if (convergent > 0) {
      optimizer <- which.max(lkdruns)
      optimal <- estimates[[optimizer]]
    }

  }

  time_off <- Sys.time()

  # Depending on results, return optimal results as an 'estimate.mpin' S4 object
  # If max_mle == 0, then no estimates have been optimal.
  # Initialize the return "estimate.mpin" object to the value it to the value
  # when it fails.
  # ----------------------------------------------------------------------------
  initialpoints <- ux$todframe(initialpoints)
  names(initialpoints) <- .xmpin$varnames(4, layers)


  mpin_optimal <- new(
    "estimate.mpin.ecm", success = FALSE, convergent.sets = convergent,
    method = "ECM", errorMessage = uierrors$mpin()$emfailed,
    parameters = list(), likelihood = -Inf,
    initialsets = initialpoints, AIC = +Inf, BIC = +Inf, AWE = +Inf,
    criterion = criterion, hyperparams = hps, dataset = data,
    runningtime = ux$timediff(time_on, time_off)
  )

  if (is.finite(optimal$likelihood)) {

    xlist <- .xmpin$get_goodbadmpin(optimal$MPINj, optimal$parameters)

    mpin_optimal@errorMessage <- ""
    mpin_optimal@success <- TRUE
    mpin_optimal@parameters <- optimal$parameters
    mpin_optimal@aggregates <- optimal$aggregates
    mpin_optimal@likelihood <- optimal$likelihood
    mpin_optimal@mpinJ <- optimal$MPINj
    mpin_optimal@mpin <- optimal$MPIN
    mpin_optimal@mpin.goodbad <- xlist
    mpin_optimal@details <- runs
    mpin_optimal@AIC <- optimal$AIC
    mpin_optimal@BIC <- optimal$BIC
    mpin_optimal@AWE <- optimal$AWE
    mpin_optimal@layers <- optimal$layers
    mpin_optimal@parallel <- is_parallel

  }

  return(mpin_optimal)

}

.estimate_ecm_mpin <- function(data, xtraclusters = 5, layers = NULL,
                             initialsets = NULL, hyperparams = list(),
                             is_parallel = FALSE, verbose = TRUE) {
# Adjusts the set of initial parameter sets 'initialsets' and sends it to the
# function '.estimate_ecm' for ECM estimation
#
# Args:
#   data        : the dataset of buys and sells
#   xtraclusters: the number of xtraclusters to use it in initials_mpin()
#   layers      : the number of layers (provided or detected) in the data
#   initialsets : a list of initial parameter sets
#   hyperparams : the set of hyperparameters
#   is_parallel : parallel processing is used when TRUE
#   verbose     : if TRUE, details about the progress are displayed
#
# Returns:
#   returns an object 'estimate.mpin.ecm' containing optimal parameters

  # Prepare 'data' and initialize variables
  # ----------------------------------------------------------------------------
  data <- ux$prepare(data)
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)
  maxlayers <- criterion <- maxinit <- NULL

  # Update 'hyperparams' by filling missing hyperparameters, and distribute the
  # new list to six different variables: 'criterion', 'minalpha', 'maxlayers',
  # 'maxeval', 'external', 'tolerance'. Recover it from dot-dot-dot arguments
  # ----------------------------------------------------------------------------
  em_hps <- hyperparams
  hpn <- names(em_hps)
  for (i in seq_len(length(hpn))) assign(hpn[i], unname(unlist(em_hps[[i]])))

  # function organize_and_perturb() makes sure that the top initial parameter
  # sets do not have duplicated alpha(1) so a larger part of the parameter
  # space is covered, and that the alpha and the delta are perturbed away
  # from zero. The argument max_activated captures whether the number of
  # initial sets returned is capped by maxinit or not.

  organize_and_perturb <- function(initialsets, max_activated = FALSE) {

    # Reorganize the initial sets with so that the first initial sets cover a
    # share as wide as possible from the parameter space (alpha more
    # specifically). We make sure that the first alphas are not duplicated.
    # --------------------------------------------------------------------------
    top <- initialsets[!duplicated(initialsets[, 1]), ]
    bottom <- initialsets[duplicated(initialsets[, 1]), ]
    initialsets <- as.data.frame(rbind(top, bottom))

    # Transform the dataframe 'initialsets' to a list
    initialsets <- ux$tolist(initialsets)

    # If max_activated is true, select only the first maxinit elements
    # of the list
    if (max_activated & maxinit < length(initialsets)) {
          initialsets <- initialsets[1:maxinit]
    }

    # Perturb the values of delta if they are 0 or 1, set them to a small
    # number so that the ECM algorithm is able to update the value away
    # from zero or one
    nu <- 10^-4
    initialsets <- lapply(initialsets,
                          function(x) x + (x == 0) * nu -  (x == 1) * nu)
    return(initialsets)
  }

  # Find initial sets for each configuration of layers and use .estimate_ecm
  # to estime ECM and get the optimal results to be returned to mpin_ecm
  # function.
  # ----------------------------------------------------------------------------
  if (!is.null(initialsets) | !is.null(layers)) {

    # Call the errors of the MPIN model
    mpin_err <- uierrors$mpin(ncol(initialsets))


    if (!is.null(initialsets)) {

      # The number of layers 'xlayers' is deduced from the initial sets
      xlayers <- (length(initialsets[1, ]) - 2) / 3

      # xlayers should be an integer, otherwise throw error
      ux$stopnow(
        !ux$integer(xlayers), m = mpin_err$wronglength,
        s = mpin_err$fn)

      nrows <- nrow(initialsets)

      if (is.null(layers)) layers <- xlayers

      mpin_ms <- uix$mpin(
        nrows = nrows, initlayers = xlayers, layers = layers)

      ux$show(verbose, m = mpin_ms$start)

      ux$show(verbose, m = mpin_ms$detectsets)

      ux$show(verbose & (xlayers != layers),
                  m = mpin_ms$differentlayers, warning = TRUE)

      ux$show(verbose, m = mpin_ms$loadinitials)

      detection <- "INITIALSETS"

    } else {

      mpin_ms <- uix$mpin(layers = layers)

      ux$show(verbose, m = mpin_ms$start)

      ux$show(verbose, m = mpin_ms$selectedlayers)

      initialsets <- initials_mpin(data, layers = layers,
                                xtraclusters = xtraclusters, verbose = FALSE)
      rownames(initialsets) <- NULL

      mpin_ms <- uix$mpin(nrows = nrow(initialsets))

      ux$show(verbose, m = mpin_ms$computinginitials)

      detection <- "USER"

    }


    # If the user has specifically set maxinit in the argument hyperparams, then
    # restrict the number of sets not to exceed maxinit, otherwise, let it
    # be free. If the number of generated initial sets is larger than maxinit,
    # then show a message, communicating that few initial sets have been
    # selected.
    max_activated <- !(is.null(hyperparams$maxinit)) &
      (nrow(initialsets) > maxinit)

    mpin_ms <- uix$mpin(maxinit = maxinit, layers = layers)
    ux$show(verbose & max_activated, m = mpin_ms$selectinitials)

    initialsets <- organize_and_perturb(initialsets, max_activated)

    # Estimate the MPIN model using the initial sets computed or provided
    ux$show(verbose, m = mpin_ms$emmethod)

    mpin_optimal <- suppressWarnings(
      .estimate_ecm(data, layers, initialsets, hyperparams,
                   is_parallel, verbose = verbose))

    attr(mpin_optimal, "posteriors") <- .mpin_posteriors(
      data, unlist(mpin_optimal@parameters))

    ux$show(verbose, m = mpin_ms$complete)

  } else {

    mpin_ms <- uix$mpin(maxlayers = maxlayers, maxinit = maxinit)

    ux$show(verbose, m = mpin_ms$start)

    ux$show(verbose, m = mpin_ms$maxlayersrange)

    ux$show(verbose, m = mpin_ms$computinginitials)

    ux$show(verbose, m = mpin_ms$selectinitials)

    # Estimate the MPIN model using the initial sets computed or provided
    ux$show(verbose, m = mpin_ms$emmethod)


    mpin_optimal <- NULL
    time_on <- Sys.time()
    allmodels <- list()

    for (k in 1:maxlayers) {

      # Compute the initial sets
      initsets <- initials_mpin(data, layers = k, xtraclusters =
                                  xtraclusters, verbose = FALSE)
      rownames(initsets) <- NULL

      initsets <- organize_and_perturb(initsets, max_activated = TRUE)

      # estimate the candidate model and add it to the list of models
      candidate <- .estimate_ecm(data, layers = k, initialsets = initsets,
                                 hyperparams = hyperparams, is_parallel =
                                  is_parallel, verbose = verbose)

      # Make sure that the list of parameters is properly named.
      xlayers <- candidate@layers
      xparams <- candidate@parameters
      xnames <- paste("layer.", 1:xlayers, "", sep = "")
      parameters <- c(
        alpha = list(setNames(xparams$alpha, xnames)),
        delta = list(setNames(xparams$delta, xnames)),
        mu = list(setNames(xparams$mu, xnames)),
        eps.b = list(xparams$eps.b), eps.s = list(xparams$eps.s))

      candidate@parameters <- parameters

      freeparams <- (3 * candidate@layers + 2)
      candidate@AIC <- 2 * freeparams - 2 * candidate@likelihood
      candidate@BIC <- log(nrow(data)) * freeparams - 2 * candidate@likelihood
      candidate@AWE <- -2 * candidate@likelihood +
        2 * freeparams * (1.5 + log(nrow(data)))
      candidate@detection <- "ECM"

      attr(candidate, "posteriors") <- .mpin_posteriors(
        data, unlist(candidate@parameters))

      allmodels <- c(allmodels, list(candidate))

      # set the optimal candidate model. If the optimal object is empty, store
      # the current model, otherwise, comparing the current model with the
      # incumbent model using the selection criterion using the function
      # getcriterion() and keep the one with highest selection criterion

      getcriterion <- function(object, criterion) {
        return(switch(criterion, BIC = object@BIC, AIC = object@AIC, AWE =
                        object@AWE))
      }

      if (is.null(mpin_optimal)) {
        mpin_optimal <- candidate
        mpin_optimal@optimal <- 1
      }

      if (getcriterion(candidate, criterion) <
          getcriterion(mpin_optimal, criterion)) {
        mpin_optimal <- candidate
        mpin_optimal@likelihood <- -factorizations$mpin(
          data)(unlist(mpin_optimal@parameters))
        mpin_optimal@optimal <- k
      }

    }

    time_off <- Sys.time()

    mpin_optimal@runningtime <- ux$timediff(time_on, time_off)

    mpin_optimal@models <- invisible(allmodels)

    mpin_ms <- uix$mpin(criterion = criterion)
    ux$show(verbose, m = mpin_ms$selectcriterion)

    ux$show(verbose, m = mpin_ms$emcomplete)

    detection <- "ECM"
  }

  mpin_optimal@detection <- detection
  mpin_optimal@parallel <- is_parallel

  return(mpin_optimal)
}

.em_mpin_oneset <- function(params, layers,  data, hyperparams) {
  # Implements the Expectation-Conditional Maximization algorithm for
  # one set of MPIN model parameters
#
# Args:
#   j           : refers to the cluster j
#   distrib     : the distribution of cluster probabilities
#   eb          : the value of uninformed trading (buys)
#   es          : the value of uninformed trading (sells)
#   muj         : the set of informed trading rates
#   data        : the dataset of buys and sells
#   layers      : the number of layers in the data
#   hyperparams : the set of hyperparameters
#
# Returns:
#   returns a list of optimal parameters output of ECM algorithm

  # initialize the local variables
  # --------------------------------------------------------------------------
  tolerance <- maxeval <- minalpha <- NULL
  distrib <- params$distrib
  eb <- params$eb
  es <- params$es
  muj <- params$muj
  layers <- length(muj)

  # cls is the number of clusters and layers is the number of layers
  cls <- 2 * layers + 1

  # Update 'hyperparams' by filling missing hyperparameters, and distribute the
  # new list to six different variables: 'criterion', 'minalpha', 'maxlayers',
  # 'maxeval', 'external', 'tolerance'.
  # ----------------------------------------------------------------------------
  hps <- hyperparams
  hpn <- names(hps)
  for (i in seq_len(length(hpn))) assign(hpn[i], unname(unlist(hps[[i]])))

  # Use the function loglikhood to compute the first two values in the vector
  # loglik, which will store the log likelihood value in each iteration.
  # ---------------------------------------------------------------------------
  loglikhood <- function(z, p, eb, es, lambda, b, s) {
    logdensity <- vapply(1:cls, .xmpin$logpmf, eb = eb, es = es,
                         lambda = lambda, b, s,
                         FUN.VALUE = double(length(b)))

    logdensity[is.na(logdensity)] <- 0
    return(ux$finite_sum(z * log(p)) + ux$finite_sum(z * logdensity))
  }

  loglik <- vector()
  loglik[1] <- 0
  loglik[2] <- loglikhood(z = distrib, p = distrib, eb, es,
                          lambda = params$muj, data$b, data$s)
  iteration <- 2
  interrupted <- FALSE

  while (abs(loglik[iteration] - loglik[iteration - 1]) >= tolerance &&
         iteration <= maxeval) {

    # ----------------------------------------------------------------------- #
    #                             EXPECTATION STEP                            #
    # ----------------------------------------------------------------------- #

    estimateQ <- function(eb, es, muj, distribution, mergelayers = TRUE) {

      # Compute the posterior matrix which, in row i, column k, stores the
      # probability that observation i belongs to cluster k . If the sum of
      # the posterior matrix is zero (model is very different from the data),
      # the process is aborted. The days having a posterior probability of zero,
      # get the posterior probability of an average day across all observations.
      # ------------------------------------------------------------------------
      posterior_mx <- vapply(1:cls, .xmpin$posterior, p = distribution, eb,
                             es, muj, data$b, data$s,
                             FUN.VALUE = double(length(data$b)))
      posterior_mx[is.na(posterior_mx)] <- 0

      if (sum(posterior_mx) == 0) return(list(interrupted = T))

      # Replace the content of a row whose sum is zero by equiprobable
      # assignment to clusters. The observations has equal probability to
      # belong to any of the six clusters.
      daily_posterior <- rowSums(posterior_mx)
      zerorows <- which(daily_posterior == 0)
      if (length(zerorows) > 0)
        posterior_mx[zerorows,] <- rep(1/6,6)


      # yn: vector of cluster membership where yn(ij) contain the prob. that
      # obs. i belongs to cluster j. This posterior distribution over clusters
      # will not be used in the current M step, but serves as a basis for
      # calculing the distribution for the next E step. The distribution of days
      # among cluster is simply the sum of probabilities per cluster.
      # ------------------------------------------------------------------------
      yn <- sweep(posterior_mx, 1, rowSums(posterior_mx, na.rm = TRUE), `/`)

      newdistrib <- colMeans(yn, na.rm = TRUE)

      if (mergelayers) {

        dx <- newdistrib
        alpha <- dx[seq(3, cls, 2)] + dx[seq(2, cls, 2)]
        todrop <- which(alpha < minalpha)

        # Drop the information layers whose alpha is too small (<minalpha)
        # ----------------------------------------------------------------------
        if (length(todrop) > 0) {

          dropped <- .xmpin$drop_layers(newdistrib, todrop, muj)

          if (!is.null(dropped)) {

            if(length(dropped$distrib) < 3)
              return(list(interrupted = T, distribution = newdistrib))

            return(list(
              tonext = T,
              distribution = dropped$distrib,
              layers = dropped$layers,
              cls = dropped$cls,
              muj = dropped$muj,
              newdistribution = dropped$distrib
            ))
          }
        }



        # Merge clusters where the values of mu's are very close to one another.
        # ----------------------------------------------------------------------
        merged <- .xmpin$merge_layers(newdistrib, eb, es, muj, layers)

        if (!is.null(merged)) {

          if(length(merged$distrib) < 3)
            return(list(interrupted = T, distribution = newdistrib))

          return(list(
            tonext = T,
            distribution = merged$distrib,
            newdistribution = merged$distrib,
            muj = merged$muj,
            layers = merged$layers,
            cls = merged$cls
          ))
        }
      }


      # There are some conditions, when fulfilled, the ECM algorithm breaks
      # either NA values for yn or less than three clusters or alpha is equal to
      # zero (impossible to estimate delta)
      interrupted <- (any(is.na(yn)) | cls < 3 | newdistrib[1] == 1)

      if (interrupted)
        return(list(interrupted = T, distribution = newdistrib))

      # yn is optimized | we used to construct a likelihood function
      # yl : posterior probability for layers - excluding the non-event cluster
      # yg : posterior probability for good information layers
      # yb : posterior probability for bad information layers
      # y0 : posterior probability for the no-information cluster
      # n : number of days in the data
      # ------------------------------------------------------------------------
      n <- length(data$b)
      yl <- yn[, 2:length(yn[1, ])]
      yg <- yl[, c(TRUE, FALSE)]
      yb <- yl[, !c(TRUE, FALSE)]
      y0 <- yn[, 1]

      # beta_* : weighted sum of y, and b when info is * in {(G)ood, (b)ad, 0}
      # sigma_*: weighted sum of y, and s when info is * in {(G)ood, (b)ad, 0}
      # The values are numbers when layer == 1, and a vector if layers > 1
      # ------------------------------------------------------------------------
      if (layers > 1) {

        beta_g <- colSums(yg * data$b, na.rm = TRUE)
        sigma_b <- colSums(yb * data$s, na.rm = TRUE)
        beta_b <- colSums(yb * data$b, na.rm = TRUE)
        sigma_g <- colSums(yg * data$s, na.rm = TRUE)
        yj <- colSums(yg + yb, na.rm = TRUE)

      } else {

        beta_g <- sum(yg * data$b, na.rm = TRUE)
        sigma_b <- sum(yb * data$s, na.rm = TRUE)
        beta_b <- sum(yb * data$b, na.rm = TRUE)
        sigma_g <- sum(yg * data$s, na.rm = TRUE)
        yj <- sum(yg + yb, na.rm = TRUE)

      }

      beta_0 <- sum(y0 * data$b, na.rm = TRUE)
      sigma_0 <- sum(y0 * data$s, na.rm = TRUE)

      # The values beta_0b = beta_0 + beta_b ; sigma_0G = sigma_0 + sigma_g
      # y_k is the vector containing the total y per information layer.
      # y0 contains the total value for y in the no-information cluster
      # ------------------------------------------------------------------------
      beta_0b <- beta_0 + sum(beta_b, na.rm = TRUE)
      sigma_0g <- sigma_0 + sum(sigma_g, na.rm = TRUE)
      y_0 <- sum(y0, na.rm = TRUE)

      LQ <- list(
        B0 = beta_0, Bg = beta_g, Bb = beta_b, B0b = beta_0b,
        S0 = sigma_0, Sg = sigma_g, Sb = sigma_b, S0g = sigma_0g,
        y0 = y_0, yj = yj, yn = yn, distribution = distrib,
        n = n, newdistribution = newdistrib
        )
    }

    # Use the function estimateQ() to estimate the complete data log-likelihood
    # function.

    LQ <- estimateQ(eb, es, muj, distrib)

    if (!is.null(LQ$interrupted) && LQ$interrupted) break
    if (!is.null(LQ$tonext) && LQ$tonext) {
      distrib <- LQ$distribution
      layers <- LQ$layers
      cls <- LQ$cls
      muj <- LQ$muj
      next
    }

    # ------------------------------------------------------------------------ #
    #                         ECM - MAXIMIZATION STEP                          #
    # ------------------------------------------------------------------------ #

    # Use the ECM method to optimize the model parameters
    oldparams <- c(eb, es, muj)

    # Optimize muj conditional on the existing eb, es
    if (length(LQ$Bg) > 1) {

      mparams <- data.frame(cbind(LQ$Bg, LQ$Sb, LQ$yj, muj))
      lparams <- ux$tolist(mparams)

    } else {

      lparams <- list(c(LQ$Bg, LQ$Sb, LQ$yj, muj))

    }

    omuj <- lapply(
      lparams, function(x) .xadjpin$solve_eqx(
        x[1:2], c(eb, es), x[3], x[4]))

    omuj <- unname(unlist(omuj))

    # Optimize eb conditional on the newly optimized omuj
    oeb <- .xadjpin$solve_eqx(
      a = c(LQ$B0b, LQ$Bg), b = c(0, omuj),  c = LQ$n, d = eb)

    # Optimize es conditional on the newly optimized omuj
    oes <- .xadjpin$solve_eqx(
      a = c(LQ$S0g, LQ$Sb), b = c(0, omuj),  c = LQ$n, d = es)

    dx <- c(oeb, oes, omuj) - oldparams
    if (all(dx == 0)) break

    eb <- oeb
    es <- oes
    muj <- omuj

    # Update the variable 'distrib to store the posterior distribution
    # --------------------------------------------------------------------------
    distrib <- LQ$newdistribution

    # Compute the new value of the log-likelihood, given new optimal parameters
    # --------------------------------------------------------------------------
    loglik[iteration + 1] <- loglikhood(z = LQ$yn, p = distrib, eb, es,
                                        lambda = muj, data$b, data$s)
    iteration <- iteration + 1

  }

  # Initialize the return value 'output' to its value in the case of interrupted
  # ECM optimization.
  # ----------------------------------------------------------------------------
  output <- list(AIC = +Inf, BIC = +Inf, AWE = +Inf, likelihood = -Inf,
                 MPIN = 0, MPINj = 0, parameters = list())

  if (!interrupted) {

    # Find the distribution of bad information clusters (dblayers), of good
    # information clusters (dglayers), and sum them to get the distribution of
    # alpha
    dblayers <- distrib[3]
    dglayers <- distrib[2]
    alpha <- dblayers + dglayers

    if (cls > 3) {
      dblayers <- distrib[seq(3, cls, 2)]
      dglayers <- distrib[seq(2, cls, 2)]
      alpha <- dblayers + dglayers
    }

    d <- dblayers / alpha
    pin <- sum(alpha * muj) / (sum(alpha * muj) + eb + es)

    pinj <- setNames(alpha * muj / (sum(alpha * muj) + eb + es),
                     paste("layer.", 1:layers, "", sep = ""))

    aggregates <- c(sum(alpha), sum(alpha * d) / sum(alpha),
                    sum(alpha *  muj) / sum(alpha), eb, es)

    names(aggregates) <- .xmpin$varnames(2)

    parameters <- c(
      list(setNames(alpha,  paste("layer.", 1:layers, "", sep = ""))),
      list(setNames(d,  paste("layer.", 1:layers, "", sep = ""))),
      list(setNames(muj,  paste("layer.", 1:layers, "", sep = ""))),
      list(eb), list(es))
    names(parameters) <- .xmpin$varnames(2)

    layers <- length(alpha)

    likhood <- -factorizations$mpin(data)(parameters)

    optimal_layers <- length(alpha)
    freeparams <- (3 * optimal_layers + 2)
    aic <- 2 * freeparams - 2 * likhood
    bic <- log(nrow(data)) * freeparams - 2 * likhood
    awe <- -2 * likhood + 2 * freeparams * (1.5 + log(nrow(data)))

    output <- list(
      AIC = aic, BIC = bic, AWE = awe, criterion = "BIC",
      likelihood = likhood, MPIN = pin, MPINj = pinj, layers = layers,
      parameters = parameters, aggregates = aggregates)

  }


  return(output)

}


.xmpin <- list(

  selectModel = function(object, criterion) {

    # initialize the local variables
    models <- optimal <- NULL

    # If the current object is selected based on the criterion 'criterion'
    # then return the object itself.
    if (criterion == object@criterion) return(object)

    # store the list of models into a variables models
    models <- object@models

    # Browse the models and select the model with lowest info. criterion

    criteria <- switch(
      criterion,
      "BIC" = lapply(models, function(x) x@BIC),
      "AIC" = lapply(models, function(x) x@AIC),
      "AWE" = lapply(models, function(x) x@AWE)
    )

    criteria <- unlist(criteria)
    object@optimal <- which.min(criteria)
    object@criterion <- criterion

    return(object)

  },

  compute_pin = function(params) {
    # Computes the value of the probability of informed trading (PIN)
    # given a set of parameters 'params'
    #
    # Args:
    #   params : a vector of parameters of the PIN model
    #            (alpha, delta, mu, eps.b, eps.s)
    #
    # Returns:
    #   a numerical value of the probability of informed trading (PIN)

    a <- mu <- eb <- es <- pin <- NULL
    layers <- (length(params) - 2) / 3

    variables <- c("a", "d", "mu", "eb", "es")
    values <- unname(split(params, rep(1:5, c(layers, layers, layers, 1, 1))))

    for (i in seq_len(length(values)))
      assign(variables[i], unname(unlist(values[[i]])))

    pin <- sum(a * mu) / (sum(a * mu) + eb + es)

    return(pin)
  },

  varnames = function(x = 1, layers = 0) {

    vnames <- c(
      list(c("a", "d", "mu", "eb", "es")),
      list(c("alpha", "delta", "mu", "eps.b", "eps.s")),
      list(c("in.alpha", "in.delta", "in.mu", "in.eps.b", "in.eps.s",
             "op.alpha", "op.delta", "op.mu", "op.eps.b", "op.eps.s",
             "likelihood", "PIN")),
      list(c(paste("alpha.", 1:layers, sep = ""),
             paste("delta.", 1:layers, sep = ""),
             paste("mu.", 1:layers, sep = ""), "eps.b", "eps.s")),
      list(c(paste("in.alpha.", 1:layers, sep = ""),
             paste("in.delta.", 1:layers, sep = ""),
             paste("in.mu.", 1:layers, sep = ""), "in.eps.b", "in.eps.s",
             paste("op.alpha.", 1:layers, sep = ""),
             paste("op.delta.", 1:layers, sep = ""),
             paste("op.mu.", 1:layers, sep = ""),
             "op.eps.b", "op.eps.s", "likelihood", "MPIN")),
      list(c("in.layer", paste("in.alpha.", 1:layers, sep = ""),
             paste("in.delta.", 1:layers, sep = ""),
             paste("in.mu.", 1:layers, sep = ""), "in.eps.b", "in.eps.s",
             "op.layer", paste("op.alpha.", 1:layers, sep = ""),
             paste("op.delta.", 1:layers, sep = ""),
             paste("op.mu.", 1:layers, sep = ""), "op.eps.b", "op.eps.s",
             "likelihood", "MPIN"))
    )

    return(vnames[[x]])
  },

  optimaldetails = function(object) {

    # store the list of models into a variables models
    models <- object@models

    # Extract two pieces of information from the object models
    # The running time, and the number of initial sets for each individual
    # MPIN estimation.
    rlayers <- lapply(models, function(x) x@layers)
    rtimes <- lapply(models, function(x) round(x@runningtime, 2))
    rninit <- lapply(models, function(x) nrow(x@initialsets))
    rbic <- lapply(models, function(x) round(x@BIC, 2))
    raic <- lapply(models, function(x) round(x@AIC, 2))
    rawe <- lapply(models, function(x) round(x@AWE, 2))

    tab <- rbind(
      BIC = rbic, AIC = raic, AWE = rawe,
      layers = rlayers, "#Sets" = rninit, time = rtimes
    )
    tab <- as.data.frame(tab)

    # Create the headers of the table, and add a star to the optimal model
    headers <- paste("model.", seq_len(length(models)), sep = "")
    xstar <- "**"
    headers[object@optimal] <- paste(headers[object@optimal],
                                     xstar, sep = "")

    colnames(tab) <- headers
    tab <- t(tab)

    totaltime <- sum(unlist(rtimes))
    totalinit <- sum(unlist(rninit))

    response <- list(rtime = totaltime, rinit = totalinit, tab = tab)

    return(response)

  },

  logpmf = function(j, eb, es, lambda, b, s) {
    # Computes the log-likelihood given the parameters provided
    #
    # Args:
    #   j     : refers to the cluster j
    #   eb    : the value of uninformed trading (buys)
    #   es    : the value of uninformed trading (sells)
    #   lambda: the set of informed trading rates
    #   b     : the vector of buy observations
    #   s     : the vector of sell observations
    #
    # Returns:
    #   a vector of daily log-likelihood values

    isb <- 1 - j %% 2  # is Buy?
    layer <- (j %/% 2) + 1
    lambda <- c(0, lambda)
    logprob <- suppressWarnings(
      dpois(b, eb + isb * lambda[layer], log = TRUE) +
        dpois(s, es + (1 - isb) * lambda[layer], log = TRUE)
    )

    logprob[is.na(logprob)] <- 0
    return(logprob)
  },

  posterior = function(j, p, eb, es, muj, b, s) {
  # Computes the numerators of posterior probabilities
  #
  # Args:
  #   j   : refers to the cluster j
  #   p   : the distribution of cluster probabilities
  #   eb  : the value of uninformed trading (buys)
  #   es  : the value of uninformed trading (sells)
  #   muj : the set of informed trading rates
  #   b   : the vector of buy observations
  #   s   : the vector of sell observations
  #
  # Returns:
  #   returns a vector of posterior probabilities


    isb <- 1 - j %% 2
    layer <- (j %/% 2) + 1
    muj <- c(0, muj)
    return(p[j] * dpois(b, eb + isb * muj[layer]) * dpois(s, es + (1 - isb) *
                                                            muj[layer]))
  },

  drop_layers = function(distrib, todrop, muj) {

    # cls is the number of clusters and layers is the number of layers
    # ------------------------------------------------------------------------
    layers <- length(muj)
    cls <- 2 * layers + 1

    # Layers can only be merged if the number of layers is at least 2
    # ------------------------------------------------------------------------
    if (layers == 1 | any(is.na(distrib))) return(NULL)

    dlayers <- distrib[2:length(distrib)]
    dglayers <- dlayers[c(TRUE, FALSE)]
    dblayers <- dlayers[!c(TRUE, FALSE)]

    # Since any layer contains at least one day, so if the frequency of days
    # in layer j is smaller than a threshold 'min_alpha', then the layer is
    # dropped and optimization continues with the remaining layers.
    # ------------------------------------------------------------------------
    dglayers <- dglayers[-c(todrop)]
    dblayers <- dblayers[-c(todrop)]
    distrib <- c(distrib[1], as.vector(rbind(dglayers, dblayers)))
    distrib <- distrib / sum(distrib)
    muj <- muj[-c(todrop)]
    dropped_layers <- length(todrop)
    cls <- cls - 2 * dropped_layers
    layers <- layers - dropped_layers

    return(list(distrib = distrib, layers = layers, muj = muj, cls = cls))
  },

  merge_layers =  function(distrib, eb, es, muj, layers) {

    # Layers can only be merged if the number of layers is at least 2
    # ------------------------------------------------------------------------
    if (layers == 1) return(NULL)

    # cls is the number of clusters and layers is the number of layers
    # ------------------------------------------------------------------------
    cls <- length(distrib)
    layers <- (cls - 1) / 2

    # Divide the distribution vectors to a distribution for bad information
    # clusters 'dblayers', and good-information clusters 'dglayers'.
    # alpha is the vector of probabilities for each layer, the sum, for each
    # layer, of good, and  bad clusters (alpha = dglayers + dblayers)
    # ------------------------------------------------------------------------
    dlayers <- distrib[2:length(distrib)]
    dglayers <- dlayers[c(TRUE, FALSE)]
    dblayers <- dlayers[!c(TRUE, FALSE)]

    # If difference of 2 consecutive mus is very small, then the clusters
    # should be combined. We merge cluster j and j+1 if mu(j) is within
    # 5% from mu(j+1): qpois(0.45, mu(j+1)) < qpois(0.5, mu(j))
    # ------------------------------------------------------------------------
    allmu <- c(max(eb, es), max(eb, es) + muj)

    lbmu <- vapply(allmu[2:(layers + 1)], function(x) qpois(0.05, x),
                   FUN.VALUE = double(1))
    ubmu <- allmu[1:layers]

    tomerge <- (ubmu > lbmu)

    # If values of mu's are close to one another, merge clusters
    # ------------------------------------------------------------------------
    if (sum(tomerge) > 0) {

      receiver <- which(tomerge > 0)[[1]]

      # If receiver == 1, then the first information layer's mu is to close
      # to max(eb, es), then the first layer is deleted, and the number of days
      # of the first layer is transferred to the non-information layer.
      # ----------------------------------------------------------------------
      if (receiver == 1) {

        xmuj <- muj[-1]
        dglayers <- dglayers[-1]
        dblayers <- dblayers[-1]
        layers_to_lose <- 1

      } else {

        # If receiver > 1, then the receiver is an information layer, and not
        # the no-information layer. The number of the information layer is then
        # rlayer = receiver - 1. Now, it is possible that many information
        # layers are to merge in one. It is therefore good to know, all
        # consecutive info. layers for which the variable tomerge takes the
        # value TRUE. This is equivalent to asking which first info layer takes
        # the value FALSE after 'receiver'.
        # ----------------------------------------------------------------------
        allfalse <- which(tomerge == FALSE)
        last_layer <- min(allfalse[allfalse > receiver])

        if (is.infinite(last_layer)) {
          last_layer <- length(tomerge)
        }

        layers_to_lose <- receiver:last_layer
        rlayer <- receiver - 1

        dupl <- c(rlayer, layers_to_lose)

        # :: Delete the duplicate values in muj
        # -----------------------------------------------------------------
        xmuj <- muj[-layers_to_lose]

        # :: Sum alpha, in the 1st layer and delete the values of the rest
        # -----------------------------------------------------------------
        dglayers[rlayer] <- sum(dglayers[dupl])
        dglayers <- dglayers[-layers_to_lose]
        dblayers[rlayer] <- sum(dblayers[dupl])
        dblayers <- dblayers[-layers_to_lose]

      }

      xdistrib <- c(distrib[1], as.vector(rbind(dglayers, dblayers)))
      xdistrib <- xdistrib / sum(xdistrib)

      dropped_layers <- length(layers_to_lose)
      xcls <- cls - 2 * dropped_layers
      xlayers <- layers - dropped_layers

      return(list(distrib = xdistrib, muj = xmuj, layers = xlayers, cls = xcls))

    }

    # If no layers are to merge, return NULL
    # ------------------------------------------------------------------------
    return(NULL)

  },

  detect_layers = function(code, data) {
    if (!(code %in% c("ECM", "E", "EG"))) code <- "EG"
    idx <- match(code, c("ECM", "E", "EG"))
    layers <-
      suppressWarnings(
        switch(
          idx,
          detectlayers_ecm(data),
          detectlayers_e(data),
          detectlayers_eg(data)[1]
        )
      )
    return(layers)
  },

  get_goodbadmpin = function(mpinj, params, pin = FALSE) {

    # Calculate good, and bad mpinJ, and mpin; and pass them to the
    # slot @mpinJ, and @mpin as 'mpin' dataclass. Check the function
    # print.mpin in utilities_functions.R to see its custom printing
    delta <-  if (is.numeric(params)) params[2] else params$delta

    lx <- length(mpinj)
    lnames <- if (lx > 1)
      paste("layer.", seq_len(lx), sep = "") else ("info")

    goodmpinj <- mpinj * (1 - delta)
    names(goodmpinj) <- lnames
    goodmpin <- sum(goodmpinj)

    badmpinj <- mpinj * delta
    names(badmpinj) <- lnames
    badmpin <- sum(badmpinj)

    if (pin) {
      response <- list(pinG = goodmpin, pinB = badmpin)
    } else {
      response <- list(mpinGj = goodmpinj, mpinBj = badmpinj,
                       mpinG = goodmpin, mpinB = badmpin)
    }

    return(response)
  }

)

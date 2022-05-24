## - | FILE  HEADER |
##
## Script name:
##    model_mpin.R
##
## Purpose of script:
##    Implements the algorithms for generation of initial parameter sets,
##    the standard estimation method of the Multilayer PIN model, as well
##    as the computation, and display of posterior probabilities for the
##    same model.
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2022-05-24
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
## initials_mpin():
##    Based on the algorithm in Ersan (2016) , generates initial
##    parameter sets for the maximum likelihood estimation of the
##    MPIN model.
##
## mpin_ml():
##    Estimates the multilayer probability of informed trading
##    MPIN using the standard Maximum likelihood method.
##
## get_posteriors():
##    Computes the posterior probability, for each day in the sample,
##    that the day belongs to a no-information day, good information
##    day and bad information day, respectively (Easley and Ohara (1992),
##    Easley et al. (1996), Ersan (2016)).
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


#' @title MPIN initial parameter sets of Ersan (2016)
#'
#' @description Based on the algorithm in
#' \insertCite{Ersan2016;textual}{PINstimation}, generates
#' initial parameter sets for the maximum likelihood estimation of the `MPIN`
#' model.
#'
#' @usage
#' initials_mpin(data, layers = NULL, detectlayers = "EG",
#'  xtraclusters = 4, verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param layers An integer referring to the assumed number of
#' information layers in the data. If the value of `layers` is `NULL`, then
#' the number of layers is automatically determined by one of the following
#' functions: `detectlayers_e()`, `detectlayers_eg()`, and `detectlayers_ecm()`.
#' The default value is `NULL`.
#'
#' @param detectlayers A character string referring to the layer
#' detection algorithm used to determine the number of layers in the data. It
#' takes one of three values: `"E"`, `"EG"`, and `"ECM"`. `"E"` refers to the
#' algorithm in \insertCite{Ersan2016;textual}{PINstimation}, `"EG"` refers to
#' the algorithm in \insertCite{Ersan2022a;textual}{PINstimation}; while
#' `"ECM"` refers to the algorithm in
#' \insertCite{Ghachem2022;textual}{PINstimation}. The default value is `"EG"`.
#' Comparative results between the layer detection
#' algorithms can be found in \insertCite{Ersan2022a;textual}{PINstimation}.
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{#(1 + layers + xtraclusters)} clusters, thereby resulting in
#' \code{#comb(layers + xtraclusters, layers)} initial parameter sets in
#' line with \insertCite{ErsanAlici2016;textual}{PINstimation}, and
#' \insertCite{Ersan2016;textual}{PINstimation}. The default value is `4`
#' as chosen in \insertCite{Ersan2016;textual}{PINstimation}.
#'
#' @param verbose a binary variable that determines whether information messages
#' about the initial parameter sets, including the number of the initial
#' parameter sets generated. No message is shown when \code{verbose} is set
#' to \code{FALSE}. The default value is \code{TRUE}.
#'
#' @details The argument 'data' should be a numeric dataframe, and contain
#' at least two variables. Only the first two variables will be considered:
#' The first variable is assumed to correspond to the total number of
#' buyer-initiated trades, while the second variable is assumed to
#' correspond to the total number of seller-initiated trades. Each row or
#' observation correspond to a trading day. `NA` values will be ignored.
#'
#' @return Returns a dataframe of initial parameter sets each consisting of
#' `3J + 2` variables \{\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es\}.
#' \eqn{\alpha}, \eqn{\delta}, and \eqn{\mu} are vectors of length `J` where
#' `J` is the number of layers in the `MPIN` model.
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations.   Each observation corresponds to a day and contains the
#' # total number of buyer-initiated   transactions ('Buys') and
#' # seller-initiated transactions ('S') on that day. To know   more, type
#' # ?dailytrades
#'
#'  xdata <- dailytrades
#'
#' # Obtain a dataframe of initial parameter sets for estimation of the MPIN
#' # model using the algorithm of Ersan (2016) with 3 extra clusters.
#' # By default, the number of layers in the data is detected using the
#' # algorithm of Ersan and Ghachem (2022a).
#'
#'  init.sets <- initials_mpin(xdata, xtraclusters = 3)
#'
#' # Show the initial parameter sets
#'
#'  show(round(init.sets, 2))
#'
#' # Use these initial parameter sets to estimate the probability of informed
#' # trading, the number of information layers will be detected from the
#' # initial parameter sets.
#'
#'  estimate <- mpin_ml(xdata, initialsets = init.sets, verbose = FALSE)
#'
#' # Display the estimated MPIN value
#'  show(estimate@mpin)
#'
#' # Display the estimated parameters as a numeric vector.
#'  show(unlist(estimate@parameters))
#'
#' # Store the posterior probabilities in a dataframe variable, and show its
#' # first 6 rows.
#'
#'  modelposteriors <- get_posteriors(estimate)
#'  show(round(head(modelposteriors), 3))
#'
#' @export
initials_mpin <- function(data, layers = NULL, detectlayers = "EG",
                             xtraclusters = 4, verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$arguments()$mpininitfn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, layers, detectlayers, xtraclusters, verbose)
  names(largs) <- names(formals())
  largs$fn <- "mpin"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$arguments()$mpininitfn)


  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)
  initials <- NULL

  # Get the number of layers if not provided
  # --------------------------------------------------------------------------
  if (is.null(layers))
    layers <- .xmpin$detect_layers(detectlayers, data)

  # Cluster aoi observations using HAC algorithm
  clusters <- hclust(dist(data$aoi), method = "complete")
  data$cluster <- cutree(clusters, layers + xtraclusters + 1)

  # Find average aoi per cluster and rank clusters by mean(aoi)
  means <- aggregate(. ~ cluster, data, mean)
  means <- means[order(means$aoi), ]
  clrank <- means$cluster


  # Find all different ways to partition cluster among no-information
  # cluster and information layers and store it in 'partitions'
  # --------------------------------------------------------------------------
  bind_to_matrix <- function(x, mat) {
    return(cbind(rep(x, nrow(mat)), mat))
  }
  positions <- function(sizesofar, layer, extra = xtraclusters) {
    maxsize <- extra + layer - sizesofar
    if (layer >= layers) {
      return(matrix((sizesofar + 1):(sizesofar + maxsize), ncol = 1))
    }
    if (layer < layers) {
      sequence <- sizesofar + (1:maxsize)
      output <- Reduce(
        rbind, lapply(sequence, function(x)
          (bind_to_matrix(x, positions(x, layer + 1)))))
      return(output)
    }
  }
  partitions <- positions(0, 1)
  npartitions <- nrow(partitions)
  xpartitions <- ux$tolist(partitions)

  # --------------------------------------------------------------------------

  # Reorder the cluster so they follow the order of clrank
  # ----------------------------------------------------------------------------
  data$cluster <- match(data$cluster, clrank)

  # Algorithm overview
  # ----------------------------------------------------------------------------
  # Create noEventRange which are the indices of clusters with the lower
  # absolute order imbalance. noEventRange will start with 1 element to
  # reach at the end init.points elements. It consists of the kth first
  # elements of Clrank where k goes from 1 to init.points.
  # for the clusters in the noEventRange, a column called set will take
  # the value 'noEvent'. For those outside notEventRange, the column set
  # takes the value 'good' if the order imbalance (oi) is non-negative
  # and takes 'bad' otherwise.
  # Once the variable 'set' is constructed, we can just aggregate the
  # data by the variable set and take the average within each group.
  # Once we have obtained the means, we can use GAN (2015) algorithm to
  # calculate an initial parameter set.
  # Once we have a new initial parameter set, we append it to the list of
  # initial points called initials.
  # ----------------------------------------------------------------------------
  .generate_one_iniatialset <- function(combination) {

    combination <- xpartitions[[combination]]
    cutoffs <- unname(unlist(c(0, combination, layers + xtraclusters + 1)))
    data$layer <- with(data, findInterval(cluster, cutoffs, left.open = TRUE) -
                         1)

    # Get the information about the sign of the OI, and delete the variables
    # oi, aoi, and cluster, not needed anymore.
    data$info <- sign(data$oi) * sign(data$layer)
    data$cluster <- data$oi <- data$aoi <- NULL
    data$info <- apply(
      data, 1, function(x) c("bad", "none", "good")[x[4] + 2])

    # Collect information on the information days
    infodata <- data[data$layer != 0, ]
    means <- aggregate(. ~ layer + info, infodata, mean, drop = FALSE)
    means$days <- aggregate(b ~ layer + info, infodata, length, drop = FALSE)$b
    means[is.na(means)] <- 0

    headers <- c("layer", "info", "b", "s", "days")
    xcol <- length(headers)

    # Create a shadow dataframe to make sure that all information layers
    # (+ and -) are represented.
    means_bg <- as.data.frame(matrix(0, ncol = xcol, nrow = 2 * layers))
    colnames(means_bg) <- headers
    means_bg$layer <- rep(1:layers, 2)
    means_bg$info <- c(rep("good", layers), rep("bad", layers))

    means <- merge(
      means, means_bg, all.y = TRUE, by = c("layer", "info"), )[, 1:xcol]

    colnames(means) <- headers

    # Add the none statistics
    noinfodata <- data[data$layer == 0, ]

    means_ne <- aggregate(. ~ info, noinfodata, mean, drop = TRUE)
    means_ne$days <- nrow(noinfodata)
    means_ne <- means_ne[, headers]
    means <- rbind(means, means_ne)
    means[is.na(means)] <- 0


    # Compute estimates of the parameters as in Ersan (2016)
    # --------------------------------------------------------------------------

    .get_distribution <- function(df, var) {

      types <- c("bad", "none", "good")
      .get_meanvalue <- function(df, var, type) {
        val <- df[df$info == type, var]
        denom <- ifelse(var == "days", nrow(data), 1)
        val <- val / denom
      }

      result <- lapply(types, function(x) .get_meanvalue(df, var, x))
      names(result) <- types
      return(result)
    }

    xd <- .get_distribution(means, "days")
    xb <- .get_distribution(means, "b")
    xs <- .get_distribution(means, "s")

    a <- xd$bad + xd$good
    d <- xd$bad / a

    .w_mean <- function(var1, var2, info) {
      v1 <- unlist(var1[info])
      v2 <- unlist(var2[info])
      v2 <- v2 / sum(v2)
      return(weighted.mean(x = v1, w = v2))
    }

    eb <- .w_mean(xb, xd, info = c("bad", "none"))
    es <- .w_mean(xs, xd, info = c("good", "none"))
    ediff <- eb - es

    wmu <- xb
    wmu$bad <- xs$bad - xb$bad + ediff
    wmu$good <- xb$good - xs$good - ediff
    mu <- (xd$bad * wmu$bad + xd$good * wmu$good) / (xd$bad + xd$good)
    mu[mu < 0] <- 0

    # Order the vectors alpha, delta by increasing mu
    ordmu <- order(mu)
    a <- a[ordmu]
    d <- d[ordmu]
    mu <- mu[ordmu]

    return(c(a, d, mu, eb, es))

  }


  xinitials <- vapply(
    1:npartitions,
    .generate_one_iniatialset, numeric(3 * layers + 2)
    )
  xinitials <- as.data.frame(t(xinitials))
  colnames(xinitials) <- .xmpin$varnames(4, layers)
  rownames(xinitials) <- NULL

  pin_err <- uierrors$pin()
  ux$show(c = verbose, m = pin_err$displaysets(
    "initials_mpin(...)", nrow(initials)), warning = TRUE)

  return(invisible(xinitials))
}


#' @title MPIN model estimation via standard ML methods
#'
#' @description Estimates the multilayer probability of informed trading
#' (`MPIN`) using the standard Maximum Likelihood method.
#'
#' @usage mpin_ml(data, layers = NULL, xtraclusters = 4, initialsets = NULL,
#' detectlayers = "EG", ..., verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param layers An integer referring to the assumed number of
#' information layers in the data. If the argument \code{layers} is given,
#' then the maximum likelihood estimation will use the number of layers
#' provided. If \code{layers} is omitted,
#' the function \code{mpin_ml()} will find the optimal number of layers using
#' the algorithm developed in \insertCite{Ersan2022a;textual}{PINstimation}
#' (as default).
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{(1 + layers + xtraclusters)} clusters, thereby resulting in
#' \code{#comb(layers + xtraclusters, layers)} initial parameter sets in line
#' with \insertCite{ErsanAlici2016;textual}{PINstimation}, and
#' \insertCite{Ersan2016;textual}{PINstimation}. The default value is `4` as
#'  chosen in \insertCite{Ersan2016;textual}{PINstimation}.
#'
#' @param initialsets A dataframe containing initial parameter
#' sets for the estimation of the `MPIN` model. The default value is `NULL`.
#' If `initialsets` is `NULL`, the initial parameter sets are determined by the
#' function `initials_mpin()`.
#'
#' @param detectlayers A character string referring to the layer
#' detection algorithm used to determine the number of  layer in the data. It
#' takes one of three values: `"E"`, `"EG"`, and `"ECM"`. `"E"` refers to the
#' algorithm in \insertCite{Ersan2016;textual}{PINstimation}, `"EG"` refers to
#' the algorithm in \insertCite{Ersan2022a;textual}{PINstimation};
#' while `"ECM"` refers to the algorithm in
#' \insertCite{Ghachem2022;textual}{PINstimation}.
#' The default value is `"EG"`. Comparative results between the layer detection
#' algorithms can be found in \insertCite{Ersan2022a;textual}{PINstimation}.
#'
#' @param ... Additional arguments passed on to the function `mpin_ml`. The
#' recognized argument is `is_parallel`. `is_parallel` is a logical variable
#' that specifies whether the computation is performed using parallel
#' processing. The default value is \code{FALSE}.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the MPIN model is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @details The argument 'data' should be a numeric dataframe, and contain
#' at least two variables. Only the first two variables will be considered:
#' The first variable is assumed to correspond to the total number of
#' buyer-initiated trades, while the second variable is assumed to
#' correspond to the total number of seller-initiated trades. Each row or
#' observation correspond to a trading day. `NA` values will be ignored. \cr
#'
#' @return Returns an object of class \code{estimate.mpin}
#'
#' @references
#'
#'  \insertAllCited
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations. Each observation corresponds to a day and contains the total
#' # number of buyer-initiated   transactions ('Buys') and seller-initiated
#' # transactions ('S') on that day. To know   more, type ?dailytrades.
#'
#' xdata <- dailytrades
#'
#' # ------------------------------------------------------------------------ #
#' # Estimate MPIN model using the Ersan(2016) algorithm.                     #
#' # ------------------------------------------------------------------------ #
#'
#' # Let the function mpin_ml() detect the optimal number of layers.
#'
#' optimalEstimate <- mpin_ml(xdata, verbose = FALSE)
#'
#' # Show the estimation output
#'
#' show(optimalEstimate)
#'
#' # Estimate the MPIN model using the function mpin_ml(), without
#' # specifying the number of layers. The number of layers is
#' # detected using Ersan and Ghachem (2022a).
#' # -------------------------------------------------------------
#'
#' estimate <- mpin_ml(xdata, verbose = FALSE)
#'
#' # Show the estimation output
#'
#' show(estimate)
#'
#' # Display the likelihood-maximizing parameters
#'
#' show(estimate@parameters)
#'
#' # Display the global multilayer probability of informed trading
#'
#' show(estimate@mpin)
#'
#' # Display the multilayer probabilities of informed trading per layer
#'
#' show(estimate@mpinJ)
#'
#' # Display the first five initial parameters sets used in the maximum
#' # likelihood estimation
#'
#' show(round(head(estimate@initialsets, 5), 4))
#'
#' @importFrom skellam qskellam
#' @export
mpin_ml <- function(data, layers = NULL, xtraclusters = 4, initialsets = NULL,
                    detectlayers = "EG", ..., verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  allvars <- allvars[-6]
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$arguments()$mpininitfn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(
    data, layers, xtraclusters, initialsets, detectlayers, 0, verbose
    )
  names(largs) <- names(formals())
  largs[["..."]] <- NULL
  is_parallel <- .default$mpin_parallel

  vargs <- list(...)
  # check for unknown keys in the argument "..."
  unknown <- setdiff(names(vargs), c("is_parallel"))
  ux$stopnow(length(unknown) > 0, s = uierrors$mpin()$fn,
             m = uierrors$arguments()$unknown(u = unknown))

  # Collect the arguments in the dot-dot arguments
  if (length(vargs) > 0 && "is_parallel" %in% names(vargs))
    is_parallel <- vargs$is_parallel
  largs$is_parallel <- is_parallel
  largs$fn <- "mpin"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)


  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)

  # If both layers and xtraclusters are given, check that they are compatible
  if (!is.null(layers)) {
    rst <- .xcheck$xclusters(n = nrow(data), lay = layers,
                             xtra = xtraclusters)
    ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  }

  if (is.null(initialsets) && is.null(layers)) {
    dlayers <- .xmpin$detect_layers(toupper(detectlayers), data)
    rst <- .xcheck$xclusters(
      n = nrow(data), lay = dlayers, xtra = xtraclusters)
    ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  }

  # If the user specifies a number (layers), use it. If the argument layers
  # remains NULL, then use the detectlayers algorithms
  # ------------------------------------------------------------------------
  if (!is.null(initialsets)) {

    nrows <- nrow(initialsets)
    xlayers <- (length(initialsets[1, ]) - 2) / 3

    mpin_ms <- uix$mpin(
      nrows = nrows, initlayers = xlayers, layers = layers)

    ux$show(verbose, m = mpin_ms$start)

    ux$show(verbose, m = mpin_ms$detectsets)

    ux$show(verbose && !is.null(layers) && (xlayers != layers),
            m = mpin_ms$differentlayers, warning = TRUE)

    ux$show(verbose, m = mpin_ms$loadinitials)

    layers <- xlayers

    detection <- "INITIALSETS"

  } else {

    mpin_ms <- uix$mpin()

    ux$show(verbose, m = mpin_ms$start)

    if (is.null(layers)) {

      layers <- dlayers

      mpin_ms <- uix$mpin(layers = layers)

      ux$show(verbose, m = paste(
        mpin_ms$detectdata, mpin_ms$algorithm[detectlayers], sep = ""))

      ux$show(verbose, m = mpin_ms$numlayers)

      detection <- detectlayers

    } else {

      mpin_ms <- uix$mpin(layers = layers)

      ux$show(verbose, m = mpin_ms$selectedlayers)

      detection <- "USER"
    }


    ux$show(verbose, m = mpin_ms$computinginitials)

    initialsets <- initials_mpin(
      data, layers, xtraclusters = xtraclusters, verbose = FALSE)

    mpin_ms <- uix$mpin(nrows = nrow(initialsets))

  }

  # Transform initialsets from a dataframe to a list
  initialsets <- ux$tolist(initialsets)

  # Optimize the log-likelihood factorization of Ersan (2016)
  # ------------------------------------------------------------------------
  results <- .estimate_mpin(data, initialsets, layers,
                            detection, is_parallel, verbose)
  results@parallel <- is_parallel

  ux$show(verbose, m = mpin_ms$complete)

  return(results)
}



#' @title Posterior probabilities for PIN and MPIN estimates
#'
#' @description Computes, for each day in the sample, the posterior probability
#' that the day is a no-information day, good-information day and bad-information
#' day, respectively (\insertCite{Easley1992;textual}{PINstimation},
#' \insertCite{Easley1996;textual}{PINstimation},
#' \insertCite{Ersan2016;textual}{PINstimation}).
#'
#' @usage get_posteriors(object)
#'
#' @param object (S4 object) an object of type `estimate.pin`,
#' `estimate.mpin`, or `estimate.mpin.ecm`.
#'
#' @return
#' If the argument `object` is of type `estimate.pin`, returns a dataframe of
#' three variables `post.N`, `post.G` and `post.B` containing in each row the
#' posterior probability that a given day is a no-information day (`N`),
#' good-information day (`G`), or bad-information day (`B`) respectively.
#'
#' If the argument `object` is of type `estimate.mpin` or `estimate.mpin.ecm`,
#' with `J` layers, returns a dataframe of `2*J+1` variables `Post.N`, and
#' `Post.G[j]` and `Post.B[j]` for each layer `j` containing in each row the
#' posterior probability that a given day is a no-information day,
#' good-information day in layer `j` or bad-information day in layer `j`,
#' for each layer `j` respectively.
#'
#' If the argument `object` is of any other type, an error is returned.
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations. Each observation corresponds to a day and contains the total
#' # number of buyer-initiated transactions ('B') and seller-initiated
#' # transactions ('S') on that day. To know more, type ?dailytrades
#'
#' xdata <- dailytrades
#'
#' # ------------------------------------------------------------------------ #
#' # Posterior probabilities for PIN estimates                                #
#' # ------------------------------------------------------------------------ #
#'
#' # Estimate PIN using the Ersan and Alici (2016) algorithm and the
#' # factorization Lin and Ke(2011).
#'
#' estimate <- pin_ea(xdata, "LK", verbose = FALSE)
#'
#' # Display the estimated PIN value
#'
#' estimate@pin
#'
#' # Store the posterior probabilities in a dataframe variable and display its
#' # first 6 rows.
#'
#' modelposteriors <- get_posteriors(estimate)
#' show(round(head(modelposteriors), 3))
#'
#' # ------------------------------------------------------------------------ #
#' # Posterior probabilities for MPIN estimates                               #
#' # ------------------------------------------------------------------------ #
#'
#' # Estimate MPIN via MLE using the default layer detection algorithm of
#' # Ersan and Ghachem (2022a)
#'
#' mpin.estimate <- mpin_ml(xdata, verbose = FALSE)
#'
#' # Display the estimated Multilayer PIN value
#'
#' show(mpin.estimate@mpin)
#'
#' # Store the posterior probabilities in a dataframe variable and display its
#' # first six rows. The posterior probabilities are contained in a dataframe
#' # with 7 variables: one for no-information days, and two variables for each
#' # layer, one for good-information days and one for bad-information days.
#'
#' modelposteriors <- get_posteriors(mpin.estimate)
#' show(round(head(modelposteriors), 3))
#'
#' @export
get_posteriors <- function(object) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, uierrors$mpin()$fn)

  if (is(object, "estimate.adjpin"))
    return(message("\rError: The posteriors can't be obtained for",
                   " 'estimate.adjpin' objects!"))

  if (is(object, "estimate.pin") | is(object, "estimate.mpin")
      | is(object, "estimate.mpin.ecm"))
    return(attr(object, "posteriors"))
  return(message("\rError: Wrong object type!"))
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


.mpin_posteriors <- function(data, params) {
#  Computes the posterior probability for each day in the sample that the day
#  belongs to a no-information day, good information day and bad information
#  day, respectively
#
# Args:
#   data    : a dataframe containing two variables 'b' and 's'
#   params  : the set of MPIN estimates used to evaluate the posterior
#             probabilities
#
# Returns:
#   a dataframe of three variables `PostN`, `PostG` and `PostB` containing in
#  each row the  posterior probability that a given day is a no-information day,
#  good-information day or bad-information day respectively.

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  dx <- ux$prepare(data)
  a <- d <- mu <- eb <- es <- NULL

  # Collect the model parameters
  # --------------------------------------------------------------------------
  layers <- (length(params) - 2) / 3
  variables <- c("a", "d", "mu", "eb", "es")
  values <- unname(split(params, rep(1:5, c(layers, layers, layers, 1, 1))))
  for (i in seq_len(length(values)))
    assign(variables[i], unname(unlist(values[[i]])))

  # Compute the two parts of the factorization
  # --------------------------------------------------------------------------
  # Construct the variables e1, e2, e3, emax = max(ei) as in Ersan (2016).

  e1 <- kronecker(dx$b, t(mu), function(x, y) log(1 + y / eb) * x - y)
  e2 <- kronecker(dx$s, t(mu), function(x, y) log(1 + y / es) * x - y)
  e3 <- 0
  de <- data.frame(e1, e2, e3)
  emax <- do.call(pmax, de)

  dx$p1 <- dx$b * log(eb) + dx$s * log(es)  + emax
  dp2 <- cbind(t(t(exp(e1 - emax)) * (a * (1 - d))),
                   t(t(exp(e2 - emax)) * (a * d)),
                   (1 - sum(a)) * exp(e3 - emax))

  dx$p2 <- log(rowSums(dp2))
  dx$denom <- dx$p1 + dx$p2

  # Compute and return the conditional probabilities
  # --------------------------------------------------------------------------
  # - pn: numerator of P(n|b, s) - pg: numerator of P(g|b, s)
  # - pb: numerator of P(b|b, s) - denom : denominator of all probabilities

  dx$pn <- log((1 - sum(a))) + dx$b * log(eb) + dx$s * log(es)
  dx$pg <- log(t(t(exp(e1 - emax)) * (a * (1 - d)))) +
    dx$b * log(eb) + dx$s * log(es)
  dx$pb <- log(t(t(exp(e2 - emax)) * (a * d))) + dx$b * log(eb)
                  + dx$s * log(es)

  posteriors <- data.frame(with(
    data, cbind(exp(dx$pn - dx$denom), exp(dx$pg - dx$denom + emax),
                exp(dx$pb - dx$denom + emax))
  ))

  q <- seq(1:layers)
  qnames <- c("post.N", sprintf("post.G[%s]", q), sprintf("Post.B[%s]", q))
  colnames(posteriors) <- qnames

  return(posteriors)
}

.estimate_mpin <- function(data, initialsets, layers, detection,
                           is_parallel, verbose) {
# find MPIN-likelihood maximizing parameters starting from initial sets
# provided by the user.
#
# Args:
#   data        : a dataframe containing two variables 'b', and 's'
#   initialsets : a list of parameter vectors for the MPIN model
#   detection   : a character specifying how the number of layers is obtained
#               : It takes one of five values "INITIALSETS", "USER", "EG", "E",
#                 and "ECM"
#
# Returns:
#   a 'estimate.mpin' object containing the result of the optimization


  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------

  data <- ux$prepare(data)
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)
  a <- d <- mu <- eb <- es <- time_on <- time_off <- NULL
  mpin_ms <- uix$mpin()

  estimates <- NULL
  optimal <- list(likelihood = -Inf)

  # Optimize the likelihood for each initial set and pick the one
  # corresponding to the highest likelihood
  # --------------------------------------------------------------------------
  convergent <- 0
  low <- c(rep(0, (3 * layers)), 0, 0)
  up <- c(rep(1, (2 * layers)), rep(Inf, layers), Inf, Inf)
  nrows <- length(initialsets)

  time_on <- Sys.time()


  ux$show(verbose, m = mpin_ms$mlemethod)

  if (verbose) {
    pb_mpin <- ux$progressbar(0, nrows)
    cat(mpin_ms$progressbar)
  }

  .get_mlrun <- function(current) {

    # Prepare the current parameters, and the details dataframe
    # -----------------------------------------------------------------------
    temp_run <- unlist(initialsets[[current]])
    full <- length(initialsets)
    thisrun <- c(temp_run, rep(NA, 3 * layers + 4))

    # Code for displaying the progress bar
    # --------------------------------------------------------------------------
    if (verbose)
      pb_mpin <- ux$progressbar(minvalue = current - 1, maxvalue = full)

    # Estimate the ECM model by calling the function neldermead().
    # If the output of neldermead() is valid, calculate its likelihood using
    # -factorizations$mpin().
    # --------------------------------------------------------------------------
    tryCatch({
      estimates <- suppressWarnings(
        neldermead(
          initialsets[[current]], factorizations$mpin(data),
          lower = low, upper = up)
      )})


    if (!is.null(estimates)) {

      # The vector thisrun contains all optimal parameters, alongside the
      # list 'estimates' containing the results of the ML estimation.
      thisrun <- c(list(c(temp_run, estimates$par, -estimates$value,
                   .xmpin$compute_pin(estimates$par)), I(list(estimates))))
    }

    # Update the progress bar in the parent environment
    pe <- parent.env(environment())
    if (verbose) setTxtProgressBar(pe$pb_mpin, current)

    return(thisrun)
  }

  # Loop over the initial sets to find the optimal estimates
  # ----------------------------------------------------------------------------
  xs <- seq_len(length(initialsets))

  ux <- ux
  uix <- uix
  .xmpin <- .xmpin

  mpin_ms <- uix$mpin(layers = layers)
  if (verbose) {
    pb_mpin <- ux$progressbar(minvalue = 0, maxvalue = length(initialsets))
    cat(mpin_ms$progressbar)
  }

  if (is_parallel & length(initialsets) >= .default$parallel_cap()) {

    future::plan(multisession, gc = TRUE, workers = .default$parallel_cores())

    runs <- furrr::future_map(xs, function(x) .get_mlrun(x))

    future::plan(sequential)

  } else {

    runs <- lapply(xs, .get_mlrun)

  }

  time_off <- Sys.time()

  if (length(runs) > 1) {

    # Receive the list of all runs, each list element contains two list elements
    # the first one is all parameters relative to the run, and the second is the
    # list object, outcome of the ML neldermead estimation.
    estimates <- lapply(runs, "[[", 2)

    # Get and format the dataframe 'runs' which will be contained in the slot
    # 'details' of the optimal estimate
    xruns <- lapply(runs, "[[", 1)
    runs <- data.frame(do.call(rbind, xruns))

    colnames(runs) <- .xmpin$varnames(5, layers)
    rownames(runs) <- paste("set.", seq_len(nrow(runs)), sep = "")

    # Get the list of all likelihood. The number 'convergent' is the number of
    # all runs, for which the likelihood value is finite. If convergent is
    # different from zero, then there is an optimal estimate corresponding to
    # the one with highest likelihood value.
    lkdruns <- unlist(runs$likelihood)
    convergent <- length(lkdruns[is.finite(lkdruns)])
    if (convergent > 0) {
      optimizer <- which.max(lkdruns)[1]
      optimal <- estimates[[optimizer]][[1]]
    }

  }

  time_off <- Sys.time()


  # Optimization ended! - Format and return the optimal results
  # --------------------------------------------------------------------------

  initialpoints <- ux$todframe(initialsets)
  names(initialpoints) <- .xmpin$varnames(4, layers)

  if (is.finite(optimal$value)) {

    variables <- .xmpin$varnames()

    estimation <- split(
      optimal$par, rep(1:5, c(layers, layers, layers, 1, 1)))

    for (i in 1:5) assign(variables[i], estimation[[i]])

    mpin_j <- setNames(a * mu / (sum(a * mu) + eb + es),
                       paste("layer.", 1:layers, "", sep = ""))
    mpin <- sum(a * mu) / (sum(a * mu) + eb + es)

    aggregates <- c(sum(a), sum(a * d) / sum(a),
                    sum(a * mu) / sum(a), eb, es)

    names(aggregates) <- .xmpin$varnames(2)

    parameters <- c(
      list(setNames(a,  paste("layer.", 1:layers, "", sep = ""))),
      list(setNames(d,  paste("layer.", 1:layers, "", sep = ""))),
      list(setNames(mu,  paste("layer.", 1:layers, "", sep = ""))),
      list(eb), list(es))

    names(parameters) <- .xmpin$varnames(2)

    xlist <- .xmpin$get_goodbadmpin(mpin_j, parameters)

    mpin_optimal <- new("estimate.mpin",
                   success = TRUE,
                   convergent.sets = convergent,
                   method = "ML",
                   layers = layers,
                   detection = detection,
                   parameters = parameters,
                   aggregates = aggregates,
                   likelihood = -optimal$value,
                   mpin.goodbad = xlist,
                   mpinJ = mpin_j,
                   mpin = mpin,
                   initialsets = initialpoints,
                   dataset = data,
                   details = runs,
                   runningtime = ux$timediff(time_on, time_off)
    )

    attr(mpin_optimal, "posteriors") <- .mpin_posteriors(
      data, c(a, d, mu, eb, es))


  } else {

    errmsg <- uierrors$mpin()$failed

    mpin_optimal <-
      new(
        "estimate.mpin",
        success = FALSE,
        method = "ML",
        detection = detection,
        layers = layers,
        errorMessage = errmsg,
        initialsets = initialpoints,
        dataset = data,
        runningtime = ux$timediff(time_on, time_off),
        likelihood = NaN,
        mpinJ = NaN,
        mpin = NaN
      )
  }

  return(mpin_optimal)

}

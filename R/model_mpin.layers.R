## - | FILE  HEADER |
##
## Script name:
##    model_mpin.layers.R
##
## Purpose of script:
##    Implement three algorithms for detection of the number of information
##    layers present in trade-data namely, those in Ersan (2016),
##    Ersan and Ghachem (2022a), and Ghachem and Ersan (2022b).
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
##
##
## Public functions:
## ++++++++++++++++++
##
## detectlayers_eg():
##    Detects the number of information layers present in trade
##    data using the algorithm in Ersan and Ghachem (2022a).
##
## detectlayers_e():
##    Detects the number of information layers present in trade
##    data using the algorithm in Ersan(2016).
##
## detectlayers_ecm():
##    Detects the number of information layers present in trade
##    data using the algorithm in Ghachem and Ersan (2022b).
##
## ++++++++++++++++++
##
## Notes:
##
## Package PINstimation
## website: www.pinstimation.com
## Authors: Montasser Ghachem and Oguz Ersan


##       +++++++++++++++++++++++++
## ++++++| |  PUBLIC FUNCTIONS | |
##       +++++++++++++++++++++++++


#' @title Layer detection in trade-data
#'
#' @description Detects the number of information layers present in trade-data
#' using the algorithms in \insertCite{Ersan2016;textual}{PINstimation},
#' \insertCite{Ersan2022a;textual}{PINstimation},
#' and \insertCite{Ghachem2022;textual}{PINstimation}.
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @return Returns an integer corresponding to the number of layers detected in
#' the data.
#'
#' @details The argument 'data' should be a numeric dataframe, and contain
#' at least two variables. Only the first two variables will be considered:
#' The first variable is assumed to correspond to the total number of
#' buyer-initiated trades, while the second variable is assumed to
#' correspond to the total number of seller-initiated trades. Each row or
#' observation correspond to a trading day. `NA` values will be ignored.
#'
#' The argument `hyperparams`  contains the hyperparameters of the `ECM`
#' algorithm. It is either empty or contains one or more of the following
#' elements:
#' \itemize{
#' \item `maxeval`: (`integer`) It stands for maximum number of iterations
#' of the `ECM`  for each initial parameter set. When missing, `maxeval`
#' takes the default value of `100`.
#'
#' \item `tolerance` (`numeric`) The `ECM` algorithm is stopped when the
#' (relative) change of log-likelihood is smaller than tolerance. When
#' missing, `tolerance` takes the default value of `0.001`.
#'
#' \item `maxinit`: (`integer`) It is the maximum number of initial
#' parameter sets used for the `ECM` estimation per layer. When missing,
#' `maxinit` takes the default value of `20`.
#'
#' \item `maxlayers` (`integer`) It is the upper limit of number of layers
#' used in the ECM algorithm. To find the optimal number of layers, the ECM
#' algorithm will estimate a model for each value of the number of layers
#' between `1` and `maxlayers`, and then picks the model that has the lowest
#' Bayes information criterion (BIC). When missing, `maxlayers` takes the
#' default value of `8`.
#'
#' }
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations. Each observation corresponds to a day and contains the
#' # total number of buyer-initiated trades ('B') and seller-initiated
#' # trades ('S') on that day. To know more, type ?dailytrades
#'
#' xdata <- dailytrades
#'
#' # Detect the number of layers present in the dataset 'dailytrades' using the
#' # different algorithms and display the results
#'
#' e.layers <- detectlayers_e(xdata)
#' eg.layers <- detectlayers_eg(xdata)
#' \donttest{em.layers <- detectlayers_ecm(xdata)
#'
#' show(c(e = e.layers, eg = eg.layers, em = em.layers))}
#'
#' @name detecting-layers
#' @aliases detectlayers_ecm detectlayers_e detectlayers_eg
#'
NULL


#' @rdname detecting-layers
#' @param  correction A binary variable that determines whether the
#' data will be adjusted prior to implementing the algorithm of
#' \insertCite{Ersan2016;textual}{PINstimation}. The default value is `TRUE`.
#' @export
detectlayers_e <- function(data, confidence = 0.995, correction = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$detection()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, confidence, correction)
  names(largs) <- names(formals())
  largs$fn <- "detect"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$detection()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)

  return(.findlayers(data, confidence = confidence, correct = correction))
}


#' @rdname detecting-layers
#' @param  confidence A number from `(0.5,1)`, corresponding to the
#' range of the confidence interval used to determine whether a given cluster is
#' compact, and therefore can be considered an information layer.
#' If all values of absolute order imbalances (AOI) within a given cluster are
#' within the confidence interval of a Skellam distribution with level equal to
#' `'confidence'`, and centered on the mean of AOI, then the cluster is considered
#' compact, and, therefore, an information layer. If some observations
#' are outside the confidence interval, then the data is clustered further. The
#' default value is `0.995`. `[i]` This is an argument of the functions
#' `detectlayers_e()`, and `detectlayers_eg()`.
#'
#' @importFrom skellam qskellam
#'
#' @export
detectlayers_eg <- function(data, confidence = 0.995) {

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, confidence)
  names(largs) <- names(formals())
  largs$fn <- "detect"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$detection()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)


  # Find the absolute imbalance per day as the absolute difference between
  # buys and sells and save it in the column 'aoi'
  # ---------------------------------------------------------------------
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)
  data$tot <- data$b + data$s
  data <- data[order(data$oi), ]
  rownames(data) <- NULL

  # ---------------------------------------------------------------
  # Find the number of information layers:
  #----------------------------------------------------------------

  # check_bounds check whether the distribution of data is within the skellam
  # bounds. noinfo=TRUE means it concerns no-information cluster, and when it
  # FALSE, it checks for information clusters.
  # For each number of clusters, multiply the response of the function
  # check_bounds() so we only get TRUE if all tests are done successfully.

  check_bounds <- function(df, noinfo = TRUE, conf = confidence) {

    if (nrow(df) == 1) return(TRUE)

    if (noinfo) {

      maxv <- max(df$oi)
      minv <- min(df$oi)

      mean_b <- round(mean(df$b))
      mean_s <- round(mean(df$s))

      quantiles_b <- quantile(df$b, probs = seq(0, 1, 0.25), na.rm = TRUE)[2:4]
      quantiles_s <- quantile(df$s, probs = seq(0, 1, 0.25), na.rm = TRUE)[2:4]

      if (nrow(df) > 4) {

        comb <- expand.grid(c(mean_b, quantiles_b), c(mean_s, quantiles_s))
        comb <- setNames(as.data.frame(comb), c("u1", "u2"))

        comb$lbound <- apply(
          comb, 1, function(x) qskellam(((1 - conf) / 2), x[1], x[2]))
        comb$ubound <- apply(
          comb, 1, function(x) qskellam((1 - ((1 - conf) / 2)), x[1], x[2]))

        comb$pass <- (maxv <= comb$ubound & minv >= comb$lbound)
        response <- ifelse(sum(comb$pass) != 0, TRUE, FALSE)

      } else {

        u1 <- round(mean(df$b))
        u2 <- round(mean(df$s))

        lbound <- suppressWarnings(qskellam(((1 - conf) / 2), u1, u2))
        ubound <- suppressWarnings(qskellam(1 - ((1 - conf) / 2), u1, u2))

        response <- (maxv <= ubound & minv >= lbound)
      }

    } else {

      df$maxbs <- pmax(df$b, df$s)
      df$minbs <- pmin(df$b, df$s)

      u1 <- round(mean(df$maxbs))
      u2 <- round(mean(df$minbs))

      maxv <- max(df$aoi)
      minv <- min(df$aoi)

      lbound <- suppressWarnings(qskellam(((1 - conf) / 2), u1, u2))
      ubound <- suppressWarnings(qskellam(1 - ((1 - conf) / 2), u1, u2))

      response <- (maxv <= ubound & minv >= lbound)
    }

    return(response)
  }

  # ************************************************************************** #
  # *                STEP 1 | Find the no-information cluster                * #
  # ************************************************************************** #

  rownames(data) <- NULL
  configuration <- NULL
  noinfo_temp <- 0
  alldays <- NULL
  maxclusters <- floor(nrow(data) / 2)
  oiclusters <- max(floor(nrow(data) / 2) - 2, 2)
  is_skellam <- FALSE

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # + STEP 1.1 | Find the clustering with largest no-information cluster       +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  while (oiclusters < maxclusters | length(configuration) == 0) {

    # Cluster oi observations in 'oiclusters' clusters
    clusters <- hclust(dist(data$oi), method = "complete")
    data$cluster <- cutree(clusters, oiclusters)

    # Compute the average values of all variables per cluster
    means <- aggregate(. ~ cluster, data, mean)
    means$days <- aggregate(b ~ cluster, data, length)$b

    alldays <- c(alldays, means$days)
    alldays <- unique(alldays)

    # Run the Skellam test on all clusters
    is_skellam <- TRUE
    for (c in 1:oiclusters)
      is_skellam <- is_skellam *
      check_bounds(data[data$cluster == c, ], noinfo = TRUE)

    # An no-information cluster (noinfo_temp) is defined as the cluster with
    # lowest trade intensity.
    # If all clusters pass the test, and the current no-information cluster has
    # larger number of days as the previous one, noinfo_temp is updated and the
    # clustering configuration is saved.

    if (is_skellam == TRUE) {

      lowest <- means$cluster[which.min(means$tot)]
      if (nrow(data[data$cluster == lowest, ]) > noinfo_temp) {

        noinfo_temp <- nrow(data[data$cluster == lowest, ])
        configuration <- data$cluster
      }
    }

    oiclusters <- oiclusters + 1
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # + STEP 1.2 | Find the largest no-info cluster satisfying the Skellam test  +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Pick the optimal configuration and compute mean values for all clusters
  data$cluster <- configuration
  means <- aggregate(. ~ cluster, data, mean)
  means$days <- aggregate(b ~ cluster, data, length)$b

  # Order the clusters in an increasing order of their total trade intensities
  cls <- means$cluster[order(means$tot)]

  # Initialize the no-infocluster by the cluster with the lowest trade intensity
  # and then add more clusters if the combined cluster passes the Skellam test.

  noinfocluster <- cls[1]
  is_skellam <- TRUE
  nextcluster <- 2

  while (is_skellam & nextcluster < length(cls)) {
    is_skellam <- check_bounds(data[data$cluster %in% cls[1:nextcluster], ],
                               noinfo = TRUE, conf = confidence) #
    if (is_skellam) noinfocluster <- c(noinfocluster, cls[nextcluster])
    nextcluster <- nextcluster + 1
  }

  # The index of observations within the no-information cluster are stored in
  # the variable rows
  rows <- which(data$cluster %in% noinfocluster)
  if (length(rows) == nrow(data))
    rows <- which(data$cluster %in% means$cluster[means$tot == min(means$tot)])

  # ************************************************************************** #
  # *                  STEP 2 | Find the information clusters                * #
  # ************************************************************************** #

  # Divide the data into no-info data and info data
  noinfodata <- data[rows, ]
  infodata <- data[-rows, ]

  # Find an estimate of the displacement of eb-es using the no information layer
  # The positive oi values in layer j are centered around eb-es+muj
  # The negative oi values in layer j are centered around eb-es-muj
  # If we translate all values by es-eb then the positive oi values will be
  # centered on muj and the negative oi values will be centered on -muj

  # After translation, we can take absolute values of oi and we are sure that
  # the absolute values will be centered around the same mean muj and actually
  # will be similarly distributed as the positive values of oi, i.e., follows
  # Skellam distribution with mean mu!

  # The only problem lies that the Skellam distribution has a domain R while we
  # only work with positive numbers.
  # However, when mu is sufficiently high, the probability assigned to tail is
  # negligible and can, therefore, be ignored. The distribution function F of
  # the random variable oi counting the difference between buys and sells
  # oi=b-s in layer j has mean muj, and we can establish that
  # if muj> Delta, F(oi<0)<epsilon. The approximation with Skellam therefore
  # holds.

  # Using the clustering algorithm hclust, which clusters data as function of
  # their proximity , we can make the Skellam test. If the skellam test fails,
  # we will not split the data but will rather run the clustering algorithm
  # again with a higher number of clusters.
  # We will only work with information clusters from now on!

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # + STEP 2.1 | Find the displacements and adjusting the data                 +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  infodata$cluster <- 1

  displacement_b <- mean(noinfodata$b)
  displacement_s <- mean(noinfodata$s)

  infodata$b <- infodata$b + displacement_s
  infodata$s <- infodata$s + displacement_b

  infodata$oi <- infodata$b - infodata$s
  infodata$aoi <- abs(infodata$oi)

  infodata <- infodata[order(infodata$aoi), ]

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # + STEP 2.2 | Find the information clusters using the Skellam test          +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # If the number of day in the informed trading days is equal to 1, then there
  # is a single information layer
  if (nrow(infodata) == 1) return(c(1, 59))


  aoiclusters <- 1
  rownames(infodata) <- NULL
  is_skellam <- FALSE

  while (!is_skellam) {

    # Cluster the aoi observations
    if (aoiclusters > 1) {

      clusters <- hclust(dist(infodata$aoi), method = "complete")
      infodata$cluster <- cutree(clusters, aoiclusters)
    }

    # Run the skellam tests on all clusters. If they all pass, the process stops
    # and we obtain our number of layers, otherwise, we increase the number of
    # clusters.
    is_skellam <- TRUE
    for (c in 1:aoiclusters) {
      is_skellam <- is_skellam *
        check_bounds(infodata[infodata$cluster == c, ], noinfo = FALSE)
    }

    if (!is_skellam) aoiclusters <- aoiclusters + 1
  }

  layers <- aoiclusters

  # -------------------------------------------------------------------
  # Uncomment the following code if you want to report noinfodays:
  # line 1: noinfodays <- nrow(noinfodata)
  # line 2: response <- c(layers, noinfodays)
  # -------------------------------------------------------------------

  response <- layers

  noinfodays <- nrow(noinfodata)
  response <- layers

  return(response)
}

#' @rdname detecting-layers
#' @param hyperparams A list containing the hyperparameters of the `ECM`
#' algorithm. When not empty, it contains one or more of the following
#' elements: `maxeval`, `tolerance`, `maxinit`, and `maxlayers`. More
#' about these elements are found in the Details section. `[i]` This is
#' an argument of the function `detectlayers_ecm()`.
#'
#' @export
detectlayers_ecm <- function(data, hyperparams = list()) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$detection()$fn)

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$detection()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, hyperparams)
  names(largs) <- names(formals())
  largs$fn <- "detect"
  rst <- .xcheck$args(largs)
  ux$stopnow(rst$off, m = rst$error, s = uierrors$detection()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)

  # Update 'hyperparams' by filling missing hyperparameters, and distribute the
  # new list to seven different variables: 'criterion', 'minalpha', 'maxlayers',
  # 'maxeval', 'external', 'tolerance', 'maxinit'
  # ----------------------------------------------------------------------------
  rst <- .xcheck$hyperparams(hyperparams, nrow(data))
  ux$stopnow(rst$off, m = rst$error, s = uierrors$mpin()$fn)
  hps <- rst$hyperparams
  hpn <- names(hps)
  for (i in seq_len(length(hpn))) assign(hpn[i], unname(unlist(hps[[i]])))

  getmpin <- mpin_ecm(data, xtraclusters = 3, hyperparams = hps, verbose = FALSE)

  return(getmpin@layers)
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


.split_cluster <- function(data, confidence = 0.999) {
  # splits clusters if the AOI observations do not fit in confidence interval
  # with confidence level 'confidence'.
  #
  # Args:
  #   data		  : a dataframe containing a variable 'aoi'
  #   confidence: a number used as confidence level in constructing a
  #               confidence interval
  #
  # Returns:
  #   a list of dataframes whose aoi observations fit in a confidence
  #   interval of level 'confidence'


  if (is.null(data)) return(NULL)
  data <- as.data.frame(data)

  # Return the dataframe 'data' as is if it has one unique aoi observation
  # --------------------------------------------------------------------------
  if (length(unique(data$aoi)) == 1) return(list(data))


  # Find the bounds of a skellam distribution with parameters u1, u2 at the
  # confidence level: 'confidence'
  # --------------------------------------------------------------------------
  # u1: the mean of the largest of buys and sells
  # u2: the mean of the lowest of buys and sells.
  data$maxbs <- pmax(data$b, data$s)
  data$minbs <- pmin(data$b, data$s)
  u1 <- round(mean(data$maxbs))
  u2 <- round(mean(data$minbs))

  # Find the skellam bounds using a confidence level: confidence
  ubound <- suppressWarnings(qskellam(1 - ((1 - confidence) / 2), u1, u2))
  lbound <- suppressWarnings(qskellam((1 - confidence) / 2, u1, u2))

  # Check if all aoi are within the Skellam bounds
  # --------------------------------------------------------------------------
  # If yes, return the dataframe, otherwise, split further.

  # Find the maximum and the minimum of aoi in the cluster under study.
  max_aoi <- max(data$aoi)
  min_aoi <- min(data$aoi)

  if (max_aoi < ubound & min_aoi > lbound) {
    return(list(data))
  } else {
    clusters <- hclust(dist(data$aoi), method = "complete")
    data$cluster <- cutree(clusters, 2)
    subs <- split(data, data$cluster)
    return(c(.split_cluster(subs[[1]]), .split_cluster(subs[[2]])))
  }
}

.findlayers <- function(data, confidence = 0.999, correct = TRUE) {
  # finds the number of information layers in the provided data using the
  # algorithm of Ersan(2016)
  #
  # Args:
  #   data : a dataframe containing a variable 'aoi'
  #   confidence: a number used as confidence level in constructing a
  #               confidence interval
  #
  # Returns:
  #   an integer respresenting the number of information layers in the data
  #   computed using the algorithm of Ersan(2016)


  # Prepare the dataset 'data'
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)

  # Adjust the dataset observations as in Ersan(2o16) if correct = TRUE
  # --------------------------------------------------------------------------
  if (correct) {

    # Find approximations for eb and es
    eb <- min(data$b)
    es <- min(data$s)

    # Adjust the data with eb, and es
    data$s <- data$s + eb
    data$b <- data$b + es
  }

  # Calculate the absolute order imbalance: aoi
  # --------------------------------------------------------------------------

  # Find the absolute imbalance per day aoi = |B - S|
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)


  # Cluster the aoi observations into 2 clusters initially
  # ------------------------------------------------------------------------
  clusters <- hclust(dist(data$aoi), method = "complete")
  data$cluster <- cutree(clusters, 2)

  # Divide 'data' into 2 dataframes, and run '.split_cluster' recursively
  # ------------------------------------------------------------------------
  subsets <- split(data, data$cluster)

  finalclusters <- c(.split_cluster(subsets[[1]], confidence),
                     .split_cluster(subsets[[2]], confidence))

  foundlayers <- length(finalclusters) - 1

  return(foundlayers)
}

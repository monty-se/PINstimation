## - | FILE  HEADER |
##
## Script name:
##    model_pin.R
##
## Purpose of script:
##    Implement the algorithms for generation of initial parameter sets,
##    as well as, the estimation methods of the original PIN model of
##    Easley et al. (2011, 2012)
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2022-10-18
##
## License:
##    GPL 3
## Email:
##    montasser.ghachem@pinstimation.com
##
##
##
## Public functions:
## ++++++++++++++++++
##
## pin():
##    Estimates the Probability of Informed Trading (PIN) using
##    custom initial parameter sets.
##
## pin_bayes():
##    Estimates the probability of Informed Trading (PIN) using
##    the bayesian approach as in Griffin et al.(2011) and initial
##    parameter sets from the algorithm of Ersan and Alici (2016).
##
## pin_ea():
##    Estimates the Probability of Informed Trading (PIN) using
##    the initial sets from the algorithm in Ersan and Alici (2016).
##
## pin_gwj():
##    Estimates the Probability of Informed Trading (PIN) using
##    the initial sets from the algorithm in Gan et al (2015).
##
## pin_yz():
##    Estimates the Probability of Informed Trading (PIN) using
##    the initial sets from the algorithm in Yan and Zhang (2012).
##
## initials_pin_ea():
##    Based on the algorithm in Ersan and Alici (2016),
##    generates initial parameter sets for the maximum likelihood
##    estimation of the `PIN` model.
##
## initials_pin_gwj():
##    Based on the algorithm in Gan et al. (2015),
##    generates an initial parameter set for the maximum likelihood
##    estimation of the `PIN` model.
##
## initials_pin_yz():
##    Based on the algorithm in Yan and Zhang (2012),
##    generates initial parameter sets for the maximum likelihood
##    estimation of the `PIN` model.
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


#' @title PIN estimation - custom initial parameter sets
#'
#' @description Estimates the Probability of Informed Trading (`PIN`)
#' using custom initial parameter sets
#'
#' @usage pin(data, initialsets, factorization = "E", verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param factorization A character string from
#' \code{\{"EHO", "LK", "E", "NONE"\}} referring to a given factorization. The
#' default value is set to \code{"E"}.
#'
#' @param initialsets A dataframe with the following variables in
#' this order (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es).
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the PIN model is displayed.
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
#' The factorization variable takes one of four values:
#' \itemize{
#'    \item \code{"EHO"} refers to the factorization in
#'    \insertCite{Easley2010;textual}{PINstimation}
#'    \item \code{"LK"}  refers to the factorization in
#'    \insertCite{WilliamLin2011;textual}{PINstimation}
#'    \item \code{"E"}  refers to the factorization in
#'    \insertCite{Ersan2016;textual}{PINstimation}
#'    \item \code{"NONE"} refers to the original likelihood function - with no
#' factorization
#' }
#'
#' @return Returns an object of class \code{estimate.pin}
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
#' #--------------------------------------------------------------
#' # Using generic function pin()
#' #--------------------------------------------------------------
#'
#' # Define initial parameters:
#' # initialset = (alpha, delta, mu, eps.b, eps.s)
#'
#' initialset <- c(0.3, 0.1, 800, 300, 200)
#'
#' # Estimate the PIN model using the factorization of the PIN likelihood
#' # function by Ersan (2016)
#'
#' estimate <- pin(xdata, initialsets = initialset, verbose = FALSE)
#'
#' # Display the estimated PIN value
#'
#' show(estimate@pin)
#'
#' # Display the estimated parameters
#'
#' show(estimate@parameters)
#'
#' # Store the initial parameter sets used for MLE in a dataframe variable,
#' # and display its first five rows
#'
#' initialsets <- estimate@initialsets
#' show(head(initialsets, 5))
#'
#' @export
pin <- function(data, initialsets, factorization = "E", verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, initialsets, factorization, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)



  # Get a local copy of user messages
  pin_ms <- uix$pin(nrows = nrow(initialsets), type = "custom")

  ux$show(verbose, m = pin_ms$start)

  ux$show(verbose, m = pin_ms$factorization[[factorization]])

  ux$show(verbose, m = pin_ms$loadinitials)

  ux$show(verbose, m = pin_ms$mlemethod)

  results <- .estimate_pin(data, factorization, initialsets, verbose = verbose)

  ux$show(verbose, m = pin_ms$complete)

  results@algorithm <- "CUSTOM"

  results@factorization <- factorization

  return(results)
}





#' @title PIN estimation - Bayesian approach
#'
#' @description Estimates the Probability of Informed Trading (`PIN`) using
#' Bayesian Gibbs sampling as in
#' \insertCite{griffin2021;textual}{PINstimation} and the initial sets
#' from the algorithm in \insertCite{ErsanAlici2016;textual}{PINstimation}.
#'
#' @usage pin_bayes(data, xtraclusters = 4, sweeps = 1000, burnin = 500,
#'                  prior.a = 1, prior.b = 2, verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{#(2 + xtraclusters)} clusters, thereby resulting in
#' \code{#comb(1 + xtraclusters, 1)} initial parameter sets in line with
#' \insertCite{ErsanAlici2016;textual}{PINstimation}. The default value is `4`.
#'
#' @param sweeps An integer referring to the number of iterations for the Gibbs
#' Sampler. This has to be large enough to ensure convergence of the Markov chain.
#' The default value is \code{1000}.
#'
#' @param burnin An integer referring to the number of initial iterations for
#' which the parameter draws should be discarded. This is to ensure that we keep
#' the draws at the point where the MCMC has converged to the parameter space in
#' which the parameter estimate is likely to fall. This figure must always be
#' less than the sweeps. The default value is \code{500}.
#'
#' @param prior.a An integer controlling the mean number of informed trades,
#' such as the prior of informed buys and sells is the Gamma density function
#' with \eqn{\mu} ~ `Ga(prior.a,` \eqn{\eta}`)`. The default value is \code{1}.
#' For more details, please refer to
#' \insertCite{griffin2021;textual}{PINstimation}.
#'
#' @param prior.b An integer controlling the mean number of uninformed trades,
#' such as the prior of uninformed buys and sells is the Gamma density function
#' with \eb ~ `Ga(prior.b,` \eqn{\eta}`)`, and \es ~ `Ga(prior.b,` \eqn{\eta}`)`.
#' The default value is \code{2}. For more details, please refer to
#' \insertCite{griffin2021;textual}{PINstimation}.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the PIN model is displayed.
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
#' The function \code{pin_bayes()} implements the algorithm detailed in
#' \insertCite{ErsanAlici2016;textual}{PINstimation}.
#' The higher the number of the additional clusters (\code{xtraclusters}), the
#' better is the estimation. \insertCite{ErsanAlici2016;textual}{PINstimation},
#' however, have shown the benefit of increasing this number beyond 5 is
#' marginal, and statistically insignificant.\cr \cr
#' The function \code{initials_pin_ea()} provides the initial parameter sets
#' obtained through the implementation of the
#' \insertCite{ErsanAlici2016;textual}{PINstimation} algorithm.
#' For further information on the initial parameter set determination, see
#' `initials_pin_ea()`.
#'
#' @return Returns an object of class \code{estimate.pin}
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # Use the function generatedata_mpin() to generate a dataset of
#' # 60 days according to the assumptions of the original PIN model.
#'
#' sdata <- generatedata_mpin(layers = 1)
#' xdata <- sdata@data
#'
#' # Estimate the PIN model using the Bayesian approach developed in
#' # Griffin et al. (2021), and initial parameter sets generated using the
#' # algorithm of Ersan and Alici (2016). The argument xtraclusters is
#' # set to 1. We also leave the arguments 'sweeps' and 'burnin' at their
#' # default values.
#' \donttest{
#' estimate <- pin_bayes(xdata, xtraclusters = 1, verbose = FALSE)
#'
#'
#' # Display the empirical PIN value at the data, and the PIN value
#' # estimated using the bayesian approach
#'
#' setNames(c(sdata@emp.pin, estimate@pin), c("data", "estimate"))
#'
#' # Display the empirial and the estimated parameters
#'
#' show(unlist(sdata@empiricals))
#' show(estimate@parameters)
#'
#' # Find the initial set that leads to the optimal estimate
#' optimal <- which.max(estimate@details$likelihood)
#'
#' # Store the matrix of Monte Carlo simulation for the optimal
#' # estimate, and display its last five rows
#'
#' mcmatrix <- estimate@details$markovmatrix[[optimal]]
#' show(tail(mcmatrix, 5))
#'
#' # Display the summary of Geweke test for the Monte Carlo matrix above.
#' show(estimate@details$summary[[optimal]])
#' }
#' @export
pin_bayes <- function(data, xtraclusters = 4, sweeps = 1000, burnin = 500,
                      prior.a = 1, prior.b = 2, verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, xtraclusters, sweeps, burnin, prior.a, prior.b, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)


  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  factorization <- "E"
  convergent <- 0
  pb_pin <- NULL

  pin_ms <- uix$pin()
  ux$show(verbose, m = pin_ms$start)
  ux$show(verbose, m = pin_ms$factorization[[factorization]])

  # Obtain the initial parameter sets and initial distribution of D(0)
  # -------------------------------------------------------------------------
  initials <- initials_pin_ea(
    data, xtraclusters = xtraclusters, verbose = FALSE)
  d0s <- attr(initials, which = "D0")
  ln <- nrow(initials)

  pin_ms <- uix$pin(nrows = nrow(initials), type = "EA")
  ux$show(verbose, m = pin_ms$loadinitials)
  ux$show(verbose, m = pin_ms$bayesmethod)

  # Initialize variables
  # -------------------------------------------------------------------------
  estimates <- NULL
  a0 <- prior.a
  b0 <- prior.b

  # Initialize the dataframe storing the results of the Bayesian estimation
  # -------------------------------------------------------------------------
  runs <- data.frame(matrix(0, ncol = 12, nrow = 0))
  sruns <- data.frame(summary = I(list()), markovmatrix = I(list()))
  runs <- cbind(runs, sruns)

  time_on <- Sys.time()

  for (i in seq_len(nrow(initials))) {

    # init0 holds the current initial set (alpha, delta, mu, eb, es)
    # Assign the initial parameters in init0 to variables
    # -------------------------------------------------------------------------
    init0 <- unname(unlist(initials[i, ]))
    a <- d <- mu <- eb <- es <- NULL
    variables <- .xmpin$varnames()
    for (j in 1:5) assign(variables[j], init0[j])


    # Initialize the classification D(0) by randomly selecting the type of
    # each day with probability 1/3, i.e. each day is equally likely to be
    # no-information day (0), a good-information (1), and a bad-information
    # day (2). Add the initial classification D(0) to the dataset 'data'
    # -------------------------------------------------------------------------

    dk <- t(rmultinom(nrow(data), 1:3, c(1/3, 1/3, 1/3)))
    dk <- apply(dk, 1, function(x) 0 * x[1] + 1 * x[2] + 2 * x[3])
    data$dz <- dk

    # generate eta (eta0) using the gamma function and the current parameters
    # -------------------------------------------------------------------------
    eta <- rgamma(n = 1, shape = 0.001 + 2* a0 + b0, rate = 0.001 + mu + eb + es)

    # Initialize the matrix receiving the different estimates of Gibbs sampler
    # -------------------------------------------------------------------------
    estimates <- init0

    if (i == 1 && verbose) {
      pb_pin <- ux$progressbar(minvalue = 0, maxvalue = ln)
      cat(uix$pin()$progressbar)
    }

    for (k in seq_len(sweeps)) {

      # Use the information day distribution $dz to draw informed buys (ib)
      # whenever dz == 1, and informed sells (is) whenever dz == 2
      data$ib <- apply(
        data, 1, function(x) (x[3]==1) * rbinom(1, x[1], mu/(mu + eb)))
      data$is <- apply(
        data, 1, function(x) (x[3]==2) * rbinom(1, x[2], mu/(mu + es)))

      # Deduce the uninformed buys (ub) and uninformed sells (us) calculated
      # for purposes of clarity of exposition
      data$ub <- data$b - data$ib
      data$us <- data$s - data$is

      # Calculate the number of non-information days (t0), good-information
      # days (t1), bad-information days (t2), and, total number of days (t).
      t <- nrow(data)
      t1 <- sum(data$dz == 1)
      t2 <- sum(data$dz == 2)
      t0 <- t - t1 - t2

      # (1) Update the value of mu by drawing once a value of mu(k) from
      # the posterior distribution Gamma(b + sum(ib + is), eta + t1 + t2)
      mu <- rgamma(n = 1, b0 + sum(data$ib) + sum(data$is), eta + t1 + t2)

      # (2) Update the value of es by drawing once a value of es(k) from
      # the posterior distribution Gamma(b + sum(us), eta + t)
      es <- rgamma(n = 1, b0 + sum(data$us), eta + t)

      # (3) Update the value of eb by drawing once a value of eb(k) from
      # the posterior distribution Gamma(b + sum(ub), eta + t)
      eb <- rgamma(n = 1, b0 + sum(data$ub), eta + t)

      # (4) Update the value of eta by drawing once a value of eta(k) from
      # the posterior distribution Gamma(0.001+2a0+b0, 0.001+mu+eb+es )
      eta <- rgamma(n = 1,10^(-3) + 2*a0 + b0, 10^(-3) + mu + eb + es)

      # (5) Update the value of alpha by drawing once a value of a(k) from
      # the posterior distribution Beta(1 + t1 + t2, 1 + t0)
      a <- rbeta(n = 1, 1 + t1 + t2, 1 + t0)

      # (6) Update the value of delta by drawing once a value of d(k) from
      # the posterior distribution Beta(1 + t1, 1 + t2)
      d <- rbeta(n = 1, 1 + t2, 1 + t1)

      # Calculate, for each day, the likelihood of being a non-information
      # day (l0), a good-information day (l1), and a bad-information day (l2)
      data$l0 = log(1-a) - (eb + es) + data$s * log(es) + data$b * log(eb)
      data$l1 = log(a*(1-d)) - (eb + es + mu) + data$s * log(es) +
        data$b * log(eb + mu)
      data$l2 = log(a*d) - (eb + es + mu) + data$s * log(es + mu) +
        data$b * log(eb)

      # Get the maximum of these likelihood lx
      data$lx <- with(data, pmax(l0, l1, l2))

      # Get the posterior probability for each day that it is a non-information
      # day (p0), a good-information day (p1), and a bad-information day (p2)
      # Gather it in the vector prob px = (p0, p1, p2)
      data$denom <- with(data, exp(l0 - lx) + exp(l1 - lx) + exp(l2 - lx))
      px <- with(data, cbind(exp(l0 - lx), exp(l1 - lx), exp(l2 - lx))) / data$denom

      # For each day, generate D(k) using the multinational distribution with probability
      # px[i, ] for each observation i.
      dk <- t(apply(px, 1, function(x) rmultinom(1:3,1, x[1:3])))
      dk <- apply(dk, 1, function(x) 0 * x[1] + 1 * x[2] + 2 * x[3])
      data$dz <- dk

      estimates <- rbind(estimates, c(a, d, mu, eb, es))

    }

    # Compute the pin value for each iteration
    estimates <- as.data.frame(estimates)
    estimates[, 6] <- apply(estimates, 1, function(x) .xmpin$compute_pin(x))
    rownames(estimates) <- paste("sweep.", 0:sweeps, sep = "")

    # Obtain the Geweke test scores to check if the Markov chain has converged
    # in the first 10% of the data
    xestimates <- coda::mcmc(data= estimates)
    geweketest <- coda::geweke.diag(xestimates, frac1 = 0.1, frac2 = 0.5)

    # Get rid of the first rows in the matrix (burnin)
    estimates <- tail(estimates, - burnin - 1)
    names(estimates) <- c(.xmpin$varnames(2), "PIN")
    optparams <- colMeans(estimates)
    thisrun <- c(init0, optparams[1:5], fact_pin_e(data, optparams[1:5]),
                 optparams[6])


    # Construct a matrix to hold the details of the Bayesian run
    bayesrun <- cbind(optparams, apply(estimates, 2, sd), geweketest$z,
                      pnorm(q=abs(geweketest$z), lower.tail=FALSE))
    rownames(bayesrun) <- c(.xmpin$varnames(2), "PIN")
    colnames(bayesrun) <- c("mean", "std.dev", "geweke.z-score", "geweke.p-value")

    runs[i, 1:12] <- thisrun
    runs[[i, "summary"]] <- bayesrun
    runs[[i, "markovmatrix"]] <- estimates
    colnames(runs) <- c(.xmpin$varnames(3), "summary", "markovmatrix")

    ## Update the progress bar and the messages in case of 'verbose'
    ## --------------------------------------------------------------
    if (verbose) setTxtProgressBar(pb_pin, i)

  }

  time_off <- Sys.time()

  runs <- as.data.frame(runs)
  initialsets <- ux$forcedf(initials, .xmpin$varnames(2))

  maximizer <- which.max(runs$likelihood)
  convergent <- sum(!is.na(runs$likelihood))

  # If no estimates are optimal, then the variable 'optimal' of type
  # 'estimate.pin'. If there are optimal estimates (maxmle != -Inf),
  # then update the variable 'optimal' details. Then, return 'optimal'.
  pin_optimal <-
    new("estimate.pin",
        success = FALSE,
        method = "BAYES",
        errorMessage = uierrors$pin()$failed,
        convergent.sets = convergent,
        initialsets = initialsets,
        dataset = data,
        likelihood = NaN
    )

  if (length(maximizer) > 0 && is.finite(maximizer)) {

    optparams <- unlist(runs[maximizer, 6:10])
    optlikhd <- runs[maximizer, ]$likelihood

    xlist <- .xmpin$get_goodbadmpin(
      .xmpin$compute_pin(optparams), optparams, TRUE)

    pin_optimal@success <- TRUE
    pin_optimal@parameters <- setNames(optparams, .xmpin$varnames(2))
    pin_optimal@likelihood <- optlikhd
    pin_optimal@details <- runs
    pin_optimal@runningtime <- ux$timediff(time_on, time_off)
    pin_optimal@errorMessage <- ""
    pin_optimal@pin <-  .xmpin$compute_pin(optparams)
    pin_optimal@pin.goodbad <- xlist
    attr(pin_optimal, "posteriors") <- .pin_posteriors(
      data, optparams)
  }

  ux$show(verbose, m = pin_ms$complete)

  pin_optimal@algorithm <- "EA"

  pin_optimal@factorization <- factorization

  return(pin_optimal)
}




#' @title PIN estimation - initial parameter sets of Ersan & Alici (2016)
#'
#' @description Estimates the Probability of Informed Trading (`PIN`) using the
#' initial sets from the algorithm in
#' \insertCite{ErsanAlici2016;textual}{PINstimation}.
#'
#' @usage pin_ea(data, factorization, xtraclusters = 4, verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param factorization A character string from
#' \code{\{"E", "EHO", "LK", "NONE"\}} referring to a given factorization. The
#' default value is \code{"E"}.
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{#(2 + xtraclusters)} clusters, thereby resulting in
#' \code{#comb(1 + xtraclusters, 1)} initial parameter sets in line with
#' \insertCite{ErsanAlici2016;textual}{PINstimation}. The default value is `4`.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the PIN model is displayed.
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
#' The factorization variable takes one of four values:
#' \itemize{
#'    \item \code{"EHO"} refers to the factorization in
#'    \insertCite{Easley2010;textual}{PINstimation}
#'    \item \code{"LK"}  refers to the factorization in
#'    \insertCite{WilliamLin2011;textual}{PINstimation}
#'    \item \code{"E"}  refers to the factorization in
#'    \insertCite{Ersan2016;textual}{PINstimation}
#'    \item \code{"NONE"} refers to the original likelihood function - with no
#' factorization
#' }
#' The function \code{pin_ea()} implements the algorithm detailed in
#' \insertCite{ErsanAlici2016;textual}{PINstimation}.
#' The higher the number of the additional layers (\code{xtraclusters}), the
#' better is the estimation. \insertCite{ErsanAlici2016;textual}{PINstimation},
#' however, have shown the benefit of increasing this number beyond 5 is
#' marginal, and statistically insignificant.\cr \cr
#' The function \code{initials_pin_ea()} provides the initial parameter sets
#' obtained through the implementation of the
#' \insertCite{ErsanAlici2016;textual}{PINstimation} algorithm.
#' For further information on the initial parameter set determination, see
#' `initials_pin_ea()`.
#'
#' @return Returns an object of class \code{estimate.pin}
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
#' # Estimate the PIN model using the factorization of Ersan (2016), and initial
#' # parameter sets generated using the algorithm of Ersan and Alici (2016).
#' # The argument xtraclusters is omitted so will take its default value 4.
#'
#' estimate <- pin_ea(xdata, verbose = FALSE)
#'
#' # Display the estimated PIN value
#'
#' show(estimate@pin)
#'
#' # Display the estimated parameters
#'
#' show(estimate@parameters)
#'
#' # Store the initial parameter sets used for MLE in a dataframe variable,
#' # and display its first five rows
#'
#' initialsets <- estimate@initialsets
#' show(head(initialsets, 5))
#'
#' @export
pin_ea <- function(data, factorization = "E", xtraclusters = 4,
                   verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, factorization, xtraclusters, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)


  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)

  pin_ms <- uix$pin()

  ux$show(verbose, m = pin_ms$start)

  ux$show(verbose, m = pin_ms$factorization[[factorization]])

  initialsets <- initials_pin_ea(
    data, xtraclusters = xtraclusters, verbose = FALSE)

  pin_ms <- uix$pin(nrows = nrow(initialsets), type = "EA")

  ux$show(verbose, m = pin_ms$loadinitials)

  ux$show(verbose, m = pin_ms$mlemethod)

  results <- .estimate_pin(data, factorization, initialsets, verbose = verbose)

  ux$show(verbose, m = pin_ms$complete)

  results@algorithm <- "EA"

  results@factorization <- factorization

  return(results)
}



#' @title PIN estimation - initial parameter set of Gan et al. (2015)
#'
#' @description Estimates the Probability of Informed Trading (`PIN`) using the
#' initial set from the algorithm in Gan et al.(2015).
#'
#' @usage pin_gwj(data, factorization = "E", verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first corresponds to
#' buyer-initiated trades (buys), and the second corresponds to seller-initiated
#' trades (sells).
#'
#' @param factorization A character string from
#' \code{\{"EHO", "LK", "E", "NONE"\}} referring to a given factorization. The
#' default value is set to `"E"`.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the PIN model is displayed.
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
#' The factorization variable takes one of four values:
#' \itemize{
#'    \item \code{"EHO"} refers to the factorization in
#'    \insertCite{Easley2010;textual}{PINstimation}
#'    \item \code{"LK"}  refers to the factorization in
#'    \insertCite{WilliamLin2011;textual}{PINstimation}
#'    \item \code{"E"}  refers to the factorization in
#'    \insertCite{Ersan2016;textual}{PINstimation}
#'    \item \code{"NONE"} refers to the original likelihood function - with no
#' factorization
#' }
#'
#' The function \code{pin_gwj()} implements the algorithm detailed in
#' \insertCite{Gan2015;textual}{PINstimation}. You can use the function
#' \code{initials_pin_gwj()} in order to get the initial parameter set.
#'
#' @return Returns an object of class \code{estimate.pin}
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
#' # Estimate the PIN model using the factorization of Ersan (2016), and initial
#' # parameter sets generated using the algorithm of Gan et al. (2015).
#' # The argument xtraclusters is omitted so will take its default value 4.
#'
#' estimate <- pin_gwj(xdata, verbose = FALSE)
#'
#' # Display the estimated PIN value
#'
#' show(estimate@pin)
#'
#' # Display the estimated parameters
#'
#' show(estimate@parameters)
#'
#' # Store the initial parameter sets used for MLE in a dataframe variable,
#' # and display its first five rows
#'
#' initialsets <- estimate@initialsets
#' show(head(initialsets, 5))
#'
#' @export
pin_gwj <- function(data, factorization = "E", verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, factorization, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)

  initialsets <- initials_pin_gwj(data, verbose = FALSE)

  pin_ms <- uix$pin(nrows = "1", type = "GWJ")

  ux$show(verbose, m = pin_ms$start)

  ux$show(verbose, m = pin_ms$factorization[[factorization]])

  ux$show(verbose, m = pin_ms$loadinitials)

  ux$show(verbose, m = pin_ms$mlemethod)

  results <- .estimate_pin(data, factorization, initialsets, verbose = verbose)

  ux$show(verbose, m = pin_ms$complete)

  results@algorithm <- "GWJ"

  results@factorization <- factorization

  return(results)
}



#' @title PIN estimation - initial parameter sets of Yan & Zhang (2012)
#'
#' @description Estimates the Probability of Informed Trading (`PIN`) using the
#' initial parameter sets generated using the grid search algorithm of
#' Yan and Zhang (2012).
#'
#' @usage pin_yz(data, factorization, ea_correction = FALSE, grid_size = 5,
#'                                                   verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param factorization A character string from
#' \code{\{"EHO", "LK", "E", "NONE"\}} referring to a given factorization. The
#' default value is `"E"`.
#'
#' @param ea_correction A binary variable determining whether the
#' modifications of the algorithm of \insertCite{Yan2012;textual}{PINstimation}
#' suggested by \insertCite{ErsanAlici2016;textual}{PINstimation} are
#' implemented. The default value is `FALSE`.
#'
#' @param grid_size An integer between `1`, and `20`;
#' representing the size of the grid. The default value is `5`. See
#' more in details.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the PIN model is displayed.
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
#' The factorization variable takes one of four values:
#' \itemize{
#'    \item \code{"EHO"} refers to the factorization in
#'    \insertCite{Easley2010;textual}{PINstimation}
#'    \item \code{"LK"}  refers to the factorization in
#'    \insertCite{WilliamLin2011;textual}{PINstimation}
#'    \item \code{"E"}  refers to the factorization in
#'    \insertCite{Ersan2016;textual}{PINstimation}
#'    \item \code{"NONE"} refers to the original likelihood function - with no
#' factorization
#' }
#'
#' The argument `grid_size` determines the size of the grid of the variables:
#' `alpha`, `delta`, and `eps.b`. If `grid_size` is set to a given value `m`,
#' the algorithm creates a sequence starting from \code{1/2m}, and ending in
#' \code{1 - 1/2m}, with a step of `1/m`. The default value of `5` corresponds
#' to the size of the grid in \insertCite{Yan2012;textual}{PINstimation}.
#' In that case, the sequence starts at \code{0.1 = 1/(2 x 5)}, and ends in
#' \code{0.9 = 1 - 1/(2 x 5)} with a step of \code{0.2 = 1/m}.
#'
#' The function \code{pin_yz()} implements, by default, the original
#' \insertCite{Yan2012;textual}{PINstimation} algorithm as the default value of
#' \code{ea_correction} takes the value \code{FALSE}.
#' When the value of \code{ea_correction} is set to \code{TRUE}; then, sets
#' with irrelevant `mu` values are excluded, and sets with boundary values are
#' reintegrated in the initial parameter sets.
#'
#' @return Returns an object of class \code{estimate.pin}
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
#' # Estimate the PIN model using the factorization of Lin and Ke(2011), and
#' # initial parameter sets generated using the algorithm of Yan & Zhang (2012).
#' # In contrast to the original algorithm, we set the grid size for the grid
#' # search algorithm at 3. The original algorithm assumes a grid of size 5.
#'
#' estimate <- pin_yz(xdata, "LK", grid_size = 3, verbose = FALSE)
#'
#' # Display the estimated PIN value
#'
#' show(estimate@pin)
#'
#' # Display the estimated parameters
#'
#' show(estimate@parameters)
#'
#' # Store the initial parameter sets used for MLE in a dataframe variable,
#' # and display its first five rows
#'
#' initialsets <- estimate@initialsets
#' show(head(initialsets, 5))
#'
#' @export
pin_yz <- function(data, factorization = "E", ea_correction = FALSE,
                   grid_size = 5, verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, factorization, ea_correction, grid_size, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)


  # If we use the improved algorithm developed by Ersan(2016)
  # We don't exclude the boundaries from the optimal estimates.
  # ----------------------------------------------------------
  exclude_boundaries <- !ea_correction

  pin_ms <- uix$pin()

  ux$show(verbose, m = pin_ms$start)

  ux$show(verbose, m = pin_ms$factorization[[factorization]])

  initialsets <- initials_pin_yz(
    data, ea_correction, grid_size = grid_size, verbose = FALSE)

  pin_ms <- uix$pin(nrows = nrow(initialsets), type = "YZ")

  ux$show(verbose, m = pin_ms$loadinitials)

  ux$show(verbose, m = pin_ms$mlemethod)

  results <- .estimate_pin(data, factorization, initialsets,
                           exclude_boundaries, verbose)

  ux$show(verbose, m = pin_ms$complete)

  results@algorithm <- ifelse(!ea_correction, "YZ", "YZ*")

  results@factorization <- factorization

  return(results)
}



#' @title Initial parameter sets of Ersan & Alici (2016)
#'
#' @description Based on the algorithm in Ersan and Alici (2016),
#' generates initial parameter sets for the maximum likelihood
#' estimation of the `PIN` model.
#'
#' @usage initials_pin_ea(data, xtraclusters = 4, verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param xtraclusters An integer used to divide trading days into
#' \code{#(2 + xtraclusters)} clusters, thereby resulting in
#' `#comb(1 + xtraclusters, 1)` initial parameter sets in line with
#' \insertCite{ErsanAlici2016;textual}{PINstimation}. The default value is `4`.
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
#' The function \code{initials_pin_ea()} uses a hierarchical agglomerative
#' clustering (HAC) to find initial parameter sets for
#' the maximum likelihood estimation. The steps in
#' \insertCite{ErsanAlici2016;textual}{PINstimation} algorithm differ from those
#' used by \insertCite{Gan2015;textual}{PINstimation}, and are summarized below.
#'
#' Via the use of HAC, daily absolute order imbalances (AOIs) are grouped in
#' \code{2+J} (default \code{J=4}) clusters. After sorting the clusters based on
#' AOIs, they are combined into two larger groups of days (event and no-event)
#' by merging neighboring clusters with each other. Consequently, those groups
#' are formed in `#comb(5, 1) = 5` different ways. For each of the `5`
#' configurations with which, days are grouped into two (event group and
#' no-event group), the procedure below is applied to obtain initial parameter
#' sets. \cr\cr
#' Days in the event group (the one with larger mean AOI) are distributed into
#' two groups, i.e. good-event days (days with positive OI) and bad-event days
#' (days with negative OI).
#' Initial parameters are obtained from the frequencies, and average trade
#' rates of three types of days. See
#' \insertCite{ErsanAlici2016;textual}{PINstimation} for further details.\cr\cr
#' The higher the number of the additional clusters (\code{xtraclusters}), the
#' better is the estimation. \insertCite{ErsanAlici2016;textual}{PINstimation},
#' however, have shown the benefit of increasing this number beyond 4 is
#' marginal, and statistically insignificant.
#'
#' @return Returns a dataframe of initial sets each consisting of five
#' variables \{\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es\}.
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
#' # Obtain a dataframe of initial parameters for the maximum likelihood
#' # estimation using the algorithm of Ersan and Alici (2016).
#'
#' init.sets <- initials_pin_ea(xdata)
#'
#' # Use the obtained dataframe to estimate the PIN model using the function
#' # pin() with custom initial parameter sets
#'
#' estimate.1 <- pin(xdata, initialsets = init.sets, verbose = FALSE)
#'
#' # pin_ea() directly estimates the PIN model using initial parameter sets
#' # generated using the algorithm of Ersan & Alici (2016).
#'
#' estimate.2 <- pin_ea(xdata, verbose = FALSE)
#'
#' # Check that the obtained results are identical
#'
#' show(estimate.1@parameters)
#' show(estimate.2@parameters)
#'
#' @export
initials_pin_ea <- function(data, xtraclusters = 4, verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, xtraclusters, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  data$oi <- data$b - data$s
  data$aoi <- abs(data$b - data$s)
  initials <- NULL

  # cluster aoi observations using HAC algorithm
  clusters <- hclust(dist(data$aoi), method = "complete")
  data$cluster <- cutree(clusters, xtraclusters + 2)

  # Find average aoi per cluster and rank clusters by mean(aoi)
  means <- aggregate(aoi ~ cluster, data, mean)
  means <- means[order(means$aoi), ]
  clrank <- means$cluster

  for (k in 0:xtraclusters) {
    noevent_range <- clrank[1:(1 + k)]
    data$set <-
      with(data, ifelse(
        cluster %in% noevent_range,
        "noEvent",
        ifelse(oi < 0, "bad", "good")
      ))

    means <- aggregate(. ~ set, data, mean)
    means$days <- aggregate(b ~ set, data, FUN = length)$b
    rownames(means) <- means$set

    # Compute estimates of the parameters as in Ersan (2016)
    # --------------------------------------------------------------------------
    # wg 	: share of days in good information cluster
    # wb	: share of days in bad information cluster
    # wn	: share of days in no-information cluster

    wb <- ifelse(is.na(means["bad", ]$days / sum(means$days)), 0,
                 means["bad", ]$days / sum(means$days))
    wg <- ifelse(is.na(means["good", ]$days / sum(means$days)), 0,
                 means["good", ]$days / sum(means$days))
    wn <- means["noEvent", ]$days / sum(means$days)

    a <- wb + wg
    d <- wb / a
    bb <- ifelse(is.na(means["bad", "b"]), 0, means["bad", "b"])
    nb <- ifelse(is.na(means["noEvent", "b"]), 0, means["noEvent", "b"])
    gb <- ifelse(is.na(means["good", "b"]), 0, means["good", "b"])

    bs <- ifelse(is.na(means["bad", "s"]), 0, means["bad", "s"])
    ns <- ifelse(is.na(means["noEvent", "s"]), 0, means["noEvent", "s"])
    gs <- ifelse(is.na(means["good", "s"]), 0, means["good", "s"])

    eb <-  sum(wb * bb + wn * nb) / sum(wb + wn)
    es <-  sum(wn * ns + wg * gs) / sum(wg + wn)
    diffe <- eb - es

    mu <- (wb / (wb + wg)) * (bs - bb + diffe) +
      (wg / (wb + wg)) * (gb - gs - diffe)

    initialset <- c(a, d, mu, eb, es)
    initialset <- pmax(initialset, rep(0, 5))
    initials <- rbind(initials, initialset)
    infod <- with(data, ifelse(set == "good", 1, ifelse(set == "bad", 2, 0)))

  }


  initials <- as.data.frame(initials)
  names(initials) <- c("alpha", "delta", "mu", "eps.b", "eps.s")

  pin_err <- uierrors$pin()
  ux$show(c = verbose, m = pin_err$displaysets(
    "initials_pin_ea(...)", nrow(initials)), warning = TRUE)

  return(invisible(initials))
}


#' @title Initial parameter set of Gan et al.(2015)
#'
#' @description Based on the algorithm in
#' \insertCite{Gan2015;textual}{PINstimation}, generates an initial parameter
#' set for the maximum likelihood estimation of the `PIN` model.
#'
#' @usage initials_pin_gwj(data, verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
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
#' @return Returns a dataframe containing numerical vector of five elements
#' \{\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es\}.
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
#' # Obtain the initial parameter set for the maximum likelihood estimation
#' # using the algorithm of Gan et al.(2015).
#'
#' initparams <- initials_pin_gwj(xdata)
#'
#' # Use the obtained dataframe to estimate the PIN model using the function
#' # pin() with custom initial parameter sets
#'
#' estimate.1 <- pin(xdata, initialsets = initparams, verbose = FALSE)
#'
#' # pin_gwj() directly estimates the PIN model using an initial parameter set
#' # generated using the algorithm of Gan et al.(2015).
#'
#' estimate.2 <- pin_gwj(xdata, "E", verbose = FALSE)
#'
#' # Check that the obtained results are identical
#'
#' show(estimate.1@parameters)
#' show(estimate.2@parameters)
#'
#' @export
initials_pin_gwj <- function(data, verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  a <- d <- mu <- eb <- es <- NULL
  data$oi <- data$b - data$s

  clusters <- hclust(dist(data$oi), method = "complete")
  data$cluster <- cutree(clusters, 3)

  # Aggregate the variables b, s and OI by the group cluster
  # ---------------------------------------------------------------------

  means <- aggregate(. ~ cluster, data, mean)
  days <- aggregate(b ~ cluster, data, FUN = length)$b
  means$days <- days

  # Sort the dataframe means by the means of order imbalances
  # The first row concerns the cluster with the lowest average imbalance
  # The third row concerns the cluster with the highest average imbalance
  # An example of the resulting sorted dataframe means:

  # |        | cluster|     b|     s|    OI| days|
  # |:-------|-------:|-----:|-----:|------:|----:|
  # |bad     |       1| 376.5| 395.5|  -19.0| 1200|
  # |noEvent |       2| 186.5| 539.0| -352.5|  400|
  # |good    |       3| 885.0| 399.0|  486.0|  400|
  # ---------------------------------------------------------------------

  means <- means[order(means$oi), ]
  row.names(means) <- c("bad", "noEvent", "good")


  # Using the dataframe means, we calculate the different estimates of the
  # parameters.
  # ----------------------------------------------------------------------

  wb <- means["bad", ]$days / sum(means$days)
  wg <- means["good", ]$days / sum(means$days)
  wn <- means["noEvent", ]$days / sum(means$days)

  a <- wb + wg
  d <- wb / a
  eb <- (wb * means["bad", "b"] + wn * means["noEvent", "b"]) / (wb + wn)
  es <- (wg * means["good", "s"] + wn * means["noEvent", "s"]) / (wg + wn)
  mub <- max(means["good", "b"] - eb, 0)
  mus <- max(means["bad", "s"] - es, 0)
  mu <- (wg * mub + wb * mus) / (wg + wb)

  initialset <- setNames(c(a, d, mu, eb, es), .xmpin$varnames(2))

  pin_err <- uierrors$pin()
  ux$show(c = verbose, m = pin_err$displaysets(
    "initials_pin_gwj(...)", 1), warning = TRUE)
  return(invisible(initialset))
}


#' @title Initial parameter sets of Yan and Zhang (2012)
#'
#' @description Based on the grid search algorithm of
#' \insertCite{Yan2012;textual}{PINstimation}, generates
#' initial parameter sets for the maximum likelihood estimation of the `PIN`
#' model.
#'
#' @usage initials_pin_yz(data, grid_size = 5, ea_correction = FALSE,
#'  verbose = TRUE)
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param grid_size An integer between `1`, and `20`;
#' representing the size of the grid. The default value is `5`. See
#' more in details.
#'
#' @param ea_correction A binary variable determining whether the
#' modifications of the algorithm of \insertCite{Yan2012;textual}{PINstimation}
#' suggested by \insertCite{ErsanAlici2016;textual}{PINstimation} are
#' implemented. The default value is `FALSE`.
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
#' The argument `grid_size` determines the size of the grid of the variables:
#' `alpha`, `delta`, and `eps.b`. If `grid_size` is set to a given value `m`,
#' the algorithm creates a sequence starting from \code{1/2m}, and ending in
#' \code{1 - 1/2m}, with a step of `1/m`. The default value of `5` corresponds
#' to the size of the grid in \insertCite{Yan2012;textual}{PINstimation}.
#' In that case, the sequence starts at \code{0.1 = 1/(2 x 5)}, and ends in
#' \code{0.9 = 1 - 1/(2 x 5)} with a step of \code{0.2 = 1/m}.
#'
#' The function \code{initials_pin_yz()} implements, by default, the original
#' \insertCite{Yan2012;textual}{PINstimation} algorithm as the default value of
#' \code{ea_correction} takes the value \code{FALSE}.
#' When the value of \code{ea_correction} is set to \code{TRUE}; then, sets
#' with irrelevant `mu` values are excluded, and sets with boundary values are
#' reintegrated in the initial parameter sets.
#'
#' @return Returns a dataframe of initial sets each consisting of five
#' variables \{\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es\}.
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
#' # The function pin_yz() allows the user to directly estimate the PIN model
#' # using the full set of initial parameter sets generated using the algorithm
#' # of Yan and # Zhang (2012).
#' \donttest{
#' estimate.1 <- pin_yz(xdata, verbose = FALSE)
#' }
#' # Obtaining the set of initial parameter sets using initials_pin_yz allows
#' # us to estimate the PIN model using a subset of these initial sets.
#'
#' initparams <- initials_pin_yz(xdata, verbose = FALSE)
#'
#' # Use 10 randonly chosen initial sets from the dataframe 'initparams' in
#' # order to estimate the PIN model using the function pin() with custom
#' # initial parameter sets
#'
#' numberofsets <- nrow(initparams)
#' selectedsets <- initparams[sample(numberofsets, 10),]
#'
#' estimate.2 <- pin(xdata, initialsets = selectedsets, verbose = FALSE)
#'
#' # Compare the parameters and the pin values of both specifications
#' \donttest{
#' comparison <- rbind(c(estimate.1@parameters, pin = estimate.1@pin),
#'                     c(estimate.2@parameters, estimate.2@pin))
#'
#' rownames(comparison) <- c("all", "10")
#'
#' show(comparison)
#' }
#'
#' @export
initials_pin_yz <- function(data, grid_size = 5, ea_correction = FALSE,
                            verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$pin()$fn)

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, grid_size, ea_correction, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "pin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$pin()$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(data)
  pin_err <- uierrors$pin()
  a <- d <- mu <- eb <- NULL
  gs <- grid_size

  mean_b <- mean(data$b)
  mean_s <- mean(data$s)
  max_t <- max(data$b, data$s)

  # Generate the set of values of a, d and eb which is mean_b
  # multiplied by a coefficient g in (0.1, 0.3, 0.5, 0.7, 0.9)
  # ----------------------------------------------------------
  half_step <- 1 / (2 * grid_size)
  grid <- seq(half_step, 1 - half_step, 2 * half_step)

  a_values <- grid
  d_values <- grid
  eb_values <- grid * mean_b

  # Take the cartesian product of the values of a, d and g and
  # store them in dataframe called init_values.
  # ----------------------------------------------------------
  init_values <- expand.grid(a_values, d_values, eb_values)
  init_values <- as.data.frame(init_values)
  colnames(init_values) <- c("a", "d", "eb")

  # Construct the variables es and mu using the formulas in Yan
  # and Zhang (2012)
  # ----------------------------------------------------------
  init_values$mu <- with(init_values, (mean_b - eb) / (a * (1 - d)))
  init_values$es <- with(init_values, mean_s - (a * d * mu))

  # Delete all rows for which es is negative
  # ----------------------------------------------------------
  init_values <- init_values[init_values$es >= 0, ]
  newsize <- nrow(init_values)
  deletedrows <- gs^3 - newsize
  if (deletedrows > 0)
    pin_err <- uierrors$pin(dr = deletedrows)
    ux$show(c = verbose, m = pin_err$yzdeleted, warning = TRUE)

  # Reorder the columns of the dataframe in the following order
  # ----------------------------------------------------------
  init_values <- init_values[, .xmpin$varnames()]

  # If we use the improved algorithm developed by Ersan(2016)
  # We exclude all values of mu which are larger than the max
  # value of b and s in the database.
  # ----------------------------------------------------------
  if (ea_correction) {
    init_values <- init_values[init_values$mu <= max_t, ]
    deletedrows <- newsize - nrow(init_values)
    if (deletedrows > 0)
      pin_err <- uierrors$pin(dr = deletedrows)
      ux$show(c = verbose, m = pin_err$eacorrected, warning = TRUE)
  }

  # Return the variables initialsets as a dataframe
  # ----------------------------------------------------------
  initials <- init_values
  names(initials) <- .xmpin$varnames(2)
  rownames(initials) <- NULL

  ux$show(c = verbose, m = pin_err$displaysets(
    "initials_pin_yz(...)", nrow(initials)), warning = TRUE)

  return(invisible(initials))
}












##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


.estimate_pin <- function(data, factorization = c("E", "EHO", "LK", "NONE"),
                         initialsets, exclude_boundaries = FALSE,
                         verbose = TRUE) {
# Estimates the PIN model using the factorization, initial sets provided
#
# Args:
#   data          : the dataset of buys and sells
#   factorization : the factorization of the likelihood function
#   initialsets   : a list of initial parameter sets
#   exclude_boundaries:
#                 : if TRUE, sets with alpha=0 and alpha=1 are excluded
#   verbose       : if TRUE, details about the progress are displayed
#
# Returns:
#   returns an object 'estimate.pin' containing optimal parameters

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------

  data <- ux$prepare(data)
  time_on <- time_off <- NULL

  time_on <- Sys.time()

  # Pick the appropriate log-Likelihood function factorization based on the
  # user's choice of the argument 'factorization'. If no factorization is
  # given, the original likelihood function is used.
  # --------------------------------------------------------------
  idx <- match(toupper(factorization), c("E", "LK", "EHO"))
  idx <- c("A", "B", "C")[idx]
  mlefn <- switch(idx,
                  "A" = factorizations$pin_e(data),
                  "B" = factorizations$pin_lk(data),
                  "C" = factorizations$pin_eho(data),
                  factorizations$pin_none(data)
    )

  # If initialsets is a dataframe, convert to a list
  if (is.data.frame(initialsets))
    initialsets <- ux$tolist(initialsets)
  if (!is.list(initialsets)) initialsets <- list(initialsets)
  ln <- length(initialsets)

  # Initialize the dataframe storing the results of each run of the MLE.
  runs <- data.frame(matrix(NA, ncol = 0, nrow = 12))

  # Inialize the variable max_mle which will store the highest value
  # of the log-likelihood function.
  # --------------------------------------------------------------
  estimates <- NULL

  # For each element in the list of initial values, we optimize the
  # log-likelihood function using the function 'optim' that minimizes
  # the function 'mlefn' (see above) using the method 'Nelder-Mead'
  # and using the current initial value element.
  # The result of estimation is stored in the variable 'estimates'.
  # If we have a list of initial values, we will only keep the
  # estimates only if the value of the log-likelihood function is
  # higher than the previously attained maximum (max_mle).If the
  # estimates using a new initial set of parameters yield a higher
  # value of the log-likelihood function, these estimates replace
  # the previously stored estimates and the maximum level attained
  # of the log-likelihood function (max_mle) is updated.
  # --------------------------------------------------------------
  convergent <- 0
  pb_pin <- NULL
  optimal <- list(likelihood = -Inf)

  for (i in seq_len(ln)) {

    temp_run <- unlist(initialsets[[i]])
    thisrun <- c(temp_run, rep(0, 7))

    tryCatch({
      low <- c(0, 0, 0, 0, 0)
      up <- c(1, 1, Inf, Inf, Inf)
      estimates <- suppressWarnings(
        neldermead(initialsets[[i]], mlefn, lower = low, upper = up))
    })

    if (!is.null(estimates)) {

      # The likelihood is the additive inverse of the value in estimates
      estimates$likelihood <- - estimates$value

      convergent <- convergent + is.finite(estimates$likelihood)

      boundary <- (exclude_boundaries == TRUE &&
                     (estimates[["par"]][1] == 0 | estimates[["par"]][1] == 1))

      # Update the optimal estimates conditional on the value of boundary
      # If algorithm = "YZ": exclude boundary solutions. (boundary = T)

      optimal <- ux$update_optimal(optimal, estimates, !boundary)

      thisrun <- c(temp_run, estimates[["par"]], -estimates$value,
                   .xmpin$compute_pin(estimates[["par"]]))

    }

    runs <- rbind(runs, thisrun)

    ## Update the progress bar and the messages in case of 'verbose'
    ## --------------------------------------------------------------
    if (verbose) {
      if (i == 1) {
        pb_pin <- ux$progressbar(minvalue = 0, maxvalue = ln)
        cat(uix$pin()$progressbar)
      }
      setTxtProgressBar(pb_pin, i)
    }

  }

  time_off <- Sys.time()

  # After finding the optimal parameters over the set of initial
  # parameter sets; probability of informed trading (PIN) is calculated.
  # It will be reported in table along with the parameter estimate
  # and the optimal value of the log-likelihood function.
  # --------------------------------------------------------------
  colnames(runs) <- .xmpin$varnames(3)
  rownames(runs) <- paste("set.", seq_len(ln), sep = "")

  initialsets <- ux$forcedf(initialsets, .xmpin$varnames(2))
  if (nrow(initialsets))
    colnames(initialsets) <- .xmpin$varnames(2)
  runs <- round(runs, digits = 6)

  # If no estimates are optimal, then the variable 'optimal' of type
  # 'estimate.pin'. If there are optimal estimates (maxmle != -Inf),
  # then update the variable 'optimal' details. Then, return 'optimal'.
  pin_optimal <-
    new("estimate.pin",
        success = FALSE,
        errorMessage = uierrors$pin()$failed,
        convergent.sets = convergent,
        initialsets = initialsets,
        dataset = data,
        likelihood = NaN
    )

  if (is.finite(optimal$likelihood)) {

    xlist <- .xmpin$get_goodbadmpin(
      .xmpin$compute_pin(optimal[["par"]]), optimal[["par"]], TRUE)

    pin_optimal@success <- TRUE
    pin_optimal@parameters <- setNames(optimal[["par"]], .xmpin$varnames(2))
    pin_optimal@likelihood <- -optimal$value
    pin_optimal@details <- runs
    pin_optimal@runningtime <- ux$timediff(time_on, time_off)
    pin_optimal@errorMessage <- ""
    pin_optimal@pin <-  .xmpin$compute_pin(optimal[["par"]])
    pin_optimal@pin.goodbad <- xlist
    attr(pin_optimal, "posteriors") <- .pin_posteriors(
      data, unlist(pin_optimal@parameters))

  }

  return(pin_optimal)

}

.pin_posteriors <- function(data, params) {
# Computes the posterior probabilities of the PIN model at the specified
# parameters
#
# Args:
#   data    : the dataset of buys and sells
#   params  : the parameters at which to calculate the posterior prob.
#
# Returns:
#   a dataframe containing the daily posterior probabilities

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------

  dx <- ux$prepare(data)
  a <- d <- mu <- eb <- es <- NULL

  variables <- .xmpin$varnames()
  for (i in 1:5) {
    assign(variables[i], params[i])
  }

  # Start by constructing variables g1, g2, g3. Each of them is
  # constructed daily and is stored in a column with the same name.
  # The variable gmax is constructed by taking the maximum among
  # g1, g2 and g3; and is stored in a column with the same name.
  # --------------------------------------------------------------
  dx$g1 <- -mu + dx$b * log(1 + mu / eb)
  dx$g2 <- -mu + dx$s * log(1 + mu / es)
  dx$g3 <- 0
  dx$gmax <- pmax(dx$g1, dx$g2, dx$g3)

  dx$p1 <- dx$b * log(eb) + dx$s * log(es) + dx$gmax
  dx$p2 <- log(a * (1 - d) * exp(dx$g1 - dx$gmax) +
      a * d * exp(dx$g2 - dx$gmax) + (1 - a) * exp(dx$g3 - dx$gmax))
  dx$dnm <- dx$p1 + dx$p2

  # Create a function that takes the values of b, s, g1, g2, g3 and
  # gmax for each row, computes two parts of the EA likelihood
  # (p1 and p2); and store them in columns with the same names.
  # pn: numerator of P(n|b, s)
  # pg: numerator of P(g|b, s)
  # pb: numerator of P(b|b, s)
  # dnm : denominator of all posterior probabilities
  # --------------------------------------------------------------

  dx$pn <- dx$b * log(eb) + dx$s * log(es) + log(1 - a)
  dx$pg <- dx$b * log(eb + mu) - mu + dx$s * log(es) + log(a * (1 - d))
  dx$pb <- dx$b * log(eb) - mu + dx$s * log(es + mu) + log(a * d)

  posteriors <- data.frame(with(
    data,
    cbind(exp(dx$pn - dx$dnm), exp(dx$pg - dx$dnm), exp(dx$pb - dx$dnm))
  ))

  colnames(posteriors) <- c("post.N", "post.G", "post.B")

  return(posteriors)
}

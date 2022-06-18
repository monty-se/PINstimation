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
#' # omitted so will take its default value 4. We also leave the arguments
#' # 'sweeps' and 'burnin' at their default values.
#'
#' estimate <- pin_bayes(xdata, verbose = FALSE)
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
#'
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

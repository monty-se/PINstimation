## - | FILE  HEADER |
##
## Script name:
##    model_factorizations.R
##
## Purpose of script:
##    Implement factorizations of the different PIN likelihood functions.
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2023-03-17
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
## fact_pin_eho()}:
##    The factorization of the PIN likelihood function of
##    Easley et al.(2010).
##
## fact_pin_lk():
##    The factorization of the PIN likelihood function of
##    Lin and Ke.(2011).
##
## fact_pin_e():
##    The factorization of the PIN likelihood function of
##    Ersan (2016).
##
## fact_mpin():
##    The factorization of the MPIN likelihood function of
##    Ersan (2016).
##
## fact_adjpin():
##    The factorization of the AdjPIN likelihood function of
##    Ersan and Ghachem (2022c).
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


#' @title Factorizations of the different PIN likelihood functions
#'
#' @description
#' The `PIN` likelihood function is derived from the original `PIN` model as
#' developed by \insertCite{Easley1992;textual}{PINstimation} and
#' \insertCite{Easley1996;textual}{PINstimation}. The maximization of the
#' likelihood function as is leads to computational problems, in particular,
#' to floating point errors. To remedy to this issue, several
#' log-transformations or factorizations of the different `PIN` likelihood
#' functions have been suggested.
#' The main factorizations in the literature are:
#'
#' \itemize{
#' \item \code{fact_pin_eho()}: factorization of
#' \insertCite{Easley2010;textual}{PINstimation}
#' \item \code{fact_pin_lk()}: factorization of
#' \insertCite{WilliamLin2011;textual}{PINstimation}
#' \item \code{fact_pin_e()}: factorization of
#' \insertCite{Ersan2016;textual}{PINstimation}
#' }
#'
#' The factorization of the likelihood function of the multilayer `PIN` model,
#' as developed in \insertCite{Ersan2016;textual}{PINstimation}.
#' \itemize{
#' \item \code{fact_mpin()}: factorization of
#' \insertCite{Ersan2016;textual}{PINstimation}
#' }
#'
#' The factorization of the likelihood function of the adjusted `PIN` model
#' \insertCite{Duarte09}{PINstimation}, is derived, and presented in
#' \insertCite{Ersan2022b;textual}{PINstimation}.
#' \itemize{
#' \item \code{fact_adjpin()}: factorization in
#' \insertCite{Ersan2022b;textual}{PINstimation}
#' }
#'
#'
#' @param data A dataframe with 2 variables: the first
#' corresponds to buyer-initiated trades (buys), and the second corresponds
#' to seller-initiated trades (sells).
#'
#' @param parameters In the case of the `PIN` likelihood
#' factorization, it is an ordered numeric vector (\eqn{\alpha}, \eqn{\delta},
#' \eqn{\mu}, \eb, \es). In the case of the `MPIN` likelihood factorization,
#' it is an ordered numeric vector (__\eqn{\alpha}__, __\eqn{\delta}__,
#'  __\eqn{\mu}__, \eb, \es), where __\eqn{\alpha}__, __\eqn{\delta}__,
#' and __\eqn{\mu}__ are numeric vectors of size `J`, where `J` is the
#' number of information layers in the data.
#' In the case of the `AdjPIN` likelihood factorization, it is an ordered
#' numeric vector (\eqn{\alpha}, \eqn{\delta}, \eqn{\theta}, \eqn{\theta'},
#' \eb, \es, \mub, \mus, \Db, \Ds). The default value is `NULL`.
#'
#' @details The argument 'data' should be a numeric dataframe, and contain
#' at least two variables. Only the first two variables will be considered:
#' The first variable is assumed to correspond to the total number of
#' buyer-initiated trades, while the second variable is assumed to
#' correspond to the total number of seller-initiated trades. Each row or
#' observation correspond to a trading day. `NA` values will be ignored.
#'
#' Our tests, in line with \insertCite{WilliamLin2011;textual}{PINstimation},
#' and \insertCite{ErsanAlici2016;textual}{PINstimation}, demonstrate very
#' similar results for \code{fact_pin_lk()}, and \code{fact_pin_e()}, both
#' having substantially better estimates than \code{fact_pin_eho()}.
#'
#' @return If the argument `parameters` is omitted, returns a function
#' object that can be used with the optimization functions `optim()`,
#' and `neldermead()`.
#'
#' If the argument `parameters` is provided, returns a numeric value of the
#' log-likelihood function evaluated at the dataset \code{data} and the
#' parameters \code{parameters}, where \code{parameters} is a numeric vector
#' following this order (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es)
#' for the factorizations of the `PIN` likelihood function, (__\eqn{\alpha}__,
#' __\eqn{\delta}__, __\eqn{\mu}__, \eb, \es) for the factorization of the
#' `MPIN` likelihood function, and (\eqn{\alpha}, \eqn{\delta}, \eqn{\theta},
#' \eqn{\theta'}, \eb, \es ,\mub, \mus, \Db, \Ds) for the factorization of
#' the `AdjPIN` likelihood function.
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
#' # ------------------------------------------------------------------------ #
#' # Using fact_pin_eho(), fact_pin_lk(), fact_pin_e() to find the likelihood #
#' # value as factorized by Easley(2010), Lin & Ke (2011), and Ersan(2016).   #
#' # ------------------------------------------------------------------------ #
#'
#' # Choose a given parameter set to evaluate the likelihood function at a
#' # givenpoint  = (alpha, delta, mu, eps.b, eps.s)
#'
#' givenpoint <- c(0.4, 0.1, 800, 300, 200)
#'
#' # Use the ouput of fact_pin_e() with the optimization function optim() to
#' # find optimal estimates of the PIN model.
#'
#' model <- suppressWarnings(optim(givenpoint, fact_pin_e(xdata)))
#'
#' # Collect the model estimates from the variable model and display them.
#'
#' varnames <- c("alpha", "delta", "mu", "eps.b", "eps.s")
#' estimates <- setNames(model$par, varnames)
#' show(estimates)
#'
#' # Find the value of the log-likelihood function at givenpoint
#'
#' lklValue <- fact_pin_lk(xdata, givenpoint)
#'
#' show(lklValue)
#'
#' # ------------------------------------------------------------------------ #
#' # Using fact_mpin() to find the value of the MPIN likelihood function as   #
#' # factorized by Ersan (2016).                                              #
#' # ------------------------------------------------------------------------ #
#'
#' # Choose a given parameter set to evaluate the likelihood function at a
#' # givenpoint  = (alpha(), delta(), mu(), eps.b, eps.s) where alpha(), delta()
#' # and mu() are vectors of size 2.
#'
#' givenpoint <- c(0.4, 0.5, 0.1, 0.6, 600, 1000, 300, 200)
#'
#' # Use the output of fact_mpin() with the optimization function optim() to
#' # find optimal estimates of the PIN model.
#'
#' model <- suppressWarnings(optim(givenpoint, fact_mpin(xdata)))
#'
#' # Collect the model estimates from the variable model and display them.
#'
#' varnames <- c(paste("alpha", 1:2, sep = ""), paste("delta", 1:2, sep = ""),
#'               paste("mu", 1:2, sep = ""), "eb", "es")
#' estimates <- setNames(model$par, varnames)
#' show(estimates)
#'
#' # Find the value of the MPIN likelihood function at givenpoint
#'
#' lklValue <- fact_mpin(xdata, givenpoint)
#'
#' show(lklValue)
#'
#' # ------------------------------------------------------------------------ #
#' # Using fact_adjpin() to find the value of the DY likelihood function as   #
#' # factorized by Ersan and Ghachem (2022b).                                 #
#' # ------------------------------------------------------------------------ #
#'
#' # Choose a given parameter set to evaluate the likelihood function
#' # at a the initial parameter set givenpoint = (alpha, delta,
#' # theta, theta',eps.b, eps.s, muB, muS, db, ds)
#'
#' givenpoint <- c(0.4, 0.1, 0.3, 0.7, 500, 600, 800, 1000, 300, 200)
#'
#' # Use the output of fact_adjpin() with the optimization function
#' # neldermead() to find optimal estimates of the AdjPIN model.
#'
#' low <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#' up <- c(1, 1, 1, 1, Inf, Inf, Inf, Inf, Inf, Inf)
#' model <- nloptr::neldermead(
#' givenpoint, fact_adjpin(xdata), lower = low, upper = up)
#'
#' # Collect the model estimates from the variable model and display them.
#'
#' varnames <- c("alpha", "delta", "theta", "thetap", "eps.b", "eps.s",
#'               "muB", "muS", "db", "ds")
#' estimates <- setNames(model$par, varnames)
#' show(estimates)
#'
#' # Find the value of the log-likelihood function at givenpoint
#'
#' adjlklValue <- fact_adjpin(xdata, givenpoint)
#' show(adjlklValue)
#' @name factorizations
NULL


#' @rdname factorizations
#' @export
fact_pin_eho <- function(data, parameters = NULL) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$fact(model = "PIN")$fn)

  return(get_factorization(list(data, parameters), fact = "EHO"))

}

#' @rdname factorizations
#' @export
fact_pin_lk <- function(data, parameters = NULL) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$fact(model = "PIN")$fn)

  return(get_factorization(list(data, parameters), fact = "LK"))

}

#' @rdname factorizations
#' @export
fact_pin_e <- function(data, parameters = NULL) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$fact(model = "PIN")$fn)

  return(get_factorization(list(data, parameters), fact = "E"))

}


#' @rdname factorizations
#' @export
fact_mpin <- function(data, parameters = NULL) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$fact(model = "MPIN")$fn)

  return(get_factorization(list(data, parameters), fact = "MPIN"))

}

#' @rdname factorizations
#' @export
fact_adjpin <- function(data, parameters = NULL) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$fact(model = "AdjPIN")$fn)

  return(get_factorization(list(data, parameters), fact = "AdjPIN"))

}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++




get_factorization <- function(arglist, fact = NULL) {

  # Recover the model from the factorization
  # -------------------------------------------------------------------------
  model <- fact
  if (fact %in% c("E", "EHO", "LK")) model <- "PIN"

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- setNames(arglist, c("data", "parameters"))
  rst <- .xcheck$args(arglist = largs, fn = tolower(model))
  ux$stopnow(rst$off, m = rst$error, s = uierrors$fact(model = model)$fn)

  # Prepare 'data' and initialize variables
  # --------------------------------------------------------------------------
  data <- ux$prepare(largs$data)
  parameters <- largs$parameters

  factfunction <- switch(
    EXPR = fact,
    "EHO" = factorizations$pin_eho(data),
    "LK" = factorizations$pin_lk(data),
    "E" = factorizations$pin_e(data),
    "MPIN" = factorizations$mpin(data),
    "AdjPIN" = factorizations$adjpin(data)
  )

  if (!is.null(parameters)) factfunction <- - factfunction(parameters)

  return(factfunction)

}


# -----------------------------------------------------------------------------#
# PIN FACTORIZATIONS                                                           #
# -----------------------------------------------------------------------------#


factorizations <- list(

  pin_none = function(data) {
    # returns the original likelihood function evaluated at the dataset
    # 'data' to be used with the optimization function neldermead()
    #
    # Args:
    #   data    : the dataset of buys and sells
    #
    # Returns:
    #   a function with argument 'params'

    function(params) {

      # If 'params' is not valid, return +Inf
      # --------------------------------------------------------------
      if (!missing(params) && length(params) != 5) return(+Inf)

      # Prepare 'data' and initialize variables
      # --------------------------------------------------------------

      data <- ux$prepare(data)
      a <- d <- mu <- eb <- es <- NULL

      variables <- .xmpin$varnames()
      for (i in 1:5) assign(variables[i], params[i])

      # Perform operations to compute the 3 parts of the original PIN
      # likelihood function (p1, p2 and p3) and store them into columns
      # data$p1, data$p2 and data$p3.
      # --------------------------------------------------------------
      data$p1 <- a * (1 - d) * with(data, dpois(b, mu + eb) * dpois(s, es))
      data$p2 <- a * d * with(data, dpois(b, eb) * dpois(s, mu + es))
      data$p3 <- (1 - a) * with(data, dpois(b, eb) * dpois(s, es))


      # Compute and return the value of the log-likelihood function
      # --------------------------------------------------------------
      data$dlklhood <- with(data, p1 + p2 + p3)

      lkhd <- prod(data$dlklhood)

      return(-lkhd)
    }
  },

  pin_eho = function(data) {
    # returns the EHO factorization of the likelihood function evaluated
    # at the dataset 'data' to be used with optimization functions such
    # as optim() or neldermead()
    #
    # Args:
    #   data    : the dataset of buys and sells
    #
    # Returns:
    #   a function with argument 'params'

    function(params) {

      # If 'params' is not valid, return +Inf
      # --------------------------------------------------------------
      if (!missing(params) && length(params) != 5) return(+Inf)

      # Prepare 'data' and initialize variables
      # --------------------------------------------------------------

      data <- ux$prepare(data)
      a <- d <- mu <- eb <- es <- NULL

      variables <- .xmpin$varnames()
      for (i in 1:5) assign(variables[i], params[i])

      # Start by constructing variables xs, xb and m
      # --------------------------------------------------------------
      xs <- es / (mu + es)
      xb <- eb / (mu + eb)
      data$m <- with(data, pmin(b, s) + pmax(b, s) / 2)

      # Perform operations to compute the 2 parts of the EHO likelihood
      # (p1 and p2) and store them into columns data$p1 and data$p2.
      # --------------------------------------------------------------
      data$p1 <- with(
        data,
        b * log(mu + eb) + s * log(mu + es) + m * log(xb) + m * log(xs) -
          (eb + es)
      )
      data$p2 <- with(
        data,
        log(
          a * d * exp(-mu) * xb^ (b - m) * xs^ (-m) + a * (1 - d) * exp(-mu) *
            + xb^ (-m) * xs^ (s - m) + (1 - a) * xb^ (b - m) * xs^ (s - m)
        )
      )

      # Compute and return the value of the log-likelihood function
      # --------------------------------------------------------------
      data$dlklhood <- with(data, ifelse(is.nan(p2), p1, p1 + p2))

      lkhd <- sum(data$dlklhood)

      if (is.infinite(lkhd)) return(+Inf)

      # Subtract Sum(log(B!S!)) = sum(log(B!) + log(S!))
      # , which is equivalent to sum(log(B!)) + sum(log(S!))
      lkhd <- lkhd - sum(lfactorial(data$b)) - sum(lfactorial(data$s))

      return(-lkhd)
    }
  },

  pin_lk = function(data) {
    # returns the LK factorization of the likelihood function evaluated
    # at the dataset 'data' to be used with optimization functions such
    # as optim() or neldermead()
    #
    # Args:
    #   data    : the dataset of buys and sells
    #
    # Returns:
    #   a function with argument 'params'

    function(params) {

      # If 'params' is not valid, return +Inf
      # --------------------------------------------------------------
      if (!missing(params) && length(params) != 5) return(+Inf)

      # Prepare 'data' and initialize variables
      # --------------------------------------------------------------

      data <- ux$prepare(data)
      a <- d <- mu <- eb <- es <- NULL

      variables <- .xmpin$varnames()
      for (i in 1:5) assign(variables[i], params[i])

      # Start by constructing variables e1, e2, e3. Each of them is
      # constructed daily and is stored in a column with the same name.
      # The variable emax is constructed by taking the maximum among
      # e1, e2 and e3; and is stored in a column with the same name.
      # --------------------------------------------------------------
      data$e1 <- rep(-mu, length(data$b)) - data$b * log(1 + mu / eb)
      data$e2 <- rep(-mu, length(data$s)) - data$s * log(1 + mu / es)
      data$e3 <- -data$b * log(1 + mu / eb) - data$s * log(1 + mu / es)
      data$emax <- pmax(data$e1, data$e2, data$e3)

      # Create a function that takes the values of b, s, e1, e2, e3 and
      # emax for each row, computes two parts of the LK likelihood
      # (p1 and p2); and store them in columns with the same names.
      # --------------------------------------------------------------
      data$p1 <- with(
        data, b * log(eb + mu) + s * log(es + mu) - (eb + es) + emax)

      data$p2 <- with(
        data, log(a * d * exp(e1 - emax) + a * (1 - d) * exp(e2 - emax) +
                    (1 - a) * exp(e3 - emax))
      )

      # Compute and return the value of the log-likelihood function
      # --------------------------------------------------------------
      data$dlklhood <- data$p1 + data$p2

      lkhd <- sum(data$dlklhood)

      if (is.infinite(lkhd)) return(+Inf)

      # Subtract Sum(log(B!S!)) = sum(log(B!) + log(S!))
      # , which is equivalent to sum(log(B!)) + sum(log(S!))
      lkhd <- lkhd - sum(lfactorial(data$b)) - sum(lfactorial(data$s))

      return(-lkhd)
    }
  },

  pin_e = function(data) {
    # returns the E factorization of the likelihood function evaluated
    # at the dataset 'data' to be used with optimization functions such
    # as optim() or neldermead()
    #
    # Args:
    #   data    : the dataset of buys and sells
    #
    # Returns:
    #   a function with argument 'params'

    function(params) {

      # If 'params' is not valid, return +Inf
      # --------------------------------------------------------------
      if (!missing(params) && length(params) != 5) return(+Inf)

      # Prepare 'data' and initialize variables
      # --------------------------------------------------------------

      data <- ux$prepare(data)
      a <- d <- mu <- eb <- es <- NULL

      variables <- .xmpin$varnames()
      for (i in 1:5) assign(variables[i], params[i])

      # Start by constructing variables g1, g2, g3. Each of them is
      # constructed daily and is stored in a column with the same name.
      # The variable gmax is constructed by taking the maximum among
      # g1, g2 and g3; and is stored in a column with the same name.
      # --------------------------------------------------------------
      data$g1 <- rep(-mu, length(data$b)) + data$b * log(1 + mu / eb)
      data$g2 <- rep(-mu, length(data$s)) + data$s * log(1 + mu / es)
      data$g3 <- 0
      data$gmax <- pmax(data$g1, data$g2, data$g3)


      # Create a function that takes the values of b, s, g1, g2, g3 and
      # gmax for each row, computes two parts of the EA likelihood
      # (p1 and p2); and store them in columns with the same names.
      # --------------------------------------------------------------
      data$p1 <- with(data, b * log(eb) + s * log(es) - (eb + es) + gmax)

      data$p2 <- with(
        data, log(a * (1 - d) * exp(g1 - gmax) + a * d * exp(g2 - gmax) +
                    (1 - a) * exp(g3 - gmax))
      )

      # Compute and return the value of the log-likelihood function
      # --------------------------------------------------------------
      data$dlklhood <- data$p1 + data$p2

      lkhd <- sum(data$dlklhood)

      if (is.infinite(lkhd)) return(+Inf)

      # Subtract Sum(log(B!S!)) = sum(log(B!) + log(S!))
      # , which is equivalent to sum(log(B!)) + sum(log(S!))
      lkhd <- lkhd - sum(lfactorial(data$b)) - sum(lfactorial(data$s))

      return(-lkhd)
    }
  },

  mpin = function(data) {
    # computes the MPIN log-likelihood function as factorized by Ersan(2016)
    # at the provided data, as used with the minimization functions such as
    # optim() or neldermead()
    #
    # Args:
    #   data   : a dataframe containing two variables 'b' and 's'
    #
    # Returns:
    #   the function of the factorized MPIN log-likehood function evaluated at
    #   the provided data

    function(params) {

      # Prepare 'data' and initialize variables
      # ------------------------------------------------------------------------

      data <- ux$prepare(data)
      a <- d <- mu <- eb <- es <- NULL

      # Collect the model parameters
      # ------------------------------------------------------------------------
      layers <- (length(params) - 2) / 3
      variables <- c("a", "d", "mu", "eb", "es")
      values <- unname(split(params, rep(1:5, c(layers, layers, layers, 1, 1))))
      for (i in seq_len(length(values)))
        assign(variables[i], unname(unlist(values[[i]])))


      # Order the vectors alpha, delta by increasing mu
      ordmu <- order(mu)
      a <- a[ordmu]
      d <- d[ordmu]
      mu <- mu[ordmu]

      # Compute the two parts of the factorization
      # ------------------------------------------------------------------------
      # Construct the variables e1, e2, e3, emax = max(ei) as in Ersan (2016).

      e1 <- kronecker(data$b, t(mu), function(x, y) log(1 + y / eb) * x - y)
      e2 <- kronecker(data$s, t(mu), function(x, y) log(1 + y / es) * x - y)
      e3 <- 0
      de <- data.frame(e1, e2, e3)
      emax <- do.call(pmax, de)

      data$p1 <- with(data, b * log(eb) + s * log(es) - (eb + es) + emax)
      data$p2 <- cbind(t(t(exp(e1 - emax)) * (a * (1 - d))),
                       t(t(exp(e2 - emax)) * (a * d)),
                       (1 - sum(a)) * exp(e3 - emax))

      data$p2 <- log(rowSums(data$p2))

      # Compute the log-likelihood as sum of daily log-likelihoods
      # --------------------------------------------------------------
      data$dlklhood <- data$p1 + data$p2
      lkhd <- sum(data$dlklhood)

      # Make additional checks on the parameters
      # --------------------------------------------------------------

      if (sum(a) > 1) return(+Inf)
      if (is.infinite(lkhd)) return(+Inf)

      # Subtract Sum(log(B!S!)) = sum(log(B!) + log(S!))
      # , which is equivalent to sum(log(B!)) + sum(log(S!))
      lkhd <- lkhd - sum(lfactorial(data$b)) - sum(lfactorial(data$s))

      return(-lkhd)

    }
  },

  adjpin_none = function(data, restricted = NULL) {

    function(params) {

      # Check, prepare and initialize variables
      # ------------------------------------------------------------------------

      data <- ux$prepare(data)
      restricted <- .xadjpin$allrestrictions(restricted)
      len <- 10 - sum(unlist(restricted))
      if (!missing(params) & length(params) != len)
        return(+Inf)
      a <- d <- t <- tp <- mub <- mus <- mu <- NULL
      dx <- db <- ds <- eb <- es <- e <- eps <- NULL

      # Assign values to variables
      # ------------------------------------------------------------------------
      variables <- .xadjpin$vars(restricted, all = TRUE)
      for (i in seq_len(length(params))) assign(variables[i], params[i])

      if (restricted$theta) tp <- t
      if (restricted$mu) mub <- mus <- mu
      if (restricted$eps) eb <- es <- e
      if (restricted$d) db <- ds <- dx

      xpois <- function(xb, lambda) {
        return(exp(-lambda) * lambda^xb / (factorial(xb)))
      }

      # Compute the 6 parts of the likelihood function
      # ------------------------------------------------------------------------

      data$p11 <- (1 - a) * (1 - t) * apply(
        data, 1, function(x) xpois(x[1], eb) * xpois(x[2], es))

      data$p12 <- (1 - a) * t * apply(
        data, 1, function(x) xpois(x[1], eb + db) * xpois(x[2], es + ds))

      data$p21 <- a * (1 - d) * (1 - tp) * apply(
        data, 1, function(x) xpois(x[1], eb + mub) * xpois(x[2], es))

      data$p22 <- a * (1 - d) * tp * apply(
        data, 1, function(x) xpois(x[1], eb + db + mub) * xpois(x[2], es + ds))

      data$p31 <- a * d * (1 - tp) * apply(
        data, 1, function(x) xpois(x[1], eb) * xpois(x[2], es + mus))

      data$p32 <- a * d * tp * apply(
        data, 1, function(x) xpois(x[1], eb + db) * xpois(x[2], es + ds + mus))

      # Compute and return the value the likelihood function
      # ------------------------------------------------------------------------
      data$likd <- data$p11 + data$p12 + data$p21 +
        data$p22 + data$p31 + data$p32
      lkhd <- prod(data$likd)

      if (is.nan(lkhd)) lkhd <- +Inf

      return(-lkhd)
    }
  },

  adjpin = function(data, restricted = NULL) {
    # computes the AdjPIN log-likelihood function as factorized by Ersan et
    # al.(2022)at the provided data, as used with the minimization functions
    # such as optim() or neldermead()
    #
    # Args:
    #   data   : a dataframe containing two variables 'b' and 's'
    #
    # Returns:
    #   the function of the factorized AdjPIN log-likehood function evaluated at
    #   the provided data

    function(params) {

      # Prepare 'data' and initialize variables
      # ------------------------------------------------------------------------

      data <- ux$prepare(data)
      restricted <- .xadjpin$allrestrictions(restricted)

      len <- 10 - sum(unlist(restricted))
      if (!missing(params) && length(params) != len)
        return(+Inf)
      a <- d <- t <- tp <- mub <- mus <- mu <- NULL
      dx <- db <- ds <- eb <- es <- e <- NULL

      # Assign values to variables
      # ------------------------------------------------------------------------
      variables <- .xadjpin$vars(restricted, all = TRUE)
      for (i in seq_len(length(params))) assign(variables[i], params[i])

      if (restricted$theta) tp <- t
      if (restricted$mu) mub <- mus <- mu
      if (restricted$eps) eb <- es <- e
      if (restricted$d) db <- ds <- dx

      # Compute e0, e1,..., e5 and emax = max(e0,...,e5)
      # ------------------------------------------------------------------------
      e0 <- 0
      e1 <- rep(-db - ds, length(data$b)) + data$b * log(1 +  db / eb) +
        data$s * log(1 + ds / es)
      e2 <- rep(-mub, length(data$b)) + data$b * log(1 + mub / eb)
      e3 <- rep(-mub - db - ds, length(data$b)) + data$b *
        log(1 + db / eb + mub / eb) + data$s * log(1 + ds / es)
      e4 <- rep(-mus, length(data$s)) + data$s * log(1 + mus / es)
      e5 <- rep(-mus - db - ds, length(data$s)) + data$b *
        log(1 + db / eb) + data$s * log(1 + ds / es + mus / es)
      emax <- pmax(e0, e1, e2, e3, e4, e5)

      # Compute the 2 parts of the  factorized likelihood function -------------
      # ------------------------------------------------------------------------
      data$p1 <- with(data, b * log(eb) + s * log(es) - (eb + es) + emax)
      data$p2 <- with(data, log((1 - a) * (1 - t) * exp(e0 - emax) +
                                  (1 - a) * t * exp(e1 - emax) +
                                  a * (1 - tp) * (1 - d) * exp(e2 - emax) +
                                  a * tp * (1 - d) * exp(e3 - emax) +
                                  a * (1 - tp) * d * exp(e4 - emax) +
                                  a * tp * d * exp(e5 - emax))
      )
      data$p2[is.na(data$p2)] <- 0

      # Compute and return the value the likelihood function
      # ------------------------------------------------------------------------
      data$dlklhood <- data$p1 + data$p2

      lkhd <- sum(data$dlklhood)
      if (is.infinite(lkhd) | is.na(lkhd)) return(+Inf)

      # Subtract Sum(log(B!S!)) = sum(log(B!) + log(S!)),
      # which is equivalent to sum(log(B!)) + sum(log(S!))
      lkhd <- lkhd - sum(lfactorial(data$b)) - sum(lfactorial(data$s))

      return(-lkhd)

    }
  },

  ivpin = function(data) {
    # returns the factorization of the likelihood function associed the ivpin
    # model evaluated at the dataset 'data' to be used with optimization
    # functions such as optim() or neldermead()
    #
    # Args:
    #   data    : the dataset of Vb, Vs and t (See paper of Ke and Lin 2017)
    #
    # Returns:
    #   a function with argument 'params'

    function(params) {

      # If 'params' is not valid, return +Inf
      # --------------------------------------------------------------
      if (!missing(params) && length(params) != 5) return(+Inf)

      # Prepare 'data' and initialize variables
      # --------------------------------------------------------------

      colnames(data) <- c("vb", "vs", "t")
      a <- d <- mu <- eb <- es <- NULL

      # Get the names of the variable from the function .xmpin().
      # Without arguments , it returns c("a", "d", "mu", "eb", "es")
      variables <- .xmpin$varnames()
      for (i in 1:5) assign(variables[i], params[i])

      # Start by constructing variables e1, e2, e3. Each of them is
      # constructed daily and is stored in a column with the same name.
      # The variable emax is constructed by taking the maximum among
      # e1, e2 and e3; and is stored in a column with the same name.
      # -------------------------------------------------------------
      # e1 <- rep(log(alpha * delta), nrow(data)) + data$vb * log(eps.b) +
      #   data$vs * log(eps.s + mu) - (eps.b + eps.s + mu) * data$t
      # e2 <- rep(log(alpha * (1 - delta)), nrow(data)) + data$vb * log(eps.b + mu) +
      #   data$vs * log(eps.s) - (eps.b + eps.s + mu) * data$t
      # e3 <- rep(log(1 - alpha), nrow(data)) + data$vb * log(eps.b) +
      #   data$vs * log(eps.b) - (eps.b + eps.s) * data$t
      # emax <- pmax(e1, e2, e3)

      e1 <- log(a * d) + data$vb * log(eb) +
        data$vs * log(es + mu) - (eb + es + mu) * data$t
      e2 <- log(a * (1 - d)) + data$vb * log(eb + mu) +
        data$vs * log(es) - (eb + es  + mu) * data$t
      e3 <- log(1 - a) + data$vb * log(eb) +
        data$vs * log(es) - (eb + es) * data$t
      emax <- pmax(e1, e2, e3, na.rm = TRUE)

      # Compute and return the value of the log-likelihood function
      # --------------------------------------------------------------
      lkhd <- - sum(log(exp(e1 - emax) + exp(e2 - emax) + exp(e3 - emax)) +
                      emax, na.rm = TRUE)

      return(lkhd)
    }
  }

)

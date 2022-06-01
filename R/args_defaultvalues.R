## FILE HEADER
##
## Script name:
##    args_defaultvalues.R
##
## Purpose of script:
##    lists and computes the default values of the different variables used
##    as arguments of the different functions included in the package.
##
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
##
## ++++++++++++++++++
##
## Notes:
##
## Package PINstimation
## website: www.pinstimation.com
## Authors: Montasser Ghachem and Oguz Ersan


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++

.default <- list(

  ranges = function(model = "mpin") {

    kmin_eb <- 100
    kmax_eb <- 10000

    if (model == "mpin") {

      drange <- list(alpha = c(0, 1), delta = c(0, 1),
                     eps.b = c(kmin_eb, kmax_eb),
                     eps.s = NULL, mu = NULL)

    }

    if (model == "adjpin") {

      drange <- list(alpha = c(0, 1), delta = c(0, 1),
                     theta = c(0, 1), thetap = c(0, 1),
                     eps.b = c(kmin_eb, kmax_eb),
                     eps.s = NULL, mu.b = NULL, mu.s = NULL,
                     d.b = NULL, d.s = NULL)
    }

    return(drange)

  },

  rangebounds = function(model = "mpin") {

    rmin <- 1
    rmax <- +Inf
    bounds <- list()

    if (model == "mpin") {
      bounds$minv <- c(0, 0, rmin, rmin, rmin)
      bounds$maxv <- c(1, 1, rmax, rmax, rmax)
      bounds$probk <- 2
    }

    if (model == "adjpin") {
      bounds$minv <- c(0, 0, 0, 0, rmin, rmin, rmin, rmin, rmin, rmin)
      bounds$maxv <- c(1, 1, 1, 1, rmax, rmax, rmax, rmax, rmax, rmax)
      bounds$probk <- 4
    }

    return(bounds)

  },

  controls = function() {

    dcontrols <- list(
      eps_ratio = c(0.75, 1.25), mu_ratio = 1.25,
      maxlayers = 5, confidence = 0.995, overlap = c(1, 1.5)
    )

    return(dcontrols)
  },

  controlbounds = function() {

    bounds <- list()
    bounds$minv <- c(0.5, 1, 1, 0.5, 0.5)
    bounds$maxv <- c(2, 5, 10, 1, 20)

    return(bounds)

  },

  hyperparams = function(adj) {

    dhyperparams <- list(
      criterion = "BIC", minalpha = 0.001, tolerance = 0.001,
      maxeval = 100, maxlayers = 8, maxinit = 100)

    # For Adjpin model, restrict hyperparameters to tolerance and maxeval
    if (adj) dhyperparams <- dhyperparams[c(3, 4)]

    return(dhyperparams)
  },

  hyperbounds = function(adj) {

    bounds <- list()
    bounds$minv <- c(0, 0, 0, 1, 1, 1)
    bounds$maxv <- c(0, 0.1, 5, 10000, 10, 500000)

    # For Adjpin model, restrict hyperparameters to tolerance and maxeval
    if (adj) {
      bounds$minv  <- bounds$minv[c(3, 4)]
      bounds$maxv <- bounds$maxv[c(3, 4)]
    }

    return(bounds)

  },

  initialsets = c("GE", "CL", "RANDOM"),

  confidence = c(0, 1),

  timelag = c(0, +Inf),

  tradinghours = c(1, +Inf),

  samplength = c(1, +Inf),

  xtraclusters = c(0, ifelse(!is.null(getOption("numberoftradingdays")) &&
                                       getOption("numberoftradingdays") > 0,
                             getOption("numberoftradingdays"), +Inf)),

  layers = c(1, ifelse(!is.null(getOption("numberoftradingdays")) &&
                         getOption("numberoftradingdays") > 0,
                       getOption("numberoftradingdays"), +Inf)),

  buckets = c(1, +Inf),

  timebarsize = c(1, +Inf),

  num_init = c(1, 1000),

  grid_size = c(1, 20),

  series = c(1, 10^8),

  days = c(10, 10^8),

  algorithm =  c("TICK", "QUOTE", "LR", "EMO"),

  method = c("ML", "ECM"),

  detectlayers = c("EG", "E", "ECM"),

  factorization = c("LK", "E", "EHO", "NONE"),

  criterion = c("BIC", "AIC", "AWE"),

  displaydigits = list(d1 = 6, d2 = 2, d3 = 3),

  aggregation_parallel = TRUE,

  mpin_parallel = FALSE,

  reportdays = FALSE,

  parallel_cap = function() {

    xcap <- getOption("pinstimation.parallel.threshold")
    if (!ux$is.integer(xcap)) {
      options(pinstimation.parallel.threshold = 100)
    }

    return(getOption("pinstimation.parallel.threshold"))
  },

  parallel_cores = function() {

    dfcores <- getOption("pinstimation.parallel.cores")
    xcores <- ux$get_cores()

    if (!ux$is.integer(dfcores) || dfcores < 1 ||
        dfcores > xcores || dfcores > 2) {
      if (xcores > 1) xcores <- 2
      options(pinstimation.parallel.cores = xcores)
    }

    return(getOption("pinstimation.parallel.cores"))
  }

)

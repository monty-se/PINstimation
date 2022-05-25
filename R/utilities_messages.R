## FILE HEADER
##
## Script name:
##    utilities_messages.R
##
## Purpose of script:
##    lists and computes all messages displayed in the package,
##    including error messages, and warnings. It contains four
##    list of functions:
##    :: + uix:
##    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##    computes messages that are displayed when a given function
##    is being executed. It gives an idea about the operation
##    performed.
##    :: + uierrors:
##    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##    computes errors that are displayed when an error occurs,
##    and the execution is stopped.
##    :: + uiconflicts
##    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##    computes conflict messages that are displayed when there
##    are conflicts between different controls for the function
##    generating datasets: generatedata_mpin()
##    :: + uiclasses
##    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##    computes messages and outputs displayed when a given S4
##    object is displayed. The S4 objects are either estimation
##    result output with names "estimate.*", where * refers to
##    the name of the model being estimated; and objects storing
##    simulation data results, namely objects of class 'dataset'
##    and of class 'data.series'. For more information, check the
##    file 'output_classes.R'.
##
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


uix <- list(

  classification = function(rows = 0, method = "", timelag = 0,
                            time = "", isparallel = T) {
    ui <- list()
    ui$start <- "[+] Trade classification started"
    ui$complete <- "[+] Trade classification completed"
    ui$unrecognized <- paste("  |[=] Unknown classification algorithm:",
    "Tick algorithm is used")
    ui$algorithm <- paste(
      "  |[=] Classification algorithm \t:", method, "algorithm")
    ui$number <- paste(
      "  |[=] Number of trades in dataset\t:", ux$sep(rows), "trades")
    ui$lag <- paste(
      "  |[=] Time lag of lagged variables \t:", timelag, "milliseconds")

    process <- ifelse(isparallel, "parallel", "sequential")
    ui$computing <- paste(
      "  |[1] Computing lagged variables \t: using", process, "processing")

    ui$progressbar <- " of variables computed"
    ui$time <- paste("  |[=] Computed lagged variables \t: in", time)

    ui$aggregating <- ifelse(
      method == "Tick",
      paste("  |[1] Computing aggregated trades \t: using Tick algorithm"),
      paste("  |[2] Computing aggregated trades \t: using lagged variables")
    )


    return(ui)
  },

  mpin = function(nrows = 0, layers = 0, initlayers = 0, exptime = 0,
                  maxinit = 0, maxlayers = 0, criterion = "") {
    ui <- list()
    ui$start <- "[+] MPIN estimation started"
    ui$complete <- "\n[+] MPIN estimation completed"
    ui$emcomplete <- "[+] MPIN estimation completed"
    ui$detectsets <- paste("  |[1] Detecting layers from initialsets: ",
                           initlayers, " information layer(s) detected", sep = "")
    ui$detectdata <- "  |[1] Detecting layers from data \t:"
    ui$algorithm <- list(ECM = " using the ECM algorithm",
                         E = " using Ersan (2016)",
                         EG = " using Ersan and Ghachem (2022a)")
    ui$loadinitials <- paste(
      "\r  |[2] Loading initial parameter sets\t: ", nrows,
      " custom initial set(s) loaded", sep = "")
    ui$numlayers <- paste("  |[=] Number of layers in the data \t: ", layers,
      " information layer(s) detected", sep = "")
    ui$selectedlayers <- paste("  |[1] Using user-selected layers \t: ", layers,
      " layer(s) assumed in the data", sep = "")
    ui$computinginitials <-  paste(
      "  |[2] Computing initial parameter sets :",
      "using algorithm of Ersan (2016)")
    ui$expectedtime <- paste(
      "  |[3] Computing expected running time \t: ", exptime, sep = "")
    ui$mlemethod <- paste("  |[3] Estimating the MPIN model \t:",
                             " Maximum-likelihood standard estimation", sep = "")
    ui$emmethod <- paste("  |[3] Estimating the MPIN model \t:",
      " Expectation-Conditional Maximization algorithm", sep = "")
    ui$differentlayers <- paste(
      "  |[!] The number of layers detected (", initlayers,
      ")  differs from the argument",
      "'layers' (", layers, ")", sep = "")
    ui$progressbar <- " of mpin estimation completed"

    ui$emprogressbar <- paste(
      " of estimation completed [", layers, " layer(s)]", sep = "")

    ui$layersrange <- paste("  |[1] Computing the range of layers \t: ",
                           layers, " layer(s) provided by user", sep = "")
    ui$maxlayersrange <- paste("  |[1] Computing the range of layers \t: ",
                           "information layers from 1 to ", maxlayers, sep = "")
    ui$selectinitials <- paste("  |[=] Selecting initial parameter sets : max",
                               maxinit, "initial sets per estimation")
    ui$selectcriterion <- paste("\n  |[3] Selecting the optimal model \t:",
      " using lowest Information Criterion (", criterion, ")", sep = "")


    return(ui)
  },

  adjpin = function(nrows = 0, exptime = 0, maxinit = 0, init = "",
                  maxlayers = 0, criterion = "") {
    ui <- list()
    if (init == "RANDOM") init <- "random"

    ui$start <- "[+] AdjPIN estimation started"
    ui$complete <- "\n[+] AdjPIN estimation completed"
    ui$equalthetas <- paste(
      "Unable to estimate the AdjPIN model with equal thetas using the",
      "ECM algorithm.\nThe AdjPIN model", "will be estimated with the ",
      "standard maximum likelihood estimation.\n-----------")
    ui$unknowntype <- paste(
      "  |[=] Unrecognized argument 'initialsets', it is set to 'GE'.")
    ui$loadinitials <- paste(
      "\r  |[2] Loading initial parameter sets\t: ", nrows,
      " custom initial set(s) loaded", sep = "")
    ui$computinginitials <-  paste(
      "\r  |[1] Computing initial parameter sets\t:", nrows,
      init, "initial sets generated")
    ui$mlemethod <- paste("  |[2] Estimating the AdjPIN model \t:",
                          " Maximum-likelihood Standard Estimation", sep = "")
    ui$emmethod <- paste("  |[2] Estimating the AdjPIN model \t:",
                         " Expectation-Conditional Maximization algorithm", sep = "")
    ui$progressbar <- " of AdjPIN estimation completed"


    return(ui)
  },

  pin = function(nrows = 0, type = "") {
    ui <- list()
    ui$start <- "[+] PIN Estimation started "
    ui$loadinitials <- paste("  |[2] Loading initial parameter sets\t: ",
    nrows, " ", type, " initial set(s) loaded", sep = "")
    ui$mlemethod <- paste("  |[3] Estimating PIN model (1996) \t:",
          "Using Maximum Likelihood Estimation")
    ui$complete <- "\n[+] PIN Estimation completed"
    ui$progressbar <- " of PIN estimation completed"
    title <- "  |[1] Likelihood function factorization:"
    ui$factorization <- list(
      EHO = paste(title, "Easley, Hvidkjaer and O'Hara (2010)"),
      LK = paste(title, "Lin and Ke (2011)"),
      E = paste(title, "Ersan (2016)"),
      NONE = paste(title, "No factorization")
    )


    return(ui)
  },

  adjpindata = function(ndata = 0) {
    ui <- list()
    ui$start <- "[+] Adjpin data generation started"
    ui$complete <- "\n[+] Adjpin data generation completed"
    ui$nsimulation <- paste("  |[=] Generating", ux$sep(ndata),
                            "AdjPIN datasets")
    return(ui)
  },

  mpindata = function(ndata = 0) {
    ui <- list()
    ui$start <- "[+] MPIN data generation started"
    ui$complete <- "\n[+] MPIN data generation completed"
    ui$nsimulation <- paste("  |[=] Generating", ux$sep(ndata),
                            "MPIN datasets")
    return(ui)
  },

  vpin = function(timebarsize = 0) {
    ui <- list()

    ui$start <- "[+] VPIN Estimation started."

    ui$step1 <- "  |-[1] Checking and preparing the data..."

    ui$step2 <- paste(
      "  |-[2] Creating ", timebarsize, "-second timebars...", sep = "")

    ui$step3 <- "  |-[3] Calculating Volume Bucket Size (VBS) and Sigma(DP)..."

    ui$step4 <- paste("  |-[4] Breaking up large ", timebarsize,
                     "-second timebars' volume...", sep = "")

    ui$step5 <- paste(
      "  |-[5] Assigning ", timebarsize, "-second timebars into ",
                     "buckets...", sep = "")

    ui$step6 <- paste("  |-[6] Balancing timebars and adjusting bucket sizes",
                                  "to VBS...")

    ui$step7 <- "  |-[7] Calculating aggregate bucket data..."

    ui$step8 <- "  |-[8] Calculating VPIN vector..."

    ui$complete <- "[+] VPIN estimation completed"

    ui$aborted <- "[+] VPIN estimation aborted!"

    return(ui)

  }
)

uierrors <- list(

  mpin = function(cols = 0) {

    er <- list()
    er$wrongtype <- "The argument 'initialsets' should be a dataframe!"

    er$fn <- "MPIN estimation aborted!"
    er$failed <- paste(
      "\r[ERROR]\n\rThe MPIN maximum likelihood optimization failed!",
      "\nImpossible to evaluate the log-likelihood function at the initial\n",
      "\rparameter sets provided! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")
    er$emfailed <- paste(
      "The estimation using the Expectation-Conditional Maximization algorithm failed!",
      "\nThe ECM algorithm has failed to converge at the provided initial\n",
      "\rparameter sets! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")

    er$wronglength = paste(
      "\r[x] 'initialsets' must have a length 3J+2, for some integer",
      " J (number of layers):\n\r[-> You have supplied a dataframe ",
      "with ", cols, "variables.")

    return(er)
  },

  pin = function(grid_size, dr = 0) {
    er <- list()
    er$failed <-  paste(
      "\r[ERROR]\n\rThe PIN maximum likelihood optimization failed!",
      "\nImpossible to evaluate the log-likelihood function at the initial\n",
      "\rparameter sets provided! Please review your initial parameter sets\n",
      "\ror choose another factorization", sep = "")

    er$fn <- "PIN estimation aborted!"

    er$yzdeleted <- paste(
      dr, "initial sets have been deleted as they contained negative",
                          "values for eps.s!")
    er$eacorrected <- paste(
      dr, "initial sets have been deleted by the correction of",
      "Ersan and Alici (2016)!")

    er$displaysets <- function(fn, nrows)
      return(paste(
        "The function ", fn, " has generated ", nrows, " initial parameter",
        " sets.\n\rTo display them, either store them in a variable ",
        "or call", " (", fn, "). \n\rTo hide these messages, set the argument",
        " 'verbose' to FALSE.", sep = ""))


    return(er)
  },

  adjpin = function() {
    er <- list()
    er$failed <-  paste(
      "\r[ERROR]\n\rThe AdjPIN estimation failed!",
      "\nImpossible to evaluate the log-likelihood function at the initial\n",
      "\rparameter sets provided! Please review your initial parameter sets.",
      sep = "")
    er$unknownmethod <- paste(
      "\rThe argument \'method\' takes one of two values \"ECM\" or \"ML\".")
    er$wrongtype <- paste(
      "\rThe argument 'initialsets' must be a character string or a dataframe.")
    er$wrongsize <- paste(
      "\rThe number of variables in the dataframe 'initialsets' is not \n",
      "\rcompatible with the argument 'restricted'", sep = "")
    er$wrongvalues <- paste(
      "\rIn the dataframe 'initialsets', some probabilities are",
      "outside [0,1] or some parameters are negative.")
    er$wrongalgorithm <- paste(
    "\rThe argument 'initialsets' should be a dataframe or take on value from",
    "\n\rthe list 'GE', 'CL', and 'RANDOM'")
    er$wrongnuminit <- paste(
      "\rThe argument 'num_init' should be a positive integer."
    )
    er$wrongfact <- paste(
      "\rThe argument is a binary argument, and takes either 'TRUE' or 'FALSE!'"
    )

    er$wrongxtraclusters <- paste(
      "\rThe argument 'xtraclusters' is a non-negative integer that ",
      "can't exceed \n\rthe number of observations in the dataset!"
    )

    er$wrongrestriction <- paste(
      "The argument 'restricted' should be of type list, composed of",
      " up to four\n\rbinary values with the following names: 'theta',",
      " 'mu', 'eps', and 'd'!")

    er$fn <- "AdjPIN estimation aborted!"

    return(er)
  },

  adjpindata = function() {

    er <- list()
    er$unknown <- "An error occured during the AdjPIN data generation"
    er$notadataframe <- "The argument 'initialsets' should be a dataframe!"
    er$wrongdim <- paste(
      "Wrong dimension of the argument 'params'.",
      "It should contain 10 parameters!", sep = "")
    er$wrongvalues <- paste(
      "\rError: In the argument 'initialsets', either some probabilities ",
      "are outside [0,1]\n\ror some parameters are negative.")
    er$fn <- "AdjPIN data simulation aborted!"
    er$failed <- paste(
      "\r[ERROR]\n\rThe MPIN maximum likelihood optimization failed!",
      "\nImpossible to evaluate the log-likelihood function at the initial\n",
      "\rparameter sets provided! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")
    er$emfailed <- paste(
      "The estimation using the Expectation-Conditional Maximization algorithm failed!",
      "\nThe ECM algorithm has failed to converge at the provided initial\n",
      "\rparameter sets! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")

    return(er)
  },

  mpindata = function() {

    er <- list()
    er$unknown <- "An error occured during the MPIN data generation"
    er$notadataframe <- "The argument 'initialsets' should be a dataframe!"
    er$wrongdim <- paste(
      "Wrong dimension of the argument 'params'.",
      "It should contain 10 parameters!", sep = "")
    er$esrange <-  "A range for eps.s should be provided  when eps_ratio = 0!"
    er$epsimpossible <- paste(
      "[x] Impossible to generate eps.b and eps.s given the",
    " provided ranges and eps_ratio!\n\r Review the ranges",
    " or deactivate the eps_ratio by setting it to 0.")
    er$fn <- "MPIN data simulation aborted!"
    er$failed <- paste(
      "\r[ERROR]\n\rThe MPIN maximum likelihood optimization failed!",
      "\nImpossible to evaluate the log-likelihood function at the initial\n",
      "\rparameter sets provided! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")
    er$emfailed <- paste(
      "The estimation using the Expectation-Conditional Maximization algorithm failed!",
      "\nThe ECM algorithm has failed to converge at the provided initial\n",
      "\rparameter sets! Please review your initial parameter sets\n",
      "\rand try again!", sep = "")

    return(er)
  },

  summary = function() {

    ui <- list()

    ui$invalid <- paste("getSummary() is only valid for S4 objects",
                        "of type 'estimate.mpin.ecm'")

    return(ui)

  },

  vpin = function() {
    er <- list()
    er$failed <-  paste(
      "Error: The sample length is larger than the number of buckets in the ",
      "data!\n[=] Please choose a higher number of buckets per day 'buckets'",
      " or a smaller\n sample length 'samplength' or both.", sep = ""
    )

    er$largetimbarsize <- paste("Error: the argument 'timebarsize' shall not",
    "exceed the total duration in the dataset!")

    er$missingdata <- paste(
      "The argument 'data' is missing!, A dataset is needed",
      "to estimate VPIN!")

    er$wrongdata <- paste("The argument 'data' should be of class 'data.frame'",
    " containing at least 3 variables!")

    er$wrongargs <- paste(
      "\rError: The arguments 'timebarSize', 'buckets', and ",
      "'samplength' should be integers!", sep = "")

    er$fn <- "VPIN estimation aborted!"

    return(er)
  },

  ranges =  function(var = "", code, val = 0) {

    # error codes:
    # 1 : Non numeric range
    # 2 : probability range is not valid
    # 3 : rate range is not valid
    # 4 : Duplicated keys in list 'ranges'
    # 5 : Unrecognized keys in list 'ranges'
    qvariable <- unname(sapply(var, sQuote, simplify = TRUE))
    var <- paste(var, collapse = ", ")
    interval <- ifelse(var == "alpha", "(0,1)", "[0,1]")
    qvariable <- paste(qvariable, collapse = ", ")


    if (length(val) > 1 && val[1] == val[2]) val <- val[1]
    if (length(val) > 1)
      val <- paste("(", paste(val, collapse = ", "), ")", sep = "")

    if (is.character(val)) val <- shQuote(val)

    errors <- list(

      paste("\r[x] ", qvariable, " must be of type 'numeric':\n\r[-> ",
            "You have supplied the following value for ", qvariable, ": ",
            val, ".", sep = ""),

      paste("\r[x] ", qvariable, " must be either a real number from ",
      "or a subset of the interval ", interval, ":\n\r[-> ",
      "You have supplied the following value for ", qvariable, ": ",
      val, ".", sep = ""),

      paste("\r[x] '", qvariable, " must be either a real number from ",
      "or a valid subset of the positive reals", ":\n\r[-> ",
      "You have supplied a range for '", qvariable,
      "' that is not a valid subset of the positive reals.", sep = ""),

      paste("\r[x] Duplicated elements in the list 'ranges':\n\r[-> ",
            "You have supplied duplicate ranges for the variable(s): ",
            qvariable, ".", sep = ""),

      paste("\r[x] Unrecognized elements in the list 'ranges':\n\r[-> ",
            "You have supplied ranges for unrecognized variable(s): ",
            qvariable, ".", sep = ""),

      paste("\rError: 'theta' and 'thetap' cannot be simultaneously ",
      "equal to 1:\n\r[-> You have supplied the same value (=1) for ",
      "both 'theta', and 'thetap'.", sep = "")
    )

    return(errors[[code]])
  },

  controls = function(var, val = 0, code, keys) {

    bounds <- .default$controlbounds()
    minv <- bounds$minv
    maxv <- bounds$maxv

    if (is.character(val)) val <- shQuote(val)
    val <- paste(val, collapse = ",")
    if (length(val) > 1) val <- paste("(", val, ")", sep = "")
    varname <- paste(unname(sapply(keys[var], sQuote, simplify = TRUE)),
                     collapse = ", ")

    errors <- list(

      paste("\r[x] Unrecognized elements in the argument '...':\n\r[-> ",
            "You have supplied value for unrecognized variable(s): ",
            unname(sapply(var, sQuote, simplify = TRUE)), ".", sep = ""),

      paste("\r[x] ", varname, " must be of type 'numeric':\n\r[-> ",
            "You have supplied the following value for ", varname, ": ",
            val, ".", sep = ""),

      paste("\r[x] ", varname, " must be an integer from the set {", minv[var],
            ",...,", maxv[var], "}:\n\r[-> ", "You have supplied the following",
            " value for ", varname, ": ", val, ".", sep = ""),

      paste("\r[x] ",  varname, " must lie in the interval (", minv[var],
            ",", maxv[var], "):\n\r[-> ", "You have supplied the following",
            " value for ", varname, ": ", val, ".", sep = ""),

      paste("\r[x] ", varname, " must be either 0 or selected from (",
            minv[var], ",", maxv[var], "):\n\r[-> ", "You have supplied",
            " the following non-zero value for ", varname, ": ", val,
            ".", sep = ""),

      paste("\r[x] The elements of the range for ", varname, " ",
            "must be increasingly ranked:\n\r[-> ",
            "You have supplied a range for ", varname,
            " that is not valid.", sep = ""),

      paste("\r[x] Duplicated elements in the argument '...':\n\r[-> ",
            "You have supplied duplicate values for the variable(s): ",
            varname, ".", sep = "")
    )

    return(errors[[code]])

  },

  hyperparams = function(error, varname, val = 0, adj = FALSE) {

    bounds <- .default$hyperbounds(adj)
    minv <- bounds$minv
    maxv <- bounds$maxv

    keys <- names(.default$hyperparams(adj))
    var <- which(keys == varname)

    xset <- paste("{\"", paste(.default$criterion, collapse = "\", \"",
                               sep = ""), "\"", sep = "")
    varname <- Map(sQuote, varname)
    if (length(varname) > 1) varname <- paste(varname, collapse = ", ")
    if (is.character(val)) val <- shQuote(val)

    xmessage <- switch(

      error,
      "unrecognized" = paste(
        "\r[x] Unrecognized elements in the argument 'hyperparams':\n\r[-> ",
        "You have supplied value for unrecognized variable(s): ",
       varname, ".", sep = ""),

      "duplicate" = paste(
        "\r[x] Duplicated elements in the argument 'hyperparams':\n\r[-> ",
        "You have supplied duplicate values for the variable(s): ",
        varname, ".", sep = ""),

      "notnumeric" = paste(
        "\r[x] ", varname, " must be of type 'numeric':\n\r[-> ",
        "You have supplied a value of class ", val, " for ", varname, ".",
        sep = ""
      ),
      "notcharacter" = paste(
        "\r[x] ", varname, " must be of type 'character':\n\r[-> ",
        "You have supplied a value of class ", val, " for ", varname, ".",
        sep = ""
      ),
      "intrange" = paste(
        "\r[x] ", varname, " must be an integer from the set {", minv[var],
        ",...,", maxv[var], "}:\n\r[-> ", "You have supplied the following",
        " value for ", varname, ": ", val, ".", sep = ""
      ),
      "interval" = paste(
        "\r[x] ",  varname, " must lie in the interval [", minv[var],
        ", ", maxv[var], "]:\n\r[-> ", "You have supplied the following",
        " value for ", varname, ": ", val, ".", sep = ""
      ),
      "charrange" = paste(
        "\r[x] ", varname, " must be of type character from the set ", xset,
        "}:\n\r[-> ", "You have supplied the following",
        " value for ", varname, ": ", val, ".", sep = ""
      ),
    )

    return(xmessage)

  },

  layers = function(code, args = NULL) {

    response = switch(
      EXPR= code,
      paste("\r[Warning]\nThe number of layers derived from 'parameters' is not",
            " compatible with 'layers'.\nThe argument 'layers' will",
            " be ignored", sep = ""),
      paste("\r[x] Impossible to generate ", args$layers, " layers:\n\r[-> ",
            "The value of 'layers' exceeds the number of days/observations (",
            args$days, ")!", sep = ""),
      paste(
        "\r[x] Impossible to generate ", args$layers, " layers:\n\r[-> ",
        "You have supplied a minimum value of alpha that is too high (",
        args$minalpha, ")!", sep = ""),
      paste("\r[Warning] The maximum layers possible given that ",
            "alpha >= ", args$minalpha, " is: ", floor(1 / args$minalpha),
            ".\n", sep = "")
      )

    return(response)

  },

  detection = function() {

    ui <- list()
    ui$fn <- "MPIN layer detection aborted!"
    return(ui)

  },

  fact = function(model = NULL) {

    ui <- list()
    ui$fn <- paste("Computation of", model, "factorization aborted!")
    return(ui)

  },

  aggregation = function() {

    ui <- list()
    ui$fn <- "High-frequency data aggregation aborted!"
    return(ui)

  },

  arguments = function() {
    er <- list()

    er$initials <- function(error, class = NULL, cols = 0, rvars = 0,
                           length=0, unknown = "") {

      xmessage <- switch(
        error,
        "wrongclass" = paste(
          "\r[x] 'initialsets' must be a character string or a dataframe:",
          "\n\r[-> You have supplied an argument of class '", class, "'.",
          sep = ""),
        "wrongsize" = paste(
          "\r[x] The size of 'initialsets' must be compatible with ",
          "'restricted':\n\r[-> 'initialsets' has ", cols, " variables,",
          " while a restricted model has ", rvars, " variables",
          sep = ""),
        "wrongvalues" = paste(
          "\r[x] 'initialsets' must contain only valid values:\n\r[-> ",
          "some probabilities are outside [0,1] or some trading rates ",
          "are negative.", sep = ""),

        "wrongalgorithm" = paste(
          "\r[x] 'initialsets' must be a dataframe or a value from ",
          "{\"GE\", \"CL\", and \"RANDOM\"}:\n\r[-> 'initialsets' takes the ",
          "unrecognized value \"", unknown, "\".", sep = ""),

        "wronglength" = paste(
          "\r[x] 'initialsets' must have a length 3J+2, for some integer",
          " J (number of layers):\n\r[-> You have supplied a dataframe ",
          "with ", cols, "variables."),

        "wrongtype" = paste(
          "\r[x] 'initialsets' must be a dataframe:\n\r[-> ",
          "You have supplied an argument of class '", class, "'.",
          sep = "")
      )
    }

    er$logical <- function(x, type = NULL) {
      return(paste(
        "\r[x] '", x,
        "' must be of type 'logical' from the set {TRUE, FALSE}:",
        "\n\r[-> You have supplied an argument of type '", type, "'.",
        sep = "")
      )
    }

    er$list <- function(x, type = NULL) {
      return(paste(
        "\r[x] '", x, "' must be of type 'list':\n\r[-> ",
        "You have supplied an argument of type '", type, "'.",
        sep = "")
      )
    }

    er$character <- function(name, val = 0, xrange, type = NULL) {
      xset <- paste(
        "{", paste(shQuote(xrange), collapse = ", "), "}", sep = "")
      return(paste(
        "\r[x] '", name, "' must be of type 'character' from the set ",
        xset, ":\n\r[-> ",
        ifelse(
          !is.null(type),
          paste("You have supplied an argument of type '",
                type, "'.", sep = ""),
          paste("You have supplied the following character string: \"",
                val, "\".", sep = "")),
        sep = "")
      )
    }

    er$integer <- function(name, val = 0, bounds, type = NULL) {

      xset <- paste("{", bounds[1], ", ..., ", bounds[2], "}", sep = "")
      xmessage <- paste(
        "\r[x] '", name, "' must be an integer from the set ",
        xset, ":\n\r[-> ",
        ifelse(
          !is.null(type),
          paste("You have supplied an argument of type '",
                type, "'.", sep = ""),
          paste("You have supplied the following integer: ",
                val, ".", sep = "")),
        sep = "")
      return(xmessage)
    }

    er$numeric <- function(x, bounds, type = NULL, strict = TRUE) {

      xset <- paste("(", bounds[1], ", ", bounds[2], ")", sep = "")
      if (!strict) xset <- paste("[", bounds[1], ", ", bounds[2], ")", sep = "")
      xmessage <- paste(
        "\r[x] '", x, "' must be numeric from the set ", xset, ":\n\r[-> ",
        ifelse(
          !is.null(type),
          paste("You have supplied an argument of type '",
                type, "'.", sep = ""),
          paste("You have supplied a numeric variable not in ",
                xset, ".", sep = "")),
        sep = "")
      return(xmessage)
    }

    er$hfdata <- function(error, class = NULL, cols = 0, type1 = NULL,
                          limit = 0, failure = 0, dtypes = NULL,
                          negative = 0) {

      dtypes <- paste(
        "{", paste(shQuote(dtypes), collapse = ", "), "}", sep = "")

      xmessage <- switch(
        error,
        "wrongclass" = paste(
          "\r[x] 'data' must be of class 'dataframe':\n\r[-> ",
          "You have supplied an argument of class '", class, "'.",
          sep = ""),
        "fewvariables" = paste(
          "\r[x] 'data' must contain at least ", limit, " columns:\n\r[-> ",
          "You have supplied a dataframe with ", cols, " columns.",
          sep = ""),

        "nottimestamp" = paste(
          "\r[x] The fist column of 'data' must be of type 'character' or ",
          "'POSIXct':\n\r[-> You have supplied a first column of type '",
          type1, "'.", sep = ""),

        "notdate" = paste(
          "\r[x] The first column of 'data' must be convertible into a date:",
          "\n\r[-> The row number ", failure, " of the first column cannot be",
          " converted into a 'date' object.", sep = ""),

        "wrongdatatypes" = paste(
          "\r[x] The second, third, and fourth columns of 'data' must be ",
          "integers:\n\r[-> You have supplied columns with types ", dtypes,
          ".", sep = ""),

        "wrongdatavalues" = paste(
          "\r[x] The second, third, and fourth columns of 'data' must be ",
          "positive:\n\r[-> Some values of the second, third, or fourth ",
          "columns are non positive.", sep = "")

      )
    }

    er$adjpindata <- function(error, ntype = 0, ktype = 0,
                              size = 0, alpha = 0) {

      xmessage <- switch(
        error,
        "wrongtype" = paste(
          "\r[x] 'params' must be of type 'numeric':\n\r[-> ",
          "The value of 'params' at positition ", ntype,
          " is of type '", ktype, "'.", sep = ""),
        "wrongdim" = paste(
          "\r[x] 'params' must contain 10 values:\n\r[-> ",
          "You have supplied a numeric vector of size ", size, ".",
          sep = ""),

        "wrongalpha" = paste(
          "\r[x] The first value of 'params' (alpha) is a probability and ",
          "must belong to (0,1):\n\r[-> The first value of the provided ",
          "'params' (", alpha, ") is ", ifelse(
            alpha <= 0, "non-positive (<= 0)!", "larger than or equal to 1!"),
          sep = ""),

        "wrongprobabilities" = paste(
          "\r[x] The first four values of 'params' (probabilities) must ",
          "belong to [0,1]:\n\r[-> Some probabilities in 'params' are either",
          " negative or larger than 1!", sep = ""),

        "wrongrates" = paste(
          "\r[x] The last six values of 'params' (trading rates) must be",
          " positive integers:\n\r[-> Some trading rates in 'params' are ",
          "either negative or not integer-valued!", sep = ""),



      )
    }

    er$mpindata <- function(error, layers = 0, size = 0) {
      xlayers <- ifelse(layers == 1, " ", paste("", layers, "", sep = " "))

      xmessage <- switch(
        error,
        "wrongtype" = paste(
          "\r[x] 'params' must be of type 'numeric':\n\r[-> ",
          "The value of 'params' at positition ", ntype, " is of type '",
          ktype, "'.", sep = ""),

        "wrongdim" = paste(
          "\r[x] 'parameters' must have a length of 3J+2, for some J ",
          "(number of layers):\n\r[-> You have supplied a numeric vector of",
          " size ", size, ".", sep = ""),

        "incompatibledim" = paste(
          "\r[x] 'parameters' must have a length of 3J+2, where J is the number",
          " of layers:\n\r[-> You have supplied an argument 'layers' - ",
          layers, " -, and the size of 'params' is ", size, ". \n\r[-> ",
          "Remove one of the two arguments ('params' or 'layers'), and try",
          " again!", sep = ""),

        "wrongalpha" = paste(
          "\r[x] The first", xlayers,
          "value(s) of 'params' (alpha) must belong to (0,1):\n\r[-> The first",
          xlayers, "value(s) of the provided 'params' lie outside (0,1)!",
          sep = ""),

        "wrongprobabilities" = paste(
          "\r[x] The first ", ifelse(layers == 1, "two", 2 * layers),
          " value(s) of 'params' (probabilities) must belong to [0,1]:\n\r[->",
          " Some probabilities in 'params' are either negative or larger than",
          " 1!", sep = ""),

        "wrongrates" = paste(
          "\r[x] The last ", layers + 2, " values of 'params' (trading rates)",
          " must be positive integers:\n\r[-> Some trading rates in 'params' ",
          "are either negative or not integer-valued!", sep = ""),

        "rankedmu" <- paste(
          "\r[x] The values of 'params' in positions ", 2 * layers + 2, ":",
          3 * layers,
          " (informed trading rates muj) must be increasingly ranked:\n\r[-> ",
          "The informed trading rates in 'params' (muj) are not increasingly ",
          "ranked!", sep = "")



      )
    }

    er$restricted <- function(error, unknown = NULL, nonbinary = NULL) {

      qvariable <- unname(sapply(unknown, sQuote, simplify = TRUE))
      qvariable <- paste(qvariable, collapse = ", ")

      xmessage <- switch(
        error,
        "unrecognized" = paste(
          "\r[x] Unrecognized elements in the list 'restricted':\n\r[-> ",
          "You have supplied values for unrecognized variable(s): ",
                               qvariable, ".", sep = ""),
        "nonbinary" = paste(
          "\r[x] All variables in 'restricted' must take logical values:",
          "\n\r[-> You have supplied a non logical value for the variable '",
          nonbinary, "'.", sep = "")
      )
    }

    er$tdata <- function(error, class = NULL, cols = 0,
                         dtypes = NULL, limit = 0) {

      dtypes <- paste("{",
                      paste(shQuote(dtypes), collapse = ", "),
                      "}", sep = "")

      xmessage <- switch(
        error,
        "wrongclass" = paste(
          "\r[x] 'data' must be of class 'dataframe':\n\r[-> ",
          "You have supplied an argument of class '", class, "'.",
          sep = ""),
        "fewvariables" = paste(
          "\r[x] 'data' must contain at least ", limit, " columns:\n\r[-> ",
          "You have supplied a dataframe with ", cols, " columns.",
          sep = ""),
        "wrongdatatypes" = paste(
          "\r[x] The first two columns of 'data' must be integers:\n\r[-> ",
          "You have supplied columns with types ", dtypes, ".", sep = ""),

        "wrongdatavalues" = paste(
          "\r[x] The first two columns of 'data' must be positive:\n\r[-> ",
          "Some values of the first two columns are non-positive.", sep = "")

      )

    }

    er$notfound <- function(varname, msg) {
      return(paste(
        "[x] '", varname,
        "' is missing or contains a non-existent variable:\n\r[-> ",
        gsub("[\n]", "", msg), sep = ""))
    }

    er$unknown <- function(u) {
      qvariable <- unname(sapply(u, sQuote, simplify = TRUE))
      qvariable <- paste(qvariable, collapse = ", ")
      return(paste(
        "\r[x] Unrecognized elements in the function call:\n\r[-> ",
        "You have supplied values for unrecognized argument(s): ",
        qvariable, ".", sep = ""))
    }


    er$compatibility <- function(model, n, cl) {
      return(paste(
        "\r[x] 'xtraclusters' must take a valid value:\n\r[-> ",
        ifelse(model == "mpin",
          paste("The total number of clusters in initials_mpin() is ",
          "xtraclusters + layers + 1 \n\r(", cl, ") should not exceed ",
          "the total number of data observations (", n, ").",
          sep = ""),
          paste("The total number of clusters in initials_adjpin() is",
          "xtraclusters + 6 \n\r(", cl, ") should not exceed the total",
          "number of data observations (", n, ").", sep = ""))
      ))
    }


    er$mpinfn <- "MPIN estimation aborted!"
    er$adjpinfn <- "AdjPIN estimation aborted!"
    er$adjpininitfn <- "AdjPIN generation of initial sets aborted!"
    er$mpininitfn <- "MPIN generation of initial sets aborted!"

    return(er)
  }

)


uiconflicts <- list(

  add = function(conflicts, conflict, details = c()) {

    conflict_msgs <- list(
      paste("[i] The eps_ratio condition is inactive. \neps.b and eps.s are",
            " uniformly generated from their respective ranges.", sep = ""),
      paste("[i] The confidence condition is inactive and the mu_ratio (",
            details[1], ") is used.", sep = ""),
      paste("\r[!] The range of ", details[1], " has dimension larger than 2.",
            " Only the first two elements are used.", sep = ""),
      paste("[!] The mu_ratio provided (", details[1], ") is too large for ",
            "the mu_range (", details[2], ", ", details[3], "). It is set to ",
            details[4], ".", sep = ""),
      paste("[!] ", details[1], " value(s) of mu lie(s) outside the mu_range",
            " provided (", details[2], ", ", details[3], ").\n\r",
            "To resolve it, either set confidence = 0 or increase the",
            " mu_range.", sep = "")
    )

    conflicts$ids <- c(conflicts$ids, conflict)
    conflicts$msgs <- c(conflicts$msgs, conflict_msgs[[conflict]])

    conflicts$msgs <- conflicts$msgs[order(conflicts$ids)]
    conflicts$ids <- sort(conflicts$ids)

    return(conflicts)

  }

)


uiclasses <- list(

  pin = function(object) {

    ui <- list()

    badge_txt <- " PIN model "
    ui$badge <-  paste(
      "\n", ux$color(fg = 37, bg = 41, x = badge_txt), " ", sep = "")

    algorithms <- list(
      `YZ*` = paste("Initial parameter sets\t: Yan and Zhang (2012) ",
                  "as improved by Ersan and Alici (2016).\n",
                  "Boundary optimal points are integrated and ",
                  "unrealistic initial points are excluded.",
                  sep = ""),
      YZ = "Initial parameter sets\t: Yan and Zhang (2012)",
      GWJ = "Initial parameter sets\t: Gan, Wei and Johnstone (2015)",
      EA = "Initial parameter sets\t: Ersan and Alici (2016)",
      CUSTOM = "Initial parameter sets\t: Custom initial sets"
    )

    factorizations <- list(
      EHO = "Likelihood factorization: Easley, Hvidkjaer and O'Hara (2010)",
      LK = "Likelihood factorization: Lin and Ke (2011)",
      E = "Likelihood factorization: Ersan (2016)",
      NONE = "Likelihood factorization: No factorization."
    )

    ui$line <- "----------------------------------"

    ui$algorithm <- algorithms[[object@algorithm]]

    ui$factorization <- factorizations[[object@factorization]]


    ui$outcome <- if (object@success)
      "PIN estimation completed successfully" else
      "PIN estimation failed"

    ui$initialsets <- paste(
      nrow(object@initialsets), "initial set(s) are used in the estimation",
      "\nType object@initialsets to see the initial parameter sets used")

    ui$failedsets <- paste("[Warning] Estimation has failed for",
                           nrow(object@initialsets) - object@convergent.sets,
                           "initial parameter set(s)!")

    ui$error <- paste("\n\n", uierrors$pin()$failed, "\n\n")


    ui$tablevars <- c(
      "alpha", "delta", "mu", "eps.b", "eps.s", "----", "Likelihood", "PIN"
    )
    ui$tablevalues <- c(
      ux$round1(object@parameters[1:2]),
      ux$round2(object@parameters[3:5]), "",
      ux$parentheses(object@likelihood),
      ux$round1(object@pin)
    )
    ui$tableheaders <- c("Variables ", "Estimates  ")


    ui$runningtime <- ux$showtime(object@runningtime)

    return(ui)
  },

  mpin = function(object) {

    ui <- list()

    is_ecm <- (object@method == "ECM")

    is_optimal <- (object@method == "ECM" && object@optimal != 0)


    # if the model is optimal for ECM, then fetch the function
    # .mpin@optimaldetails to get the total number of initialsets,
    # the total running time, as well as a main summary of the
    # models.
    if (is_optimal) {

      details <- .xmpin$optimaldetails(object)
      nmodels <- length(object@models)
      ui$eminitialsets <- paste(
        details$rinit, "initial set(s) are used for all", nmodels,
        "estimations")
      ui$summary <- details$tab
      object@runningtime <- details$rtime

      ui$tablecaption <- paste(
        "Summary of", nmodels, "MPIN estimations by ECM algorithm")

      txt <- " Optimal Estimation "
      ui$header <- paste(" ", ux$color(fg = 37, bg = 44, x = txt), sep = "")

    } else {

      ui$eminitialsets <- paste(
        nrow(object@initialsets),
        "initial set(s) are used for the 'current' estimation",
        "\nType object@initialsets to see the initial parameter sets used.")

      txt <- " Regular Estimation "
      ui$header <- paste(" ", ux$color(fg = 29, bg = 47, x = txt), sep = "")

    }

    badge_txt <- " MPIN model "

    ui$badge <-  paste(
      "\n", ux$color(fg = 37, bg = 44, x = badge_txt), sep = "")

    parallel_txt <- " Sequential "
    if (.hasSlot(object, "parallel") && object@parallel)
      parallel_txt <- " Parallel "

    is_active <- 42
    if (object@parallel && (nrow(object@initialsets) < .default$parallel_cap()))
      is_active <- 41

    ui$parallel <-  paste(" ", ux$color(fg = 37, bg = is_active,
                                   x = parallel_txt), " ", sep = "")

    ui$algorithm <- paste("Initial parameter sets\t: Ersan (2016),",
                          "Ersan and Alici (2016)")

    ui$factorization <- "Likelihood factorization: Ersan (2016)"

    ui$method <- paste("Estimation Algorithm \t:", ifelse(!is_ecm,
    "Maximum Likelihood Estimation", "Expectation Conditional Maximization"))

    detectalgorithms <- list(ECM = "using Ghachem and Ersan (2022) [ECM]",
                             E = "using Ersan (2016)",
                             EG = "using Ersan and Ghachem (2022a)",
                             USER = "provided by the user",
                             INITIALSETS = "from the provided initial sets"
                             )

    criteria <- list(
             BIC = "Bayes Information Criterion (BIC)",
             AIC = "Akaike Information Criterion (AIC)",
             AWE = "Approximate Weight of Evidence (AWE)"
    )

    ui$layers <- paste(ifelse(object@detection == "USER",
    "Info. layers in the data:", "Info. layers detected\t:"),
                       detectalgorithms[[object@detection]])


    ui$line <- "----------------------------------"

    ui$outcome <- if (object@success)
      "MPIN estimation completed successfully" else
      "MPIN estimation failed"

    ui$initialsets <- paste(
      nrow(object@initialsets),
      "initial set(s) are used in the estimation",
      "\nType object@initialsets to see the initial parameter sets used")

    ui$failedsets <- paste("[Warning] Estimation has failed for",
                           nrow(object@initialsets) - object@convergent.sets,
                           "initial parameter set(s)!")
    ui$criterion <- if (is_ecm)
      paste("Selection criterion \t:", criteria[[object@criterion]])

    ui$emfunctions <- if (is_optimal) {
      paste(
        "Type object@models for the estimation results for all models.",
        "\nType getSummary(object) for a summary of estimates for all models."
      )
    }

    ui$error <- paste("\n\n", uierrors$mpin()$failed, "\n")

    ui$emerror <- paste("\n\n", uierrors$mpin()$emfailed, "\n")

    ui$runningtime <- ux$showtime(object@runningtime)

    xtablevars <- c("alpha", "delta", "mu", "eps.b", "eps.s", "----",
                    "Likelihood", "mpin(j)", "mpin")

    if (is_ecm) xtablevars <- c(xtablevars, "----", "AIC | BIC | AWE")

    ui$tablevars <- xtablevars

    xtablevalues <-  c(
      lapply(object@parameters[1:2], ux$round1),
      lapply(object@parameters[3:5], ux$round2), "",
      ux$parentheses(ux$round3(object@likelihood)),
      list(ux$round1(object@mpinJ)),
      ux$round1(object@mpin)
    )

    if (is_ecm)
      xtablevalues <- c(
        xtablevalues, "",
        list(ux$round2(c(object@AIC, object@BIC, object@AWE)))
    )

    ui$tablevalues <- xtablevalues

    ui$tableheaders <- c("Variables ", "Estimates  ")

    return(ui)
  },

  adjpin = function(object) {

    ui <- list()

    badge_txt <- " AdjPIN model "

    ui$badge <-  paste(
      "\n", ux$color(fg = 37, bg = 45, x = badge_txt), " ", sep = "")

    initparam <- list(GE = "Ersan and Ghachem (2022b)",
                      CL = "Cheng and Lai (2021)",
                      RANDOM = "Random initial sets",
                      CUSTOM = "Custom initial sets")

    ui$algorithm <- paste("Initial parameter sets\t:",
                          initparam[[object@algorithm]])


    factorizations <- list(GE = "Ersan and Ghachem (2022b)",
                           NONE = "No factorization used")

    ui$factorization <- paste(
      "Likelihood factorization:", factorizations[[object@factorization]]
      )

    ui$method <- paste("Estimation Algorithm \t:",
                       ifelse(object@method == "ML",
                              "Maximum Likelihood Estimation",
                              "Expectation-Conditional Maximization"))

    ui$line <- "----------------------------------"

    ui$outcome <- if (object@success)
      "AdjPIN estimation completed successfully" else
      "AdjPIN estimation failed"

    ui$initialsets <- paste(
      nrow(object@initialsets), "initial set(s) are used in the estimation",
      "\nType object@initialsets to see the initial parameter sets used")

    ui$failedsets <- paste("[Warning] Estimation has failed for",
                           nrow(object@initialsets) - object@convergent.sets,
                           "initial parameter set(s)!")

    allrestrictions <-
      c("theta = theta'", "eps.b = eps.s", "mu.b = mu.s", "d.b = d.s")
    # sort the list of model restrictions
    restrictions <- unlist(object@restrictions[c("theta", "eps", "mu", "d")])
    current_restrictions <- paste(
      allrestrictions[restrictions], collapse = " & ")
    if (sum(unlist(object@restrictions)) == 0)
      current_restrictions <- "Unrestricted model"

    ui$restrictions <- paste("Model Restrictions \t:", current_restrictions)


    ui$error <- paste("\n\n", uierrors$adjpin()$failed, "\n")

    ui$runningtime <- ux$showtime(object@runningtime)

    ui$tablevars <- c("alpha", "delta", "theta", "theta'", "----", "eps.b",
      "eps.s", "mu.b", "mu.s", "d.b", "d.s", "----",
      "Likelihood", "adjPIN", "PSOS"
    )

    ui$tableparams <- c(lapply(object@parameters[1:4], ux$round1), "",
                        lapply(object@parameters[5:10], ux$round2), "",
                        ux$parentheses(object@likelihood),
                        ux$round1(object@adjpin),
                        ux$round1(object@psos)
    )

    ui$tableheaders <- c("Variables  ", "Estimates     ")

    return(ui)
  },

  dataset = function(object) {

    ui <- list()

    badge_txt <- " Data simulation "

    ui$badge <-  paste(
      "\n", ux$color(fg = 37, bg = 40, x = badge_txt), " ", sep = "")

    is_series <- (.hasSlot(object, "series"))

    if (is_series)
      ui$badge <- paste(ui$badge, "\n")



    is_mpin <- (object@model == "MPIN")

    ui$model <- paste("Simulation model \t:",
                       ifelse(object@model == "MPIN", "MPIN model",
                               "AdjPIN model"))

    ui$line <- "----------------------------------"

    ui$outcome <- ifelse(is_series,
                          "Simulated data successfully generated",
                          "Data series successfully generated")

    ui$days <- paste("Number of trading days\t:",
                     ux$sep(object@days), "days")

    ui$layers <- paste("Number of layers\t:", ifelse(
      max(object@layers) > min(object@layers),
      paste("random from 1 to", max(object@layers)),
      paste(min(object@layers), "layer(s)")
      ))

    ui$getdata <- ifelse(
      is_series, "Type object@datasets to access the list of dataset objects",
      "Type object@data to get the simulated data")

    allrestrictions <- c(
      "theta = theta'", "eps.b = eps.s", "mu.b = mu.s", "d.b = d.s")
    # sort the list of model restrictions
    restrictions <- unlist(object@restrictions[c("theta", "eps", "mu", "d")])
    current_restrictions <- paste(
      allrestrictions[restrictions], collapse = " & ")

    if (sum(unlist(object@restrictions)) == 0)
      current_restrictions <- "Unrestricted model"

    ui$restrictions <- if (object@model == "adjPIN")
      paste("Model Restrictions \t:", current_restrictions)

    ui$runningtime <- ux$showtime(object@runningtime)

    ## data.series specific ui
    dataseries_warnings <- c(
      "[info] The eps_ratio condition is inactive.",
      "[info] The confidence interval condition is inactive.",
      "[Warning] Some ranges have dimension larger than 2!",
      paste(
        "[Warning] Some datasets have a conflict between mu",
        "range and mu_ratio!"
      ),
      paste(
        "[Warning] Some datasets suffer from a conflict between",
        "confidence interval and mu range!"
      )
    )

    ui$datasets <- if (is_series)
      paste("Number of datasets\t:", object@series, "datasets")

    ui$warnings <- if (is_series)
      paste(dataseries_warnings[object@warnings], collapse = "\n")

    if (!is_series) {
      tablevars <- c(
        "alpha", "delta", "theta", "theta'", "----", "eps.b", "eps.s", "mu.b",
        "mu.s", "d.b", "d.s", "----", "Likelihood", "adjPIN", "PSOS"
      )

      if (is_mpin)
        tablevars <- c("alpha", "delta", "mu", "eps.b", "eps.s", "----",
                                 "Likelihood", "mpin")

      ui$tablevars <- tablevars

      tablevalues <- NULL

      if (is_mpin) {

        # Theoretical values
        tablevalues <- c(lapply(object@theoreticals[1:2], ux$round1),
                         lapply(object@theoreticals[3:5], ux$round2),
                         "", "-", "-")
        # Empirical values
        tablevalues <- cbind(
          tablevalues,
          c(lapply(object@empiricals[1:2], ux$round1),
            lapply(object@empiricals[3:5], ux$round2),
            "", ux$parentheses(ux$round3(object@likelihood)),
            ux$round1(object@emp.pin)))

        # Aggregate values
        tablevalues <- cbind(
          tablevalues,
          c(lapply(object@aggregates[1:2], ux$round1),
            lapply(object@aggregates[3:5], ux$round2), "",
            ux$parentheses(ux$round3(object@likelihood)),
            ux$round1(object@emp.pin)))

      } else {

        tablevalues <- cbind(
          c(lapply(object@theoreticals[1:4], ux$round1), "",
            lapply(object@theoreticals[5:10], ux$round2), "", "",
            lapply(object@theoreticals[11:12], ux$round3)
          ),
          c(lapply(object@empiricals[1:4], ux$round1), "",
            lapply(object@empiricals[5:10], ux$round2), "",
            ux$parentheses(ux$round3(object@likelihood)),
            lapply(object@empiricals[11:12], ux$round3)
          )
        )
      }

      ui$tablevalues <- tablevalues

      headers <- c("Variables  ", "Theoretical.  ", "Empirical.  ")
      if (is_mpin) headers <- c(headers, "Aggregates.  ")


      ui$tableheaders <- headers
    }
    return(ui)
  },

  emsummary = function(object) {

    ui <- list()

    models <- attr(object, "models")
    xmodels <- length(models)

    ui$line <- "----------------------------------"
    ui$tablecaption <- paste(
      "Summary of MPIN estimations by the ECM algorithm : [",
      xmodels, " models]", sep = "")

    xsummary <- NULL

    for (i in seq_len(xmodels)) {
      xmodel <- models[[i]]
      xsummary <- rbind(xsummary, c(
        round(c(i, xmodel@layers, xmodel@mpin, xmodel@likelihood), 3),
        round(c(xmodel@AIC, xmodel@BIC, xmodel@AWE), 1)))
    }

    ui$tablevalues <- xsummary

    ui$tableheaders <- c("layers", "em.layers", "MPIN", "Likelihood",
      "AIC", "BIC", "AWE")

    ui$tablerows <- c(paste("Model[", seq_len(xmodels), "]", sep = ""))

    ui$nothing <- "A single model has been estimated, so nothing to summarize!"

    return(ui)

  },

  getmodels = function() {

    ui <- list()

    ui$nomodels <- "A single model has been estimated, so no models to show!"

    return(ui)
  },

  vpin = function(object) {

    ui <- list()

    badge_txt <- " VPIN model "

    ui$badge <-  paste(
      "\n", ux$color(fg = 37, bg = 46, x = badge_txt), " ", sep = "")

    ui$line <- "----------------------------------"

    ui$outcome <- if (object@success)
      "VPIN estimation completed successfully" else
      "VPIN estimation failed"

    ui$vpinfunctions <- paste(
      "Type object@vpin to access the VPIN vector.",
      "\nType object@bucketdata to access data used to construct ",
      "the VPIN vector.",
      "\nType object@dailyvpin to access the daily VPIN vectors.")

    vpinsummary <- unclass(summary(object@vpin))
    vpinnames <- names(vpinsummary)
    vpinsummary <- data.frame(t(ux$round3(vpinsummary)))
    colnames(vpinsummary) <- vpinnames

    ui$vpinsummary <- vpinsummary
    ui$summarycaption <- "\r[+] VPIN descriptive statistics"

    vpinparams <- data.frame(matrix(object@parameters, ncol = 5))
    colnames(vpinparams) <- names(object@parameters)
    rownames(vpinparams) <- NULL

    ui$vpinparams <- vpinparams
    ui$paramscaption <- "\r[+] VPIN parameters"

    ui$error <- paste("\n\n", uierrors$vpin()$failed, "\n\n")

    ui$runningtime <- ux$showtime(object@runningtime)

    return(ui)

  }

)

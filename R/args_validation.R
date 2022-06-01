## - | FILE  HEADER |
##
## Script name:
##    args_validation.R
##
## Purpose of script:
##    implements various functions to perform the validation of the various
##    arguments of the functions contained in the package, it contains two
##    list of functions: one called 'check' that checks for the existence,
##    and validity of arguments (ranges, controls, args, hyperparams, layers,
##    xtraclusters, and args), and a list called 'validation' that is performs
##    the validation of invidual arguments in terms of type, and range, and it
##    is called by the function .xcheck$onearg (in turn called by .xcheck$args).
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



.xcheck <- list(

  hyperparams = function(hyperparams, days, adj = FALSE) {
  # computes a set of hyperparameters for the ECM estimation
  #
  # Args:
  #   hyperparams : a list of hyperparameters entered by user
  #   days        : the number of days in the dataset
  #   adj         : takes TRUE when the call of the function comes
  #               : from the function adjpin()
  #
  # Returns:
  #   a list of valid hyperparameters

    default <- .default$hyperparams(adj)
    defaultbounds <- .default$hyperbounds(adj)

    minv <- defaultbounds$minv
    maxv <- defaultbounds$maxv

    # Before checking the hyperparameters, let's check if hyperparams has
    # unrecognized or duplicated keys
    rkeys <- names(hyperparams)
    keys <- c("criterion", "minalpha", "tolerance", "maxeval",
              "maxlayers", "maxinit")
    if (adj) keys <- c("tolerance", "maxeval")
    unknown <- setdiff(rkeys, keys)
    if (length(unknown) > 0)
      return(list(off = TRUE, error = uierrors$hyperparams(
        error = "unrecognized", varname = unknown)))


    # Check if 'hyperparams' does not have duplicates (error code: 7)
    # -------------------------------------------------------------
    iserror <- any(duplicated(rkeys) == TRUE)
    isvars <- rkeys[duplicated(rkeys)]
    if (iserror)
      return(list(off = TRUE, error = uierrors$hyperparams(
        error = "duplicate", varname = keys[which(keys == isvars)])))


    # If the list 'control' is empty, return default values
    keys <- names(default)
    if (length(hyperparams) == 0)
      return(list(off = FALSE, hyperparams = default))

    # If the list 'hyperparams' is not empty.

    # Set the new_hps values at the default value, and replace
    # them with the values in the list 'hyperparams' if corresponding values
    # exist, and are valid
    new_hps <- default

    # Check if there any unrecognized keys
    # -------------------------------------------------------------
    rkeys <- names(hyperparams)
    common <- length(intersect(rkeys, keys))


    # Check that all hyperparams are numeric (except criterion)
    # -------------------------------------------------------------
    temphps <- hyperparams

    if (!is.null(temphps$criterion)) temphps$criterion <- NULL

    if (length(temphps) > 0) {

      is_numeric <- vapply(temphps, is.numeric, logical(1))

      if (prod(is_numeric) == 0) {
        # which key(s) is not numeric

        wkey <- which(is_numeric == FALSE)[1]
        var <- which(keys == names(temphps)[[wkey]])
        addin <- 0 + !adj
        return(list(off = TRUE, error = uierrors$hyperparams(
          error = "notnumeric",
          varname = keys[var], val = class(hyperparams[[addin + wkey]]))))
      }

    }

    if (!is.null(hyperparams$criterion) && !is.character(hyperparams$criterion))
      return(list(off = TRUE, error = uierrors$hyperparams(
        error = "notcharacter",
        varname = "criterion", val = class(hyperparams$criterion))))

    names(hyperparams) <- rkeys


    # Go over the keys of the list hyperparams as found in the default
    # list, check they satisfy the validity conditions
    # -------------------------------------------------------------
    common <- sort(match(rkeys, keys))

    for (k in common) {

      iserror <- 0
      key <- keys[[k]]

      if (!adj && k == 1) {
        if (!(toupper(hyperparams$criterion) %in% .default$criterion)) {
          return(list(off = TRUE, error = uierrors$hyperparams(
            error = "charrange", varname = key, val = hyperparams$criterion)))

          next
        }
      }

      hp <- unname(unlist(hyperparams[key]))
      canbezero <- (k == 2)

      # minalpha can be set to zero
      # Check that the values are within the min-max bounds
      # -----------------------------------------------------------------
      in_bounds <- ux$is_sub(hp, c(minv[k], maxv[k]))
      int_bounds <- ux$is.integer(hp) & in_bounds


      if (adj) {

        if (k == 1 && !in_bounds)
          return(list(off = TRUE, error = uierrors$hyperparams(
            error = "interval", varname = key, val = hp)))

        if (k == 2 && !int_bounds)
          return(list(off = TRUE, error = uierrors$hyperparams(
            error = "intrange", varname = key, val = hp)))

      } else {

        # if the variables 'minalpha' and 'tolerance' then it is not an
        # integer, we just need to check the bounds (in_bounds)
        # The error code is 3 when 'minalpha', and is equal to 4 when
        # 'tolerance' because tolerance is bounded away from zero
        if (k %in% c(2, 3) && !in_bounds)
          return(list(off = TRUE, error = uierrors$hyperparams(
            error = "interval", varname = key, val = hp)))

        # if the variables 'maxeval', 'maxlayers', and 'maxinit' then it
        # must be an integer, so we need to check the bounds (int_bounds)
        if (k %in% c(4, 5, 6) && !int_bounds)
          return(list(off = TRUE, error = uierrors$hyperparams(
            error = "intrange", varname = key, val = hp)))


      }


      # If no error, assign the value of 'hp' to the list new_hps
      # -----------------------------------------------------------------
      new_hps[key] <- list(hp)

    }


    return(list(off = FALSE, hyperparams = new_hps))

  },

  existence = function(d, err) {
    # Check whether the object 'object' exists
    #
    # Args:
    #   object: an R object (mostly a dataframe in our case)
    #
    # Returns:
    #   NULL if the object exists, otherwise an error message.
    errors <- uierrors$arguments()

    for (i in seq_len(length(d))) {
      tryCatch({
        suppressWarnings(get0(d[i]))
        xlist <- list(off = FALSE, error = "")
      }, error = function(cond) {
        msg <- unlist(strsplit(toString(cond), ": "))[[2]]
        xlist <- list(off = TRUE, error = errors$notfound(d[i], msg))
        ux$stopnow(xlist$off, m = xlist$error, err)
      })
    }

  },

  ranges = function(ranges, adj = FALSE) {
    # checks the provided ranges by the user as argument in the functions
    # generatedata_mpin() and generatedata_adjpin(), and returns the ranges
    # to be used by these functions
    #
    # Args:
    #   ranges  : a list of ranges for the different model parameters
    #   adj     : if TRUE, the model in question is Duarte and Young (2009),
    #           : otherwise, it is MPIN of Ersan (2016)
    #
    # Returns:
    #   NULL if one of more ranges are not valid, otherwise, a list of valid
    #   ranges for the parameters.

    # load default ranges, and rangebounds using the function .default
    # ------------------------------------------------------------------
    model <- ifelse(adj, "adjpin", "mpin")

    default <- .default$ranges(model = model)
    defaultbounds <- .default$rangebounds(model = model)

    minv <- defaultbounds$minv
    maxv <- defaultbounds$maxv
    probk <- defaultbounds$probk

    keys <- names(default)
    new_ranges <- list()

    # If the list 'ranges' is empty, return default values
    if (length(ranges) == 0) return(list(off = FALSE, ranges = default))

    # If the list 'ranges' is not empty.

    # Set the new_ranges values at the default value, and replace
    # them with the values in the list 'ranges' if corresponding values
    # exist, and are valid
    new_ranges <- default

    rkeys <- names(ranges)

    iserror <- 0

    # Check that all control variables are numeric (error code: 1)
    # -------------------------------------------------------------
    is_numeric <- vapply(ranges, is.numeric, logical(1))
    if (prod(is_numeric) == 0) {
      # which key(s) is not numeric
      wkey <- which(is_numeric == FALSE)[1]
      return(list(off = TRUE, error = uierrors$ranges(
        var = names(ranges)[[wkey]], val = ranges[[wkey]], code = 1)))
    }

    is_numeric <- prod(vapply(ranges, is.numeric, logical(1)))
    if (!is_numeric) return(list(off = TRUE, error = uierrors$ranges(code = 1)))

    allerrors <- list()
    allvars <- list()

    # Check if 'ranges' does not have duplicates (error code: 4)
    # -------------------------------------------------------------
    iserror <- any(duplicated(rkeys) == TRUE) * 4
    isvars <- rkeys[duplicated(rkeys)]
    allerrors <- c(allerrors, list(iserror))
    allvars <- c(allvars, list(isvars))

    # Check if there any unrecognized keys (error code: 5)
    # -------------------------------------------------------------
    common <- length(intersect(rkeys, keys))

    iserror <- iserror + (iserror == 0) * (length(rkeys) != common) * 5
    isvars <- setdiff(rkeys, keys)
    allerrors <- c(allerrors, list(iserror))
    allvars <- c(allvars, list(isvars))

    # Check that not theta and thetap are both one (error code: 6)
    # -------------------------------------------------------------
    if (!is.null(ranges$theta) & !is.null(ranges$thetap)) {
      iserror <-  iserror + (iserror == 0) *
        (min(ranges$theta) == 1 & min(ranges$thetap) == 1) * 6
      allerrors <- c(allerrors, list(iserror))
      allvars <- c(allvars, list(NULL))
    }

    founderrors <- unlist(allerrors)
    if (length(which(founderrors > 0)) > 0) {
      iserror <- allerrors[[which(founderrors > 0)[1]]]
      isvars <- allvars[[which(founderrors > 0)[1]]]
      return(list(off = TRUE, error = uierrors$ranges(isvars, code = iserror)))
    }


    ranges <- lapply(ranges, function(x)
      if (length(x) == 1) c(x[1], x[1]) else x[1:2])
    names(ranges) <- rkeys


    # Go over the keys of the list ranges as found in the default
    # list, check they satisfy the validity conditions
    # -------------------------------------------------------------
    common <- sort(match(rkeys, keys))

    for (k in common) {

      iserror <- 0
      key <- keys[[k]]

      isprob <- ifelse(k <= probk, TRUE, FALSE)

      rng <- unname(unlist(ranges[key]))

      # Check that all ranges values are within the min-max bounds
      # The ranges of alpha (k ==1) is (0, 1); otherwise, the ranges of
      # other variables should be are of the form [minv, maxv]
      # -----------------------------------------------------------------
      if (k == 1 && rng[1] == rng[2])
        in_bounds <- (ux$strict_sub(rng, c(minv[k], maxv[k])))
      if ((k == 1 && rng[1] < rng[2]) | k != 1)
        in_bounds <- (ux$is_sub(rng, c(minv[k], maxv[k])))

      # If is_bounds == FALSE, then return the error
      # The parameters are probabilities when isprob = TRUE (error code: 2)
      # The parameters are trade rates when isprob = FALSE (error code: 3)
      # -----------------------------------------------------------------
      iserror <- (!in_bounds) * isprob * 2 + (!in_bounds) * (!isprob) * 3

      if (rng[1] > rng[2]) {
        iserror <- isprob * 2 + (!isprob) * 3
      }

      new_ranges[key] <- list(rng)

      if (iserror != 0) return(list(
        off = TRUE,
        error = uierrors$ranges(var = keys[[k]], code = iserror, val = rng)))

    }

    return(list(off = FALSE, ranges = new_ranges))
  },

  controls = function(control, vargs) {
    # checks the provided controls by the user as argument in the function
    # generatedata_mpin(), and, if they are valid, returns the controls
    # to be used by the function.
    #
    # Args:
    #   control : a list of numbers controlling the relationship between
    #             the different model parameters in MPIN of Ersan(2016)
    #   vargs   : list of arguments in ... for the function generatedata_mpin()
    #
    # Returns:
    #   NULL if one of more controls are not valid, otherwise, a list of valid
    #   controls.

    # load default controls, and controlbounds using the function .default
    # The keys' order:  eps_ratio, mu_ratio, maxlayers, confidence, overlap
    # ------------------------------------------------------------------
    default <- .default$controls()
    defaultbounds <- .default$controlbounds()

    minv <- defaultbounds$minv
    maxv <- defaultbounds$maxv

    # Before checking the controls, let's check if vargs has unrecognized
    # or duplicated keys
    rkeys <- names(vargs)
    keys <- c("confidence", "maxlayers", "eps_ratio", "mu_ratio")
    unknown <- setdiff(rkeys, keys)
    if (length(unknown) > 0)
      return(list(off = TRUE, error = uierrors$controls(
        var = unknown, code = 1, keys = keys)))


    # Check if 'ranges' does not have duplicates (error code: 7)
    # -------------------------------------------------------------
    iserror <- any(duplicated(rkeys) == TRUE) * 7
    isvars <- rkeys[duplicated(rkeys)]
    if (iserror != 0)
      return(list(off = TRUE, error = uierrors$controls(
        var = which(keys == isvars), code = iserror, keys = keys)))




    # If the list 'control' is empty, return default values
    keys <- names(default)


    if (length(control) == 0) return(list(off = FALSE, controls = default))

    # If the list 'control' is not empty.

    # Set the new_control values at the default value, and replace
    # them with the values in the list 'control' if corresponding values
    # exist, and are valid
    new_control <- default


    # Check if there any unrecognized keys
    # -------------------------------------------------------------
    rkeys <- names(control)
    common <- length(intersect(rkeys, keys))

    # Check that all control variables are numeric
    # -------------------------------------------------------------
    is_numeric <- vapply(control, is.numeric, logical(1))
    if (prod(is_numeric) == 0) {
      # which key(s) is not numeric
      wkey <- which(is_numeric == FALSE)[1]
      var <- which(keys == names(control)[[wkey]])
      return(list(off = TRUE, error = uierrors$controls(
        var = var, val = control[[wkey]], code = 2, keys = keys)))
    }

    control <- lapply(control, function(x)
      if (length(x) == 1) c(x[1], x[1]) else x[1:2])
    names(control) <- rkeys


    # Go over the keys of the list control as found in the default
    # list, check they satisfy the validity conditions
    # -------------------------------------------------------------
    common <- sort(match(rkeys, keys))

    for (k in common) {

      iserror <- 0
      key <- keys[[k]]

      ctrl <- unname(unlist(control[key]))
      canbezero <- (k == 1 | k == 4)
      strict <- (k == 2 | k == 4)

      # Confidence level or eps.ratio can be deactivated when set to zero
      # Check that the values are within the min-max bounds
      # -----------------------------------------------------------------
      if (!(canbezero & (ctrl[1] == 0))) {

        in_bounds <- strict * (ux$strict_sub(ctrl, c(minv[k], maxv[k])))
        in_bounds <- in_bounds +
          (!strict) * (ux$is_sub(ctrl, c(minv[k], maxv[k])))
        int_bounds <- in_bounds * (ctrl[1] == floor(ctrl[1]))

        # If is_bounds == FALSE, then return the error
        # Check that the argument 'maxlayers' is an integer
        # -----------------------------------------------------------------
        iserror <- (!int_bounds) * (k == 3) * 3
        iserror <- iserror + (iserror == 0) * (!in_bounds) * (4 + canbezero)

        # If iserror is zero, check that the elements of the argument
        # 'eps.ratio' are increasing
        # -----------------------------------------------------------------
        iserror <- iserror + (iserror == 0) * (k == 1) *
          (ctrl[2] < ctrl[1]) * 6

      }

      # The size of 'mu_ratio', 'maxlayers', and 'confidence' is 1
      # -----------------------------------------------------------------
      if (k %in% c(2, 3, 4)) ctrl <- ctrl[1] else {
        if (ctrl[1] == ctrl[2]) ctrl <- ctrl[1]
      }

      # Assign the value of 'ctrl' to the list new_control
      # -----------------------------------------------------------------
      new_control[key] <- list(ctrl)

      if (iserror != 0)
        return(list(off = TRUE, error = uierrors$controls(
          var = k, val = ctrl, code = iserror, keys = keys)))

    }

    return(list(off = FALSE, controls = new_control))

  },

  layers = function(minalpha, xlayers, layers, maxlayers, days) {
    # samples the number of layers to be used to generate data using the
    # .generate_data() function.
    #
    # Args:
    #   minalpha  : lowest value in the alpha range
    #   xlayers   : number of layers derived from the parameters
    #   layers    : number of layers provided by the users
    #   maxlayers : maximum number of layers either provided by the user
    #             : or taking its default value '5'
    #   days      : number of days for the generated data, either provided
    #             : by the user or taking its default value '60'.
    #
    # Returns:
    #   NULL if it is impossive to select a number of layers using the provided
    #   arguments, otherwise, a valid number of layers


    args <- list(days = days, minalpha = minalpha, layers = layers)

    # Check the different case to find the value of 'layers'
    # --------------------------------------------------------------------------
    alayers <- 1 / (minalpha + 10^ (-5))

    if (!is.null(layers)) {

      if (!is.numeric(layers)) return(uierrors$layers(code = 5, args))

      if (layers < 0 | floor(layers) != layers)
        return(uierrors$layers(code = 5, args))

      if (xlayers != 0) {
        if (xlayers != layers)  cat(uierrors$layers(code = 1, args))
        layers <- xlayers
      }

      if (layers >= days) return(uierrors$layers(code = 2, args))

      if (layers > alayers) return(uierrors$layers(code = 3, args))

    }

    if (is.null(layers) & maxlayers > alayers) {
      ux$show(m = uierrors$layers(code = 4, args), warning = TRUE, skip = FALSE)
      maxlayers <- alayers
    }

    if (is.null(layers)) {
      return(sample(maxlayers, 1))
    } else {
      return(layers)
    }
  },

  xclusters = function(n, xtra, lay =1, adj = FALSE) {

    err <- uierrors$arguments()

    if (adj == F) {

      # Make sure that 1 + xtraclusters + layers <= n, since the clustering
      # algorithm of initial sets generation encoded in initials_mpin will
      # cluster the data into 1 + xtraclusters + layers
      valid <- (1 + xtra + lay <= n)

      if (!valid)
        return(list(off = TRUE, error = err$compatibility(
          model = "mpin", n = n, cl = 1 + xtra + lay)))
      return(list(off = FALSE, feedback = ""))

    } else {

      # Make sure that 6 + xtraclusters <= n, since the clustering algorithm
      # of Adjpin initial sets generation encoded in initials_mpin will
      # cluster the data into 6 + xtraclusters
      valid <- (6 + xtra <= n)

      if (!valid)
        return(list(off = TRUE, error = err$compatibility(
          model = "adjpin", n = n, cl = 6 + xtra)))
      return(list(off = FALSE, feedback = ""))


    }


  },

  args = function(arglist, fn) {

    xnames <- names(arglist)
    arglist$fn <- fn

    for (i in seq_len(length(xnames))) {

      rst <- .xcheck$onearg(xnames[i], arglist)

      if (rst$off) return(rst)
    }

    return(list(off = FALSE, error = ""))

  },

  onearg = function(vn, al) {

    # vn: variable name
    # al: argument list
    errors <- uierrors$arguments()

    # determine the function, and the argument needed given the variable name vn
    # default arguments to send to the function
    # Eventually other elements will be added
    vargs <- list(var = al[[vn]], name = vn, errors = errors)

    # fn: denotes the function that call .xcheck$args
    if (vn == "fn") return(list(off = FALSE, error = ""))

    if (vn %in% c(
      "tradinghours", "samplength", "buckets", "timebarsize", "num_init",
      "xtraclusters", "layers", "grid_size", "series", "days", "timelag")
    ) {
      gn <- "xinteger"
      vargs$range <- .default[[vn]]
    }

    if (vn == "confidence") {
      gn <- "xnumeric"
      vargs$range <- .default[[vn]]
      vargs$strict <- TRUE
    }

    if (vn %in% c(
      "fact", "verbose", "is_parallel", "correction", "ea_correction",
      "reportdays")
    ) gn <- "xlogical"

    if (vn %in% c("algorithm", "method", "detectlayers", "factorization")
    ) {
      gn <- "xcharacter"
      vargs$range <- .default[[vn]]
    }

    if (vn %in% c("ranges", "hyperparams")) gn <- "xlist"

    if (vn %in% c("data", "initialsets", "restricted", "parameters")) {
      gn <- vn
      vargs$al <- al
    }

    xresult <- .xvalidate[[gn]](vargs)

    return(xresult)

  }

)


.xvalidate <- list(

  initialsets = function(vargs) {

    v <- vargs$al
    err <- vargs$errors

    vis <- v$initialsets

    is_df <- is.data.frame(vis)
    is_char <- is.character(vis)

    # Account for the case where there is only one initialset
    is_num <- is.numeric(vis)

    if (!is.null(vis) & !is_df & !is_char & !is_num) {
      return(list(off = TRUE, error = err$initials(
        error = "wrongtype", class = class(vis))))
    }

    # Check that the dataframe initialsets has the correct number of
    # variables
    if (is_df | is_num) {

      if (v$fn == "adjpin") {

        goodsize <- 10 - sum(unlist(v$restricted))
        xsize <- ifelse(is_df, ncol(vis), length(vis))
        if (xsize != goodsize) {
          return(list(off = TRUE, error = err$initials(
            error = "wrongsize", cols = xsize,
            rvars = goodsize)))
        }


        isprob <-  ifelse(ux$is.logical(v$restricted$theta),
          1:(4 - v$restricted$theta), 1:4)
        probvalues <- ifelse(
          is_df, vis[, isprob], vis[1:isprob])

        invalid <- (
          any(vis < 0) | !is.null(v$restricted$theta) &&
            any(probvalues > 1))

        if (invalid)
          return(list(off = TRUE, error = err$initials(
            error = "wrongvalues")))
      }

      if (v$fn == "mpin" | v$fn == "pin") {

        xsize <- ifelse(is_df, ncol(vis), length(vis))

        m <- (xsize - 2) / 3

        if (!ux$integer(m) || m == 0)
          return(list(off = TRUE, error = err$initials(
            error = "wronglength", cols = xsize)))

        not_prob <- ifelse(
          is_df, any(vis[, 1:(2 * m)] > 1),
          any(vis[1:(2 * m)] > 1))

        if (any(vis < 0) | not_prob)
          return(list(off = TRUE, error = err$initials(
            error = "wrongvalues")))
      }

    } else {

      if (v$fn == "adjpin") {
        if (is_char && !any(toupper(vis) %in% .default$initialsets))
          return(list(off = TRUE, error = err$initials(
            error = "wrongalgorithm", unknown = toupper(vis))))
      }

      if (v$fn == "pin" && is.null(vis)) {

        return(list(off = TRUE, error = err$initials(
          error = "wrongtype", class = class(vis))))

      }

    }





    return(list(off = FALSE, error = ""))

  },

  data = function(vargs) {

    v <- vargs$al
    err <- vargs$errors

    if (v$fn == "aggregation" | v$fn == "vpin") {

      if (!is.data.frame(v$data)) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "wrongclass",
                 class = class(v$data)
               )
          )
        )
      }

      limit <- ifelse(v$fn == "vpin", 3, 4)
      if (ncol(v$data) < limit) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "fewvariables",
                 cols = ncol(v$data),
                 limit = limit)
          )
        )
      }

      if (!ux$is.timestamp(v$data[, 1])) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "nottimestamp",
                 type1 = typeof(v$data[, 1]))
          )
        )
      }

      # Try to convert some values of the first column into a date variable

      .sample <- sample(seq_len(nrow(v$data)), min(nrow(v$data) / 10, 100))
      convertible <- vapply(
        v$data$timestamp[.sample], ux$is.convertible.to.date, logical(1))

      if (!all(convertible)) {
        .failed <- which(convertible == FALSE)[[1]]
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "notdate",
                 failure = .failed)
          )
        )
      }

      # delete NA values from v$data
      v$data <- na.omit(v$data)

      .types <- vapply(2:limit, function(x) is.numeric(v$data[, x]), logical(1))
      dtypes <- vapply(2:limit, function(x) typeof(v$data[, x]), character(1))

      if (!all(.types)) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "wrongdatatypes",
                 dtypes = dtypes)
          )
        )

      }

      .negative <- any(v$data[, 2:limit] < 0, na.rm = TRUE)
      if (.negative) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "wrongdatavalues")
          )
        )

      }



    } else {

      if (!is.data.frame(v$data)) {
        return(
          list(off = TRUE,
               error = err$tdata(
                 error = "wrongclass",
                 class = class(v$data)
               )
          )
        )
      }

      limit <- 2
      if (ncol(v$data) < 2) {
        return(
          list(off = TRUE,
               error = err$hfdata(
                 error = "fewvariables",
                 cols = ncol(v$data),
                 limit = limit)
          )
        )
      }

      .types <- vapply(2:limit, function(x) is.numeric(v$data[, x]), logical(1))
      dtypes <- vapply(2:limit, function(x) typeof(v$data[, x]), character(1))

      if (!all(.types)) {
        return(
          list(off = TRUE,
               error = err$tdata(
                 error = "wrongdatatypes",
                 dtypes = dtypes)
          )
        )

      }

      .negative <- any(v$data[, 1:limit] < 0, na.rm = TRUE)
      if (.negative) {
        return(
          list(off = TRUE,
               error = err$tdata(
                 error = "wrongdatavalues")
          )
        )

      }

    }

    # If the argument 'data' is valid, then add its row number to the default
    # range of layers and xtraclusters using the option "numberoftradingdays"
    options("numberoftradingdays" = nrow(v$data))

    return(list(off = FALSE, error = ""))

  },

  parameters = function(vargs) {

    v <- vargs$al
    err <- vargs$errors
    varname <- vargs$name

    xparams <- v[[varname]]

    if (is.null(xparams))
      return(list(off = FALSE, error = ""))


    .types <- suppressWarnings(!is.na(as.numeric(xparams)))
    allnumeric <- prod(.types)


    if (v$fn %in% c("adjpindata", "adjpin")) {

      if (allnumeric == 0)
        return(list(off = TRUE,
                    error = err$adjpindata(
                      error = "wrongtype",
                      ntype = which(.types == FALSE),
                      ktype = typeof(xparams[which(.types == FALSE)[1]])
                    )
        ))

      xparams <- as.numeric(xparams)
      if (length(xparams) != 10)
        return(list(off = TRUE,
                    error = err$adjpindata(
                      error = "wrongdim",
                      size = length(xparams)
                    )
        ))


      invalidalpha <- ((xparams[1] <= 0) | (xparams[1] >= 1))

      if (invalidalpha)
        return(list(off = TRUE,
                    error = err$adjpindata(
                      error = "wrongalpha",
                      alpha = xparams[1]
                    )
        ))


      invalidprobs <- (any(xparams[1:4] < 0) | any(xparams[1:4] > 1))

      if (invalidprobs)
        return(list(off = TRUE,
                    error = err$adjpindata(
                      error = "wrongprobabilities",
                      size = length(xparams)
                    )
        ))


      invalidrates <- (
        any(xparams[5:10] < 0) |
          any(xparams[5:10] - floor(xparams[5:10]) != 0))

      if (invalidrates) {
        return(list(off = TRUE,
                    error = err$adjpindata(
                      error = "wrongrates",
                      size = length(xparams)
                    )
        ))

      }

      return(list(off = FALSE, error = ""))


    } else {

      if (allnumeric == 0)
        return(list(off = TRUE,
                    error = err$mpindata(
                      error = "wrongtype",
                      ntype = which(.types == FALSE),
                      ktype = typeof(xparams[which(.types == FALSE)[1]])
                    )
        ))

      xparams <- as.numeric(xparams)

      # If the argument 'layers' is provided, check compatibility, otherwise
      # check that it has the size 3J + 2
      xlayers <- (length(xparams) - 2) / 3
      validsize <- (ux$is.integer(xlayers))

      if (!validsize)
        return(list(off = TRUE, error = err$mpindata(
          error = "wrongdim", size = length(xparams))))

      if (v$fn == "mpin" && !is.null(v$layers) && xlayers != v$layers) {
        ux$show(m = uierrors$layers(code = 1, NULL), warning = TRUE)
      }

      invalidalpha <- (
        any(xparams[1:xlayers] <= 0) | any(xparams[1:xlayers] >= 1))

      if (invalidalpha)
        return(list(
          off = TRUE,
          error = err$mpindata(error = "wrongalpha", layers = xlayers)))


      invalidprobs <- (any(xparams[1:(2 * xlayers)] < 0) |
                         any(xparams[1:(2 * xlayers)] > 1))

      if (invalidprobs)
        return(list(
          off = TRUE,
          error = err$mpindata(error = "wrongprobabilities",
                               size = length(xparams),
                               layers = xlayers))
          )

      raterange <- (2 * xlayers + 1):(3 * xlayers + 2)
      invalidrates <- (
        any(xparams[raterange] < 0) |
          any(xparams[raterange] - floor(xparams[raterange]) != 0))

      if (invalidrates) return(list(off = TRUE, error = err$mpindata(
        error = "wrongrates", size = length(xparams), layers = xlayers)))

      mu <- xparams[(2 * xlayers + 1):(3 * xlayers)]

      if (any(diff(mu) <= 0)) return(list(off = TRUE, error = err$mpindata(
        error = "rankedmu", size = length(xparams), layers = xlayers)))



      return(list(off = FALSE, error = ""))
    }

  },

  restricted = function(vargs) {

    v <- vargs$al
    err <- vargs$errors

    if (is.list(v$restricted) && (length(v$restricted) == 0))
      return(list(off = FALSE, error = ""))

    if (is.null(v$restricted) || !is(v$restricted, "list"))
      return(list(off = TRUE,
                  error = err$list("restricted", class(v$restricted))))

    rkeys <- names(v$restricted)
    keys <- c("mu", "eps", "d", "theta")
    unknown <- setdiff(rkeys, keys)

    if (length(unknown) > 0)
      return(list(off = TRUE, error = err$restricted(
        error = "unrecognized",
        unknown = unknown
      )))

    binary <- vapply(v$restricted, is.logical, logical(1))
    allbinary <- prod(binary)
    if (!allbinary) {
      nonbinary <- rkeys[which(binary == FALSE)[[1]]]
      return(list(off = TRUE, error = err$restricted(
        error = "nonbinary",
        nonbinary = nonbinary
      )))
    }

    return(list(off = FALSE, error = ""))

  },

  xlogical = function(vargs) {

    x <- vargs$var
    varname <- vargs$name
    err <- vargs$errors

    if (is.null(x) || !is.logical(x))
      return(list(off = TRUE, error = err$logical(varname, typeof(x))))

    return(list(off = FALSE, error = ""))

  },

  xcharacter = function(vargs) {

    x <- vargs$var
    varname <- vargs$name
    range <- vargs$range
    err <- vargs$errors

    if (is.null(x) || !is.character(x)) {

      return(list(off = TRUE,
                  error = err$character(name = varname, xrange = range,
                                        type = typeof(x))))
    }

    x <- toupper(x)
    if (!all(x %in% range))
      return(list(
        off = TRUE,
        error = err$character(
          name = varname, val = x, xrange = range, type = NULL)))

    return(list(off = FALSE, error = ""))
  },

  xinteger = function(vargs) {

    x <- vargs$var
    varname <- vargs$name
    bounds <- vargs$range
    err <- vargs$errors

    if (varname == "layers") {
      if (is.null(x))
        return(list(off = FALSE, error = ""))
    }

    if (!ux$is.integer(x)) {

      return(list(off = TRUE,
                  error = err$integer(name = varname, bounds = bounds,
                                      type = typeof(x))))
    }

    xmin <- bounds[1]
    xmax <- bounds[2]

    if (x < xmin || x > xmax)
      return(list(
        off = TRUE,
        error = err$integer(
          name = varname, val = x,  bounds = bounds, type = NULL)))

    return(list(off = FALSE, error = ""))
  },

  xnumeric = function(vargs) {

    x <- vargs$var
    varname <- vargs$name
    bounds <- vargs$range
    strict <- vargs$strict
    err <- vargs$errors

    if (!ux$is.numeric(x)) {

      return(
        list(off = TRUE,
             error = err$numeric(x = varname, bounds = bounds,
                                 type = typeof(x), strict = strict)))
    }

    xmin <- bounds[1]
    xmax <- bounds[2]

    cond <- ifelse(strict, x <= xmin || x >= xmax, x < xmin || x > xmax)

    if (cond)
      return(list(
        off = TRUE,
        error = err$numeric(
          x = varname, bounds = bounds, type = NULL, strict = strict)))

    return(list(off = FALSE, error = ""))

  },

  xlist = function(vargs) {

    x <- vargs$var
    varname <- vargs$name
    err <- vargs$errors

    if (is.null(x) || !is(x, "list"))
      return(list(off = TRUE, error = err$list(varname, class(x))))

    return(list(off = FALSE, error = ""))

  }

)

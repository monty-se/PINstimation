
## - | FILE  HEADER |
##
## Script name:
##    model_ivpin.R
##
## Purpose of script:
##    Implement the estimation of the Improved Volume-synchronized Probability of Informed Trading
##    measure (IVPIN) of Ke et al. (2017).
##
## Author:
##    Alexandre Borentain
##
## Last updated:
##    2024-06-18
##
## License:
##    GPL 3
##
## Email:
##    aboren@uw.edu
##
##
##
## Public functions:
## ++++++++++++++++++
##
## ivpin():
##    Estimates the Improved Volume-synchronized Probability of Informed Trading
##    Trading.
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


#' @title Estimation of Intraday Volume-Synchronized PIN model
#'
#' @description Estimates the Intraday Volume-Synchronized Probability of Informed
#' Trading.
#'
#' @usage ivpin(data, timebarsize = 60, buckets = 50, samplength = 50,
#'              tradinghours = 24, verbose = TRUE)
#'
#' @param data A dataframe with 3 variables:
#' \code{timestamp, price, volume}.
#' @param timebarsize An integer referring to the size of timebars
#' in seconds. The default value is \code{60}.
#' @param buckets An integer referring to the number of buckets in a
#' daily average volume. The default value is \code{50}.
#' @param samplength  An integer referring to the sample length
#' or the window size used to calculate the `IVPIN` vector.
#' The default value is \code{50}.
#' @param tradinghours An integer referring to the length of daily
#' trading sessions in hours. The default value is \code{24}.
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the IVPIN model is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @details The dataframe data should contain at least three variables. Only the
#' first three variables will be considered and in the following order
#' \code{timestamp, price, volume}.
#'
#' The property \code{@bucketdata} is created as in
#' \insertCite{abad2012;textual}{PINstimation}.
#'
#' The argument `timebarsize` is in seconds enabling the user to implement
#' shorter than `1` minute intervals. The default value is set to `1` minute
#' (`60` seconds).
#'
#' The parameter `tradinghours` is used to eventually correct the duration per
#' bucket. The duration of a given bucket is the difference between the
#' timestamp of the last trade `endtime` and the timestamp of the first trade
#' `stime` in the bucket. If the first trade and the last trade in a
#' bucket occur in two different days, and the market trading session does not
#' cover a full day \code{(24 hours)}; then the duration of the bucket will be
#' inflated. Assume that the daily trading session is 8 hours
#' `(tradinghours=8)`, the start time of a bucket is \code{2018-10-12 17:06:40}
#' and its end time is \code{2018-10-13 09:36:00}. A straightforward calculation
#' gives that the duration of this bucket is \code{59,360 secs}. However, this
#' duration includes the time during which the market is closed `(16 hours)`.
#' The corrected duration takes into consideration only the time of market
#' activity: `duration=59,360-16*3600= 1760 secs`, i.e., about `30 minutes`.
#'
#' @return Returns an object of class \code{estimate.ivpin}.
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # There is a preloaded dataset called 'hfdata' contained in the package.
#' # It is an artificially created high-frequency trading data. The dataset
#' # contains 100 000 trades and five variables 'timestamp', 'price',
#' # 'volume', 'bid' and 'ask'. For more information, type ?hfdata.
#'
#' xdata <- hfdata
#'
#' # Estimate IVPIN model, using the following parameter set where the time
#' # bar size is 5 minutes, i.e., 300 seconds (timebarsize = 300), 50
#' # buckets per average daily volume (buckets = 50), and a window size of
#' # 250 for the IVPIN calculation (samplength = 250).
#'
#' estimate <- ivpin(xdata, timebarsize = 300, buckets = 50, samplength = 250)
#'
#' # Display a description of the estimate
#'
#' show(estimate)
#'
#' # Plot the estimated IVPIN vector
#'
#' plot(estimate@vpin, type = "l", xlab = "time", ylab = "IVPIN", col = "blue")
#'
#' # Display the parameters of IVPIN estimates
#'
#' show(estimate@parameters)
#'
#' # Store the computed data of the different buckets in a dataframe 'buckets'.
#' # Display the first 10 rows of the dataframe 'buckets'.
#'
#' buckets <- estimate@bucketdata
#' show(head(buckets, 10))
#'
#' @export
ivpin <- function(data, timebarsize = 60, buckets = 50, samplength = 100,
                 tradinghours = 24, slow = FALSE, grid_size = 0.2, verbose = TRUE) {

  "
  @timebarsize  : the size of timebars in seconds default value: 60
  @buckets      : number of buckets per volume of bucket size default value: 50
  @samplength   : sample length or window of buckets to calculate VPIN default
                  value: 50
  @tradinghours : length of trading days - used to correct the durations of
                  buckets. Default value is 24.
  @slow         : TRUE computes the initial parameters by grid search at each 
                  bucket
  @grid_size    : spacing between elements of the grid search. Smaller is more precise 
                  default value: 0.2
  "
  data <- as.data.frame(data)

  # vpin_err <- uierrors$vpin()
  # vpin_ms <- uix$vpin(timebarsize = timebarsize)

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  # allvars <- names(formals())
  # environment(.xcheck$existence) <- environment()
  # .xcheck$existence(allvars, err = uierrors$vpin()$fn)

  # Checking the the arguments of the function are valid
  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, timebarsize, buckets, samplength, tradinghours, slow, grid_size, verbose)
  names(largs) <- names(formals())
  # rst <- .xcheck$args(arglist = largs, fn = "vpin")
  # ux$stopnow(rst$off, m = rst$error, s = uierrors$vpin()$fn)

  # Convert data into a dataframe, in case it is a matrix or an array
  data <- as.data.frame(data)

  # # initialize the local variables
  tbv <- vbs <- bucket <- NULL
  # estimatevpin <- new("estimate.vpin")


  time_on <- Sys.time()

  # ux$show(c= verbose, m = ivpin_ms$start)

  ##############################################################################
  #             STEP 0 : CHECKING AND PREPARING THE DATA
  ##############################################################################

  # ----------------------------------------------------------------------------
  # USER MESSAGE
  # ----------------------------------------------------------------------------

  # ux$show(c= verbose, m = ivpin_ms$step1)

  # We want only three columns: timestamp, price and volume so we import into
  # the dataset only the three first columns

  dataset <- data[, 1:3]


  # We rename the first three columns to "timestamp", "price" and "volume"

  colnames(dataset) <- c("timestamp", "price", "volume")

  # We check if the columns price and volume are numeric so that we can make
  # operations on them. If they are not, convert them to numeric.

  dataset$price <- as.numeric(dataset$price)

  dataset$volume <- as.numeric(dataset$volume)

  dataset$timestamp <- as.POSIXct(dataset$timestamp)

  rownames(dataset) <- NULL

  # If the argument 'timebarsize' is larger than the total duration of the
  # datasets in milliseconds, the abort.
  alltime <- 1000 * as.numeric(difftime(max(dataset$timestamp),
                                        min(dataset$timestamp), units = "secs"))

  # if (timebarsize >= alltime) {
  #   ux$show(m = vpin_ms$aborted, warning = TRUE)
  #   ux$stopnow(m = vpin_err$largetimebarsize, s = vpin_err$fn)
  # }


  ##############################################################################
  #             STEP 1 : CREATING THE T-SECOND TIMEBARS DATASET
  ##############################################################################

  # ----------------------------------------------------------------------------
  # USER MESSAGE
  # ----------------------------------------------------------------------------

  # ux$show(c= verbose, m = ivpin_ms$step2, skip = FALSE)

  # ----------------------------------------------------------------------------
  # I.1 CREATE THE VARIABLE INTERVAL
  # ----------------------------------------------------------------------------

  # create a variable called interval which contains the timebar extracted from
  # the timestamp. If timebarsize == 60, then the observation of timestamp
  # "2019-04-01 00:33:49" belong to the interval "2019-04-01 00:33:00", that is
  # the timebar that starts at the minute 33 and lasts 1 minute (60 seconds)
  # (timebarsize)

  # If timebarsize == 300, then the observation of timestamp "2019-04-01 00:33:49"
  # belong to the interval "2019-04-01 00:30:00", that is the timebar that
  # starts at the minute 30 and lasts 5 minutes (300 seconds) (timebarsize)

  tbsize <- paste(timebarsize, " sec", sep = "")

  dataset$interval <- cut(dataset$timestamp, breaks = tbsize)

  # ----------------------------------------------------------------------------
  # I.2 CREATE THE T-MINUTE TIMEBAR DATASET CALLED 'MINUTEBARS'
  # ----------------------------------------------------------------------------

  # Create t-minute timebar dataset which will aggregate the transaction volume
  # (tbv: timebar volume) and price change: last price - first price (dp) per
  # interval


  # ----------------------------------------------------------------------------
  # -       THIS PART IS JUST TO ESTIMATE RUNNING TIME - NO OUTPUT USED        -
  # -                                                                          -
  # diffseconds <- function(time_on, time_off) {
  #   dsecs <- difftime(time_off, time_on, units = "secs")
  #   return(dsecs)
  # }

  # if (nrow(dataset) >= 50000) {

  #   temptime_on <- Sys.time()

  #   chunk <- 5000

  #   tempbars <- aggregate(price ~ interval, data = dataset[1:chunk, ],
  #     FUN = function(x) dp <- tail(x, 1) - head(x, 1)
  #   )
  #   tempbars <- merge(tempbars,
  #                     aggregate(volume ~ interval, dataset[1:chunk, ], sum),
  #                     by = "interval"
  #   )
  #   tempbars$interval <- as.POSIXct(tempbars$interval, tz = "")

  #   temptime_off <- Sys.time()

  #   exptime <-  ux$timediff(temptime_on, temptime_off,
  #                             5*log2(nrow(dataset) / (chunk)))

  #   ux$show(c= verbose, m = paste("[~", ceiling(exptime), "seconds]"))

  # } else {

  #   # ux$show(c= verbose, m = "")
  # }

  # -                                                                          -
  # ----------------------------------------------------------------------------

  minutebars <- aggregate(price ~ interval, data = dataset,
    FUN = function(x) dp <- tail(x, 1) - head(x, 1)
  )
  minutebars <- merge(minutebars,
                      aggregate(volume ~ interval, dataset, sum),
                      by = "interval")
  minutebars$interval <- as.POSIXct(minutebars$interval, tz = "")

  colnames(minutebars) <- c("interval", "dp", "tbv")
  minutebars <- minutebars[complete.cases(minutebars), ]

  ############################################################################
  #             STEP 2 : CALCULATING VOLUME BUCKET SIZE AND SIGMA(DP)
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  # ux$show(c= verbose, m = ivpin_ms$step3)

  # --------------------------------------------------------------------------
  # II.1 CALCULATE NUMBER OF DAYS (NDAYS), TOTAL VOLUME (TOTVOL) AND THEN VBS
  # --------------------------------------------------------------------------

  # Description of the variables:
  # ndays   : number of unique days in the dataset minutebars
  # totvol  : total volume over all dataset
  # vbs     : VOLUME BUCKET SIZE

  ndays <- length(unique(substr(minutebars$interval, 1, 10)))

  totvol <- sum(minutebars$tbv)
  vbs <- (totvol / ndays) / buckets

  params <- c(timebarsize, buckets, samplength, vbs, ndays)

  names(params) <- c("tbSize", "buckets", "samplength", "VBS", "ndays")

  # estimatevpin@parameters <- params

  # --------------------------------------------------------------------------
  # II.2 CALCULATE THE STANDARD DEVIATION OF DP (SDP)
  # --------------------------------------------------------------------------

  
  sdp <- sd(minutebars$dp)
  
  ############################################################################
  #             STEP 3 :  CALCULATING BUY AND SELL VOLUME
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  # ux$show(c= verbose, m = vpin_ms$step4)

  # --------------------------------------------------------------------------
  # VI.1 ASSIGN N(0, 1) PROB. TO EACH TIMEBAR AND CALCULATE BUY/SELL VOLUMES
  # --------------------------------------------------------------------------

  # Use the cdf of N(0, 1) to calculate zb = Z(dp/sdp) and zs = 1- Z(dp/sdp)
  # The variable zb calculates the probability that the current price change
  # normalized by standard deviation (dp/sdp) corresponds to timebar with
  # buyer-initiated transacations, while zs calculates the probability that
  # the current price change normalized by standard deviation (dp/sdp)
  # corresponds to timebar with seller-initiated transacations.

  minutebars$zb <- pnorm(minutebars$dp / sdp)
  minutebars$zs <- 1 - pnorm(minutebars$dp / sdp)

  # Calculate Buy Volume (bvol) and Sell volume (svol) by multiplying timebar's
  # volume (tbv) by the corresponding probabilities zb and zs.

  minutebars$bvol <- minutebars$tbv * minutebars$zb
  minutebars$svol <- minutebars$tbv * minutebars$zs

  minutebars <- minutebars[which(minutebars$tbv > 0), ]


  ############################################################################
  #             STEP 4 : ASSIGNING TIME BARS TO VOLUME BUCKETS
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  # ux$show(c= verbose, m = vpin_ms$step5)


  # Initialize bucketdata
  num_buckets <- ceiling(sum(minutebars$tbv) / vbs)
  bucketdata <- data.frame(
    bucket = 1:num_buckets,
    agg.bvol = numeric(num_buckets),
    agg.svol = numeric(num_buckets),
    duration = numeric(num_buckets),
    current_volume = numeric(num_buckets),
    stringsAsFactors = FALSE
  )
  
  current_bucket <- 1
  remaining_volume <- vbs
  
  for (i in 1:nrow(minutebars)) {
    bar_volume <- minutebars$tbv[i]
    bar_bvol <- minutebars$bvol[i]
    bar_svol <- minutebars$svol[i]
    bar_duration <- 60
    
    while (bar_volume > 0 && current_bucket <= num_buckets) {
      if (bar_volume <= remaining_volume) {
        # The entire bar fits in the current bucket
        proportion <- 1
        volume_to_add <- bar_volume
      } else {
        # spill over
        proportion <- remaining_volume / bar_volume
        volume_to_add <- remaining_volume
      }
      
      bucketdata$agg.bvol[current_bucket] <- bucketdata$agg.bvol[current_bucket] + proportion * bar_bvol
      bucketdata$agg.svol[current_bucket] <- bucketdata$agg.svol[current_bucket] + proportion * bar_svol
      bucketdata$duration[current_bucket] <- bucketdata$duration[current_bucket] + proportion * bar_duration
      bucketdata$current_volume[current_bucket] <- bucketdata$current_volume[current_bucket] + volume_to_add
      
      # Update remaining values
      bar_volume <- bar_volume - volume_to_add
      bar_bvol <- bar_bvol * (1 - proportion)
      bar_svol <- bar_svol * (1 - proportion)
      bar_duration <- bar_duration * (1 - proportion)
      remaining_volume <- remaining_volume - volume_to_add
      
      # If the bucket is full, move to the next one
      if (remaining_volume == 0 && current_bucket <= num_buckets) {
        current_bucket <- current_bucket + 1
        remaining_volume <- vbs
      }
    }
  }  

  ############################################################################
  #             STEP 5 : COMPUTE IVPIN
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  # ux$show(c= verbose, m = vpin_ms$step6)

  # Normalize the duration to be centered around 1
  mean_duration <- mean(bucketdata$duration)
  bucketdata$duration <- bucketdata$duration / mean_duration
  bucketdata$duration <- ifelse(bucketdata$duration == 0, 1e-6, bucketdata$duration)

  # The function to be optimized                               
  compute_log_likelihood <- function(params, t, Vb, Vs) {
    alpha <- params[1]
    delta <- params[2]
    mu <- params[3] 
    eps <- params[4]
    
    e_1 <- log(alpha * delta) + Vb * log(eps) + Vs * log(eps + mu) - (2 * eps + mu) * t
    e_2 <- log(alpha * (1 - delta)) + Vb * log(eps + mu) + Vs * log(eps) - (2 * eps + mu) * t
    e_3 <- log(1 - alpha) + Vb * log(eps) + Vs * log(eps) - 2 * eps * t
    e_max <- pmax(e_1, e_2, e_3)
    
    log_likelihood <- -sum(log(exp(e_1 - e_max) + exp(e_2 - e_max) + exp(e_3 - e_max)) + e_max)
    return(log_likelihood)
  }

  # Easley et al. (2012b) derive the VPIN estimator based on two moment conditions from Poisson processes:
  # E[|VB - VS|] ≈ alpha*mu and E[|VB + VS|] = 2*epsilon + alpha*mu.
  # Ke et al. (2017) suggest that these conditions should be expressed as:
  # E[|VB - VS||t; theta] ≈ alpha*mu*t and E[|VB + VS||t; theta] = (2*epsilon + alpha*mu)*t.
  # These equations correspond to Equations (3) on page 364 of Ke et al. (2017) .
  # 
  # In our implementation:
  # - The variable `total_arrival_rate` is calculated as  |VB + VS|/t. Its average (expected value) would, therefore, represent 2*epsilon + alpha*mu.
  # - The variable `informed_arrival_rate` is calculated as |VB - VS|/t.  Its average (expected value) would, therefore, approximate alpha*mu.
  # 
  # This approximation allows us to estimate mu by dividing `informed_arrival_rate` by an estimated alpha.
  # Once mu is estimated, we can determine epsilon using the equation for `total_arrival_rate`.

  total_arrival_rate <- (bucketdata$agg.bvol + bucketdata$agg.svol) / bucketdata$duration
  informed_arrival <- abs(bucketdata$agg.bvol - bucketdata$agg.svol) / bucketdata$duration

  t <- bucketdata$duration
  Vb <- bucketdata$agg.bvol
  Vs <- bucketdata$agg.svol

  ivpin <- numeric(nrow(bucketdata))  

  perform_grid_search <- function(i, j, informed_arrival, total_arrival_rate, t, Vb, Vs, grid_size) {
    best_params <- NULL
    best_log_likelihood <- Inf
    exit_flag <- FALSE
    for (alpha_init in seq(0.1, 0.9, by = grid_size)) {
      for (delta_init in seq(0.1, 0.9, by = grid_size)) {
        mu_init <- mean(informed_arrival[j:i]) / alpha_init
        eps_init <- mean(abs(total_arrival_rate[j:i] - informed_arrival[j:i])) / 2
        
        initial_guess <- c(alpha_init, delta_init, mu_init, eps_init)
        
        low <- c(0, 0, 0, 0)  # Lower bounds
        up <- c(1, 1, Inf, Inf)  # Upper bounds

        result <- tryCatch({
          nloptr::neldermead(
            x0 = initial_guess,
            fn = function(params) compute_log_likelihood(params, t[j:i], Vb[j:i], Vs[j:i]),
            lower = low,
            upper = up
          )
        }, error = function(e) {
          message("Error during optimization: ", conditionMessage(e))
          return(NULL)
        })
        
        if (!is.null(result) && result$value < best_log_likelihood) {
          best_params <- result$par
          best_log_likelihood <- result$value
          exit_flag <- TRUE
        }
      }
    }
    return(list(best_params, best_log_likelihood, exit_flag))
  }
                               
  # We start the computation of IVPIN at the samplength-th index 
  start_index <- samplength + 1
  # Compute iVPIN with a rolling window by iterating over each bucket. We perfom a grid search to find the optimal initial parameters of the
  # first bucket, and for each subsequent bucket, we use the previous bucket's initial parametes as the current initial paramenters.                                 
  for (i in start_index:nrow(bucketdata)) {
    j <- i - samplength
    parms <- rep(-Inf, 4)  # alpha, delta, mu, eps
    
    best_log_likelihood <- Inf
    exit_flag <- FALSE
    
    if (i == start_index || slow == TRUE) {
      result <- perform_grid_search(i, j, informed_arrival, total_arrival_rate, t, Vb, Vs, grid_size)
      best_params <- result[[1]]
      best_log_likelihood <- result[[2]]
      exit_flag <- result[[3]]
    } else {
      initial_guess <- c(best_params[1], best_params[2], best_params[3], best_params[4])

      tryCatch({
        result <- nloptr::neldermead(
          initial_guess,
          fn = function(params) compute_log_likelihood(params, t[j:i], Vb[j:i], Vs[j:i]),
          lower = c(0, 0, 0, 0),
          upper = c(1, 1, Inf, Inf)
        )
        if (!is.null(result) && result$value < best_log_likelihood) {
          best_params <- result$par
          exit_flag <- TRUE
        } else {
          result <- perform_grid_search(i, j, informed_arrival, total_arrival_rate, t, Vb, Vs, grid_size)
          best_params <- result[[1]]
          best_log_likelihood <- result[[2]]
          exit_flag <- FALSE
        }
      }, error = function(e) {
        message("Error during optimization: ", conditionMessage(e))
      })
    }
    if (exit_flag == TRUE) {
      ivpin_estimate <- best_params[1] * best_params[3] / (2 * best_params[4] + best_params[3])
      ivpin[i] <- ivpin_estimate
    } else {
      cat("No valid parameters found at index", i, "\n")
      ivpin[i] <- NA
    }
  }

  # estimatevpin@ivpin <- ivpin
  # estimatevpin@runningtime <- ux$timediff(time_on, time_off)
  num_of_nan <- sum(is.na(ivpin))
  if (num_of_nan > 0) {
    warning(sprintf("Optimization failed for %d buckets", num_of_nan))
  }
  # ux$show(c= verbose, m = vpin_ms$complete)

  return(ivpin)

}

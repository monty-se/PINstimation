## - | FILE  HEADER |
##
## Script name:
##    model_vpin.R
##
## Purpose of script:
##    Implement the estimation of the Volume-synchronized PIN
##    measure (VPIN) of Easley et al. (2011, 2012)
##
##    Implement the estimation of the improved Volume-synchronized PIN
##    measure (IVPIN) of Ke and Lin (2017)
##
## Author:
##    Montasser Ghachem (vpin)
##    Alexandre Borentain Cristea and Montasser Ghachem (ivpin)
##
## Last updated:
##    2025-12-15
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
## vpin():
##    Estimates the Volume-synchronized probability of Informed
##    trading as developed in Easley (2011, 2012).
##
## ivpin():
##    Estimates the Improved Volume-synchronized probability of Informed
##    trading as developed in Ke and Lin (2017).
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


#' @title Estimation of Volume-Synchronized PIN model (vpin) and the improved
#' volume-synchronized PIN model (ivpin)
#'
#' @description Estimates the Volume-Synchronized Probability of Informed
#' Trading as developed in \insertCite{Easley2011;textual}{PINstimation}
#' and \insertCite{Easley2012;textual}{PINstimation}. \cr
#' Estimates the improved Volume-Synchronized Probability of Informed
#' Trading as developed in \insertCite{ke2017improved;textual}{PINstimation}.
#'
#' @param data A dataframe with 3 variables:
#' \code{{timestamp, price, volume}}.
#' @param timebarsize An integer referring to  the size of timebars
#' in seconds. The default value is \code{60}.
#' @param buckets An integer referring to the number of buckets in a
#' daily average volume. The default value is \code{50}.
#' @param samplength  An integer referring to the sample length
#' or the window size used to calculate the `VPIN` vector.
#' The default value is \code{50}.
#' @param tradinghours An integer referring to the length of daily
#' trading sessions in hours. The default value is \code{24}.
#' @param grid_size An integer between `1`, and `20`;
#' representing the size of the grid used in the estimation of IVPIN. The
#' default value is `5`. See more in details.
#' @param verbose A logical variable that determines whether detailed
#' information about the steps of the estimation of the VPIN (IVPIN) model is
#' displayed. No output is produced when \code{verbose} is set to \code{FALSE}.
#' The default value is \code{TRUE}.
#'
#' @details The dataframe data should contain at least three variables. Only the
#' first three variables will be considered and in the following order
#' \code{{timestamp, price, volume}}.
#'
#' The argument `timebarsize` is in seconds enabling the user to implement
#' shorter than `1` minute intervals. The default value is set to `1` minute
#' (`60` seconds) following Easley et al. (2011, 2012).
#'
#' The argument `tradinghours` is used to correct the duration per
#' bucket if the market trading session does not cover a full day \code{(24 hours)}.
#' The duration of a given bucket is the difference between the
#' timestamp of the last trade `endtime` and the timestamp of the first trade
#' `stime` in the bucket. If the first and last trades in a bucket occur
#' on different days, and the market trading session is shorter than
#' `24 hours`, the bucket's duration will be inflated. For example, if the daily
#' trading session is 8 hours `(tradinghours = 8)`, and the start time of a
#' bucket is \code{2018-10-12 17:06:40} and its end time is
#' \code{2018-10-13 09:36:00}, the straightforward calculation gives a duration
#' of \code{59,360 secs}. However, this duration includes 16 hours when the
#' market is closed. The corrected duration considers only the market activity
#' time: \code{duration = 59,360 - 16 * 3600 = 1,760 secs}, approximately
#' `30 minutes`.
#'
#' The argument `grid_size` determines the size of the grid for the variables
#' `alpha` and `delta`, used to generate the initial parameter sets
#' that prime the maximum-likelihood estimation step of the
#' algorithm by \insertCite{ke2017improved;textual}{PINstimation} for estimating
#' `IVPIN`. If `grid_size` is set to a value `m`, the algorithm creates a
#' sequence starting from \code{1 / (2m)} and ending at \code{1 - 1 / (2m)}, with a
#' step of `1 / m`. The default value of `5` corresponds to the grid size used by
#' \insertCite{Yan2012;textual}{PINstimation}, where the sequence starts at
#' \code{0.1 = 1 / (2 * 5)} and ends at \code{0.9 = 1 - 1 / (2 * 5)}
#' with a step of \code{0.2 = 1 / 5}. Increasing the value of `grid_size`
#' increases the running time and may marginally improve the accuracy of the
#' IVPIN estimates
#'
#' @return Returns an object of class \link{estimate.vpin-class}, which
#' contains the following slots:
#' \describe{
#'   \item{\code{@improved}}{ A logical variable that takes the value `FALSE`
#'   when the classical VPIN model is estimated (using `vpin()`), and `TRUE`
#'   when the improved VPIN model is estimated (using `ivpin()`).}
#'   \item{\code{@bucketdata}}{ A data frame created as in
#' \insertCite{abad2012;textual}{PINstimation}.}
#'   \item{\code{@dailyvpin}}{ A data frame with calendar–day aggregates of VPIN.
#'   For each trading day, it contains three variables:
#'     \code{day} (Date),
#'     \code{dvpin} (simple daily average of per–bucket VPIN),
#'     and \code{dwvpin} (duration–weighted daily VPIN, i.e. the weighted
#'     average of bucket VPINs with weights proportional to the effective
#'     bucket durations).
#'   }
#'   \item{\code{@vpin}}{ A vector of VPIN values.}
#'   \item{\code{@ivpin}}{ A vector of IVPIN values, which remains empty when
#'   the function `vpin()` is called.}
#' }
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # The package includes a preloaded dataset called 'hfdata'.
#' # This dataset is an artificially created high-frequency trading data
#' # containing 100,000 trades and five variables: 'timestamp', 'price',
#' # 'volume', 'bid', and 'ask'. For more information, type ?hfdata.
#'
#' xdata <- hfdata
#'
#' ### Estimation of the VPIN model ###
#'
#' # Estimate the VPIN model using the following parameters:
#' # - timebarsize: 5 minutes (300 seconds)
#' # - buckets: 50 buckets per average daily volume
#' # - samplength: 250 for the VPIN calculation
#'
#' estimate <- vpin(xdata, timebarsize = 300, buckets = 50, samplength = 250)
#'
#' # Display a description of the VPIN estimate
#'
#' show(estimate)
#'
#' # Display the parameters of the VPIN estimates
#'
#' show(estimate@parameters)
#'
#' # Display the summary statistics of the VPIN vector
#'
#' summary(estimate@vpin)
#'
#' # Store the computed data of the different buckets in a dataframe 'buckets'
#' # and display the first 10 rows of the dataframe.
#'
#' buckets <- estimate@bucketdata
#' show(head(buckets, 10))
#'
#' # Display the first 10 rows of the dataframe containing daily vpin values.
#'
#' dayvpin <- estimate@dailyvpin
#' show(head(dayvpin, 10))
#'
#'
#' ### Estimation of the IVPIN model ###
#'
#' # Estimate the IVPIN model using the same parameters as above.
#' # The grid_size parameter is unspecified and will default to 5.
#'
#' iestimate <- ivpin(xdata[1:50000,], timebarsize = 300, samplength = 50, verbose = FALSE)
#'
#' # Display the summary statistics of the IVPIN vector
#'
#' summary(iestimate@ivpin)
#'
#' # The output of ivpin() also contains the VPIN vector in the @vpin slot.
#' # Plot the VPIN and IVPIN vectors in the same plot using the iestimate object.
#'
#' # Define the range for the VPIN and IVPIN vectors, removing NAs.
#'
#' vpin_range <- range(c(iestimate@vpin, iestimate@ivpin), na.rm = TRUE)
#'
#' # Plot the VPIN vector in blue
#'
#' plot(iestimate@vpin, type = "l", col = "blue", ylim = vpin_range,
#'      ylab = "VPIN/iVPIN", xlab = "Bucket", main = "Plot of VPIN and IVPIN")
#'
#' # Add the IVPIN vector in red
#' lines(iestimate@ivpin, type = "l", col = "red")
#'
#' # Add a legend to the plot
#' legend("topright", legend = c("VPIN", "IVPIN"), col = c("blue", "red"),
#'  lty = 1,
#'  cex = 0.6,  # Adjust the text size
#'  x.intersp = 1.2,  # Adjust the horizontal spacing
#'  y.intersp = 2,  # Adjust the vertical spacing
#'  inset = c(0.05, 0.05))  # Adjust the position slightly
#'
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by mutate arrange select summarise
#' @importFrom stats lag
#' @importFrom tidyr fill
#' @aliases vpin ivpin
#' @name vpin_measures
NULL


#' @rdname vpin_measures
#' @export
vpin <- function(data, timebarsize = 60, buckets = 50, samplength = 50,
                 tradinghours = 24, verbose = TRUE) {

  "
@timebarsize  : the size of timebars in seconds default value: 60
@buckets      : number of buckets per volume of bucket size default value: 50
@samplength   : sample length or window of buckets to calculate VPIN default
                value: 50
@tradinghours : length of trading days - used to correct the durations of
                buckets. Default value is 24.
"

  vpin_err <- uierrors$vpin()
  vpin_ms <- uix$vpin(timebarsize = timebarsize, improved = FALSE)

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$vpin()$fn)

  # Checking the the arguments of the function are valid
  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, timebarsize, buckets, samplength, tradinghours, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "vpin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$vpin()$fn)

  .vpin(data = data, timebarsize = timebarsize, buckets = buckets,
        samplength = samplength, tradinghours = tradinghours, improved = FALSE,
        verbose = verbose)
}


#' @rdname vpin_measures
#' @export
ivpin <- function(data, timebarsize = 60, buckets = 50, samplength = 50,
                   tradinghours = 24, grid_size = 5, verbose = TRUE) {

  "
@timebarsize  : the size of timebars in seconds default value: 60
@buckets      : number of buckets per volume of bucket size default value: 50
@samplength   : sample length or window of buckets to calculate VPIN default
                value: 50
@tradinghours : length of trading days - used to correct the durations of
                buckets. Default value is 24.
@grid_size    : size of the grid used to generate initial parameter sets for
                the maximum-likelihood step for the estimation of ivpin.
"

  vpin_err <- uierrors$vpin()
  vpin_ms <- uix$vpin(timebarsize = timebarsize, improved = TRUE)

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$vpin()$fn)

  # Checking the the arguments of the function are valid
  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, timebarsize, buckets, samplength, tradinghours, grid_size,
                verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "vpin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$vpin()$fn)

  .vpin(data = data, timebarsize = timebarsize, buckets = buckets,
        samplength = samplength, tradinghours = tradinghours,
        grid_size = grid_size, improved = TRUE, verbose = verbose)
}





##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++



.vpin <- function(data, timebarsize = 60, buckets = 50, samplength = 50,
                  tradinghours = 24, improved = FALSE, grid_size = 5,
                  verbose = TRUE) {

  "
@timebarsize  : the size of timebars in seconds default value: 60
@buckets      : number of buckets per volume of bucket size default value: 50
@samplength   : sample length or window of buckets to calculate VPIN default
                value: 50
@tradinghours : length of trading days - used to correct the durations of
                buckets. Default value is 24.
@improved     : selects the estimation model, the volume-synchronized PIN of
                Easley et al.(2011, 2012) if FALSE, and the improved volume-
                synchronized PIN of Ke and Lin (2017).
@grid_size    : size of the grid used to generate initial parameter sets for
                the maximum-likelihood step for the estimation of ivpin.
"

  vpin_err <- uierrors$vpin()
  vpin_ms <- uix$vpin(timebarsize = timebarsize, improved)

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$vpin()$fn)

  # Checking the the arguments of the function are valid
  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, timebarsize, buckets, samplength, tradinghours, improved,
                grid_size, verbose)
  names(largs) <- names(formals())
  rst <- .xcheck$args(arglist = largs, fn = "vpin")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$vpin()$fn)

  # Convert data into a dataframe, in case it is a matrix or an array
  data <- as.data.frame(data)

  # initialize the local variables
  tbv <- vbs <- bucket <- NULL
  estimatevpin <- new("estimate.vpin")
  estimatevpin@improved = improved


  time_on <- Sys.time()

  ux$show(c= verbose, m = vpin_ms$start)

  ##############################################################################
  #             STEP 0 : CHECKING AND PREPARING THE DATA
  ##############################################################################

  # ----------------------------------------------------------------------------
  # USER MESSAGE
  # ----------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step1)

  # --------------------------------------------------------------------------
  # 0.1 Extract and rename core columns
  # --------------------------------------------------------------------------
  # We keep only the first three columns and force them to be a data.frame
  # (in case 'data' is a matrix or tibble). We then rename them:
  # "timestamp", "price" and "volume"
  # These are the only fields needed for VPIN/IVPIN construction.

  dataset <- as.data.frame(data[, 1:3, drop = FALSE])
  colnames(dataset) <- c("timestamp", "price", "volume")


  # --------------------------------------------------------------------------
  # 0.2 Ensure numeric price and volume, drop invalid rows
  # --------------------------------------------------------------------------
  # We coerce 'price' and 'volume' to numeric, suppressing warnings (e.g. if
  # there are non-numeric strings). Then we drop any rows where price or volume
  # are NA after coercion.

  dataset$price  <- suppressWarnings(as.numeric(dataset$price))
  dataset$volume <- suppressWarnings(as.numeric(dataset$volume))

  # Keep only rows with valid numeric price AND volume
  dataset <- dataset[!(is.na(dataset$price) | is.na(dataset$volume)), ]


  # --------------------------------------------------------------------------
  # 0.3 Ensure POSIXct timestamp
  # --------------------------------------------------------------------------
  # We convert 'timestamp' to POSIXct in UTC. This assumes the original
  # timestamp is numeric seconds since epoch or an object that as.POSIXct can
  # handle via this origin. Adjust this if your input timestamps are already
  # POSIXct or in a different format.

  dataset$timestamp <- as.POSIXct(dataset$timestamp,
                                  origin = "1970-01-01", tz = "UTC")

  # Reset row names to a clean 1:n sequence
  rownames(dataset) <- NULL

  # --------------------------------------------------------------------------
  # 0.4 Sanity check for timebarsize
  # --------------------------------------------------------------------------
  # We compute the total span of the dataset in SECONDS and ensure that
  # 'timebarsize' (also in SECONDS) is strictly smaller. If the time bar is
  # larger than or equal to the total sample span, we abort because you cannot
  # construct more than one bar.

  total_span_sec <- as.numeric(
    difftime(max(dataset$timestamp), min(dataset$timestamp), units = "secs")
  )

  if (timebarsize >= total_span_sec) {
    ux$show(m = vpin_ms$aborted, warning = TRUE)
    ux$stopnow(m = vpin_err$largetimebarsize, s = vpin_err$fn)
  }

  # At this point:
  # - 'dataset' contains one row per trade with (timestamp, price, volume),
  #   all clean and numeric.
  # - 'timebarsize' is in seconds, and small enough relative to the sample span.
  # We are ready to construct time bars for VPIN/IVPIN.


  ##############################################################################
  #             STEP 1 : CREATING THE T-SECOND TIMEBARS DATASET
  ##############################################################################

  # ----------------------------------------------------------------------------
  # USER MESSAGE
  # ----------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step2, skip = FALSE)

  ##############################################################################
  # STEP 1.1 : CREATE THE VARIABLE INTERVAL
  ##############################################################################


  # --------------------------------------------------------------------------
  # 1.1. Align the time-bar grid to the daily clock
  # --------------------------------------------------------------------------
  # To avoid bars crossing midnight or non-trading days, we:
  #  - Align the first bar to the nearest multiple of 'timebarsize' from midnight
  #    of the first trading day.
  #  - Then build a regular grid of bars starting from that time.

  # Ensure trades are sorted in time
  dataset <- dataset[order(dataset$timestamp), , drop = FALSE]

  # Calendar date of the first trade (UTC) and midnight of that day
  first_day  <- as.Date(dataset$timestamp[1], tz = "UTC")
  start_day  <- as.POSIXct(first_day, tz = "UTC")  # e.g. "2019-04-01 00:00:00"

  # Time of the first trade
  first_trade_time <- dataset$timestamp[1]

  # Seconds from midnight to the time of the first trade
  total_secs <- as.numeric(difftime(first_trade_time, start_day, units = "secs"))

  # Index of the bar starting at or before the first trade:
  #   bar_index = floor(total_secs / timebarsize)
  # Bar start time:
  first_bar_index <- floor(total_secs / timebarsize)
  start_time      <- start_day + first_bar_index * timebarsize

  # Example:
  #  first_trade_time = "2019-04-01 09:03:10"
  #  timebarsize      = 300 (5 minutes)
  #  -> start_time    = "2019-04-01 09:00:00"

  # Map each trade timestamp to the start of its bar:
  # - secs_since_start is the number of seconds since the global start_time.
  # - floor(secs_since_start / timebarsize) gives the bar index (0,1,2,...).
  # - 'interval' is then the POSIXct start time of that bar.
  #
  # All trades that fall in the same time bar share the same 'interval'.

  secs_since_start <- as.numeric(
    difftime(dataset$timestamp, start_time, units = "secs"))
  dataset$interval <- start_time +
    timebarsize * floor(secs_since_start / timebarsize)

  # Identify the unique trading days actually present in the original data
  trading_days <- unique(as.Date(dataset$timestamp, tz = "UTC"))

  # For defining the grid end, we use the last bar actually touched by a trade
  last_interval <- max(dataset$interval)


  # --------------------------------------------------------------------------
  # 1.2 Expand to a full grid of T-second bars (including empty bars)
  # --------------------------------------------------------------------------
  # We now ensure that for every bar between 'start_time' and 'last_interval'
  # there is at least one row in 'dataset'. Bars with no trades will be added
  # with NA price/volume initially, then adjusted in the next step.

  dataset <- dataset %>%
    tidyr::complete(interval = seq(from = start_time,
                                   to   = last_interval,
                                   by   = timebarsize)) %>%
    dplyr::arrange(interval)

  # --------------------------------------------------------------------------
  # 1.3 Forward-fill price, set zero volume for empty bars
  # --------------------------------------------------------------------------
  # For bars with no trades:
  #  - We carry forward the last observed price (standard in HF bar construction).
  #  - We set volume = 0.
  #  - We set timestamp = interval start, so every bar has a timestamp.

  dataset <- dataset %>%
    tidyr::fill(price, .direction = "down") %>%         # carry last price forward
    dplyr::mutate(
      volume   = tidyr::replace_na(volume, 0),          # empty bar -> zero volume
      timestamp = dplyr::coalesce(timestamp, interval), # use interval start if NA
      day       = as.Date(interval, tz = "UTC")
    )


  # --------------------------------------------------------------------------
  # 1.4 Drop non-trading days that arise from the grid
  # --------------------------------------------------------------------------
  # The complete()/grid step may create bars for days with no trades (e.g. weekends).
  # We remove those days by keeping only days that appear in 'trading_days'.

  dataset <- dataset %>%
    dplyr::filter(day %in% trading_days) %>%
    dplyr::select(-day) %>%
    dplyr::arrange(interval)

  # RESULT:
  # - 'dataset' now has one or more rows per T-second bar (interval),
  #   including bars with no trades (volume = 0).
  # - The intervals are aligned to the daily clock and do not cross days.
  # - This is the correct starting point to aggregate to bar-level dp and tbv
  #   for VPIN/IVPIN.



  ##############################################################################
  # STEP 1.2 : CREATE THE T-SECOND TIMEBAR DATASET 'minutebars'
  ##############################################################################
  # Goal:
  #   Collapse tick data ('dataset') into regular T-second bars identified
  #   by 'interval'. For each bar we compute:
  #     dp  = last(price in bar) - first(price in bar)
  #     tbv = sum(volume in bar)
  #
  # These bar-level series (dp, tbv) are the basic inputs for VPIN/IVPIN.



  # ----------------------------------------------------------------------------
  # I.2 CREATE THE T-MINUTE TIMEBAR DATASET CALLED 'MINUTEBARS'
  # ----------------------------------------------------------------------------
  # Goal:
  #   Collapse tick-by-tick data into regular T-second bars, one row per bar.
  #   For each bar (identified by 'interval'):
  #     dp  = last(price) - first(price) within the bar
  #     tbv = total traded volume within the bar
  #
  # 'interval' is the bar identifier = start time of the bar.
  # All trades with timestamps in [interval, interval + timebarsize) share the
  # same 'interval' and are aggregated together.

  # ----------------------------------------------------------------------------
  # Optional: rough running-time estimate (for user feedback only)
  # ----------------------------------------------------------------------------

  if (nrow(dataset) >= 50000L) {

    chunk <- min(50000L, nrow(dataset))
    t0 <- Sys.time()

    invisible(
      dataset[seq_len(chunk), ] %>%
        dplyr::group_by(interval) %>%
        dplyr::summarise(
          dp  = dplyr::last(price) - dplyr::first(price),
          tbv = sum(volume, na.rm = TRUE),
          .groups = "drop"
        )
    )

    t1 <- Sys.time()
    chunk_time <- as.numeric(difftime(t1, t0, units = "secs"))
    scale      <- nrow(dataset) / chunk
    exptime    <- chunk_time * scale
    ux$show(c = verbose, m = paste0("[~", ceiling(exptime), " seconds]"))

  } else {
    ux$show(c = verbose, m = "")
  }


  # ----------------------------------------------------------------------------
  # Main aggregation: build 'minutebars' (T-second timebars)
  # ----------------------------------------------------------------------------

  # IMPORTANT:
  # - We sort by (interval, timestamp) to ensure that within each bar the
  #   first() and last() prices are computed in correct time order.
  # - We group by 'interval' (bar identifier) and compute dp and tbv.

  minutebars <- dataset %>%
    dplyr::arrange(interval, timestamp) %>%     # ensure within-bar order
    dplyr::group_by(interval) %>%
    dplyr::summarise(
      # Price change within the bar: last price - first price
      dp  = dplyr::last(price) - dplyr::first(price),

      # Timebar volume: sum of volumes in this bar
      tbv = sum(volume, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    tidyr::drop_na(dp, tbv) %>%                 # keep only fully defined bars
    dplyr::arrange(interval)



  # RESULT:
  # - 'minutebars' contains one row per T-second bar.
  # - Columns:
  #     interval : POSIXct bar start time (bar identifier)
  #     dp       : last(price) - first(price) in that bar
  #     tbv      : total traded volume in that bar
  # - This is the input for:
  #     * computing ndays, total volume, and VBS (Step 2),
  #     * computing sd(dp),
  #     * and eventually constructing volume buckets for VPIN/IVPIN.


  ############################################################################
  # STEP 2 : CALCULATING VOLUME BUCKET SIZE (VBS) AND SIGMA(DP)
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------
  ux$show(c = verbose, m = vpin_ms$step3)

  # We assume:
  # - 'minutebars' has been created in Step 1 and contains:
  #     interval : POSIXct bar start time
  #     dp       : last(price) - first(price) in the bar
  #     tbv      : total traded volume in the bar
  # - 'buckets' is the desired number of VOLUME buckets per TRADING DAY.
  # - 'timebarsize' and 'samplength' are already defined.

  # --------------------------------------------------------------------------
  # II.1 CALCULATE NUMBER OF DAYS (ndays), TOTAL VOLUME (totvol), AND VBS
  # --------------------------------------------------------------------------
  # Definitions:
  #   ndays  : number of distinct trading days in 'minutebars'
  #   totvol : total traded volume over the whole sample
  #   VBS    : volume bucket size = average daily volume / buckets per day
  #
  # This follows the standard VPIN construction:
  #   VBS = (TotalVolume / NumberOfDays) / NumberOfBucketsPerDay
  # See Abad & Yagüe (2012) and Easley et al. (2012).

  # Number of unique trading days in the bar dataset
  ndays <- dplyr::n_distinct(as.Date(minutebars$interval, tz = "UTC"))

  # Total volume across all bars
  totvol <- sum(minutebars$tbv, na.rm = TRUE)

  # Volume bucket size (VBS): average daily volume divided by buckets per day
  vbs <- (totvol / ndays) / buckets

  # Store key parameters in the estimation object
  params <- c(timebarsize, buckets, samplength, vbs, ndays)
  names(params) <- c("tbSize", "buckets", "samplength", "VBS", "ndays")

  estimatevpin@parameters <- params

  # --------------------------------------------------------------------------
  # II.2 CALCULATE STANDARD DEVIATION OF DP (sdp)
  # --------------------------------------------------------------------------
  # We need sigma(dp) to compute the normalised price change dp/sdp, which is
  # then mapped through the standard normal CDF to get the probabilities of
  # buyer- and seller-initiated volume (zb and zs) in Step 6.
  # This is the bulk classification scheme used in the VPIN literature.

  sdp <- stats::sd(minutebars$dp, na.rm = TRUE)

  # 'vbs' and 'sdp' are now ready for subsequent steps:
  #  - vbs : to split time bars and build volume buckets
  #  - sdp : to compute zb, zs and then (bvol, svol) per time bar


  ############################################################################
  #           STEP 3 :  BREAKING UP LARGE T-MINUTE TIME BARS' VOLUME
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step4)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTION
  # --------------------------------------------------------------------------
  # Goal:
  #   Prevent a single T-second bar from spanning many volume buckets by
  #   splitting "large" bars into several smaller pseudo-bars whose volumes
  #   are at most 'threshold' (here chosen as VBS).
  #
  #   For IVPIN, we also split the *duration* of a large bar proportionally
  #   to volume so that:
  #       sum(tbv_parts)      = original tbv
  #       sum(duration_parts) = original timebarsize
  #
  # This is consistent with the usual assumption that trades are uniformly
  # distributed within the T-second bar.


  # --------------------------------------------------------------------------
  # 3.1 DEFINE THE THRESHOLD AND FIND ALL TIMEBARS WITH VOLUME > THRESHOLD
  # --------------------------------------------------------------------------

  # Assign an 'id' to each timebar to keep track of rows
  minutebars$id <- seq.int(nrow(minutebars))

  # All bars start with the full bar duration
  minutebars$duration <- timebarsize

  # Volume threshold for splitting large bars
  # Here we use threshold = VBS; if tbv > threshold, split.
  threshold <- vbs

  # Identify indices of "large" bars
  large_idx <- which(minutebars$tbv > threshold)

  # --------------------------------------------------------------------------
  # 3.2 BREAK DOWN LARGE VOLUME TIMEBARS INTO SMALLER ONES
  # --------------------------------------------------------------------------
  # For each large bar:
  #   - Let tbv_orig be its original volume.
  #   - We write tbv_orig = remainder + n_rep * threshold,
  #     where:
  #       remainder = tbv_orig %% threshold
  #       n_rep     = tbv_orig %/% threshold
  #
  #   - The original row in 'minutebars' is turned into the "remainder" bar
  #     with volume = remainder.
  #   - We create n_rep new pseudo-bars with:
  #       tbv      = threshold
  #       duration = threshold / tbv_orig * timebarsize
  #       dp, interval, id inherited from original bar.
  #
  #   - Duration of the remainder bar is:
  #       duration = remainder / tbv_orig * timebarsize
  #
  # The sum of volumes and durations over the remainder + replicas equals the
  # original tbv_orig and timebarsize, respectively.

  if (length(large_idx) > 0L) {

    # Extract large bars (before modifying minutebars)
    large <- minutebars[large_idx, , drop = FALSE]
    vol   <- large$tbv

    # Decomposition: vol = rem + n_rep * threshold
    n_rep <- vol %/% threshold          # number of full chunks
    rem   <- vol %%  threshold          # remainder volume

    ## 1) Turn original rows into "remainder" bars
    # tbv_rem = rem, duration_rem = (rem/vol)*timebarsize
    minutebars$tbv[large_idx]      <- rem
    minutebars$duration[large_idx] <- timebarsize * (rem / vol)

    ## 2) Create replicated pseudo-bars with tbv = threshold
    # duration_rep = (threshold/vol)*timebarsize
    reps <- large[rep(seq_len(nrow(large)), n_rep), , drop = FALSE]
    vol_rep <- vol[rep(seq_len(nrow(large)), n_rep)]

    reps$tbv      <- threshold
    reps$duration <- timebarsize * (threshold / vol_rep)

    ## 3) Append, sort, reindex
    minutebars <- rbind(minutebars, reps)
    # Optional: drop zero-volume remainder bars if desired
    # minutebars <- minutebars[minutebars$tbv > 0, , drop = FALSE]

    minutebars <- minutebars[order(minutebars$interval), , drop = FALSE]
    minutebars$id <- seq_len(nrow(minutebars))

    rm(large, vol, n_rep, rem, reps, vol_rep, large_idx)
  }


  ############################################################################
  #             STEP 4 : ASSIGNING T-SECOND TIME BARS INTO BUCKETS
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c = verbose, m = vpin_ms$step5)

  # --------------------------------------------------------------------------
  # IV.1 ASSIGN A BUCKET TO EACH TIMEBAR AND COMPUTE EXCESS VOLUME
  # --------------------------------------------------------------------------
  # We move from calendar-time bars (minutebars) to volume-time buckets.
  #
  # Definitions:
  #   runvol : cumulative volume over all bars, in time order
  #   bucket : integer bucket index in volume-time
  #            bucket = 1 + floor(runvol / vbs)
  #   exvol  : "excess" volume inside the current bucket
  #            exvol = runvol - (bucket - 1) * vbs
  #
  # Example (vbs = 100):
  #   runvol = 70   → bucket = 1 + 70 %/% 100 = 1 (still filling bucket 1)
  #   runvol = 472  → bucket = 1 + 472 %/% 100 = 5
  #                    exvol  = 472 - (5 - 1)*100 = 72
  #                  → 4 full buckets (400) plus 72 in bucket 5.

  # Cumulative volume over T-second bars
  minutebars$runvol <- cumsum(minutebars$tbv)

  # Assign volume-time bucket index
  minutebars$bucket <- 1L + (minutebars$runvol %/% vbs)

  # Excess volume in the current bucket
  minutebars$exvol <- minutebars$runvol - (minutebars$bucket - 1L) * vbs


  ############################################################################
  #       STEP 5 :  BALANCING TIMEBARS AND ADJUSTING BUCKET SIZES TO VBS
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step6)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTION
  # --------------------------------------------------------------------------
  # Some T-second bars "straddle" two buckets:
  #   part of their volume completes bucket k - 1,
  #   and the remaining part belongs to bucket k.
  #
  # Example (vbs = 100):
  #   interval          tbv  runvol  bucket  exvol
  #   --------------------------------------------
  #   04:52             50     90      1      90
  #   04:53             30    120      2      20
  #
  # The bar at 04:53 has tbv = 30. Bucket 1 needs 10 to reach 100,
  # and bucket 2 gets the remaining 20:
  #   - volume in bucket 1: tbv - exvol = 30 - 20 = 10
  #   - volume in bucket 2: exvol        = 20
  #
  # The second bar has tbv = 30, of which:
  #   10 belongs to bucket 1  (to fill 90 → 100)
  #   20 belongs to bucket 2  (this is exvol = 20)
  #
  # We split that bar into two pseudo-bars:
  #   2019-04-02 04:53  53.0  10   bucket 1
  #   2019-04-02 04:53  53.0  20   bucket 2
  #
  # We also split the duration proportionally to volume.

  # Ensure bars are ordered by time and bucket
  minutebars <- minutebars[order(minutebars$interval, minutebars$bucket), , drop = FALSE]

  # --------------------------------------------------------------------------
  # 5.1 FINDING THE FIRST TIMEBAR IN EACH BUCKET
  # --------------------------------------------------------------------------

  # Find the first timbar in each bucket, we will have to ignore the first
  # timebar of bucket 1 as it does not share trade volume with a previous bucket

  first_in_bucket <- !duplicated(minutebars$bucket)
  xtrarnum <- which(first_in_bucket & minutebars$bucket != 1L)

  # Copy of those first bars for later use (to create the "extra" rows)
  xtrarows <- minutebars[xtrarnum, , drop = FALSE]

  # In our example, xtrarows should contain the following timebar
  #
  #   interval          dp      tbv   runvol  bucket  exvol
  # --------------------------------------------------------
  #   2019-04-02 04:53  53.0    30.   120     2        20

  # ------------------------------------------------------------------------
  # 5.2 CHANGE THE VOLUME OF FIRST TIMEBAR IN EACH BUCKET TO EXVOL
  # ------------------------------------------------------------------------
  # For each such bar:
  #   current-bucket volume = exvol
  #   previous-bucket volume = tbv - exvol
  #
  # We keep the current rows for the *current* bucket and assign tbv = exvol.
  # Duration is scaled proportionally: duration_curr = duration * (exvol / tbv).

  # Now that we have found all the identifiers; for each minutebar having an
  # identifier in xtrarnum, change the value volume (tbv) to the value of excess
  # volume (exvol), and the duration to the proportional time corresponding to
  # the volume.

  minutebars[xtrarnum, ]$duration <- minutebars[xtrarnum, ]$duration *
    (minutebars[xtrarnum, ]$exvol/minutebars[xtrarnum, ]$tbv)
  minutebars[xtrarnum, "tbv"] <- minutebars[xtrarnum, "exvol"]


  # ------------------------------------------------------------------------
  # 5.3 CREATE EXTRA ROWS FOR THE PREVIOUS BUCKET (tbv - exvol)
  # ------------------------------------------------------------------------
  # Using the copy 'xtrarows' (original values before we changed minutebars):
  #   - tbv_prev    = tbv - exvol
  #   - duration_prev = duration * (tbv_prev / tbv)
  #   - bucket_prev = bucket - 1
  #

  tbv_prev <- xtrarows$tbv - xtrarows$exvol
  frac_prev <- tbv_prev / xtrarows$tbv         # share of original volume

  # New duration for that piece
  xtrarows$duration <- xtrarows$duration * frac_prev

  # Assign previous-bucket volume and bucket index
  xtrarows$tbv    <- tbv_prev
  xtrarows$bucket <- xtrarows$bucket - 1L

  # Keep only the relevant columns in xtrarows (same structure as minutebars)
  xtrarows <- xtrarows[, c("interval", "dp", "tbv", "id", "duration",
                           "runvol", "bucket", "exvol"), drop = FALSE]


  # ------------------------------------------------------------------------
  # 5.4 APPEND EXTRA ROWS AND RECOMPUTE RUNVOL & EXVOL
  # ------------------------------------------------------------------------

  # We add the replicated timebars in xtrarows to the main dataset
  # 'minutebars'and sort it by interval and by bucket so we have all bucket
  # minutebars next to one another.

  # Add the new "previous-bucket" rows
  minutebars <- rbind(minutebars, xtrarows)

  # Sort so that bucket-time order is respected
  minutebars <- minutebars[order(minutebars$interval, minutebars$bucket), , drop = FALSE]

  # Recompute runvol and exvol with the updated tbv / bucket structure
  minutebars$runvol <- cumsum(minutebars$tbv)
  minutebars$exvol  <- minutebars$runvol - (minutebars$bucket - 1L) * vbs

  # Reset rownames
  rownames(minutebars) <- NULL

  rm(xtrarnum, xtrarows, tbv_prev, frac_prev)



  ############################################################################
  #             STEP 6 :  CALCULATING AGGREGATE BUCKET DATA
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step7)

  # --------------------------------------------------------------------------
  # 6.1 ASSIGN N(0,1) PROBABILITIES AND BUY/SELL VOLUMES PER TIMEBAR
  # --------------------------------------------------------------------------
  # We use a simple bulk-classification scheme:
  #   z  = dp / sdp
  #   zb = Φ(z)          → prob. bar is buyer-initiated
  #   zs = 1 - Φ(z)      → prob. bar is seller-initiated
  #
  # Buy / sell volume per bar:
  #   bvol = tbv * zb
  #   svol = tbv * zs

  z  <- minutebars$dp / sdp
  zb <- pnorm(z)

  minutebars$zb   <- zb
  minutebars$zs   <- 1 - zb
  minutebars$bvol <- minutebars$tbv * minutebars$zb
  minutebars$svol <- minutebars$tbv * minutebars$zs

  # --------------------------------------------------------------------------
  # 6.2 CALCULATE AGGREGATE BUCKET DATA
  # --------------------------------------------------------------------------
  # For each volume bucket we want:
  #   agg.bvol  : total buy volume
  #   agg.svol  : total sell volume
  #   aoi       : |agg.bvol - agg.svol| = absolute order imbalance
  #   duration  : total duration in calendar time (seconds)
  #   starttime : time of first bar in the bucket
  #   endtime   : time of last bar in the bucket

  bucketdata <- minutebars %>%
    dplyr::arrange(bucket, interval) %>%
    dplyr::group_by(bucket) %>%
    dplyr::summarise(
      agg.bvol  = sum(bvol,     na.rm = TRUE),
      agg.svol  = sum(svol,     na.rm = TRUE),
      duration  = sum(duration, na.rm = TRUE),
      starttime = dplyr::first(interval),
      .groups   = "drop"
    )

  bucketdata$aoi <- abs(bucketdata$agg.bvol - bucketdata$agg.svol)

  # --------------------------------------------------------------------------
  # 6.3 ENFORCE CONTIGUOUS BUCKET TIMES
  # --------------------------------------------------------------------------
  # Bucket 1 keeps its observed starttime.
  # Bucket k starts after all previous bucket durations have elapsed.
  # endtime = starttime + duration.

  ref_start <- bucketdata$starttime[1]

  # cumulative duration of previous buckets
  cum_prev_duration <- c(0, cumsum(head(bucketdata$duration, -1)))

  bucketdata$starttime <- ref_start + cum_prev_duration
  bucketdata$endtime   <- bucketdata$starttime + bucketdata$duration



  ############################################################################
  #             STEP 7 :  CALCULATING VPIN VECTOR FOLLOWING EPO 2012
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose & !improved, m = vpin_ms$step8)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTON
  # --------------------------------------------------------------------------

  # The calculation of the VPIN vector uses + the vector of order imbalances
  # (OI), the sample length (samplength) and the volume bucket size (vbs).
  # Assume for now that samplength is 50. The formula for the first VPIN
  # observation is sum(OI, i=1 to 50)/(50*vbs).
  # The formula for the 5th VPIN observation is sum(OI, i=5 to 54)/(50*vbs).

  # Note that sum(OI, i=5 to 54) = sum(OI, i=1 to 54) - sum(OI, i=1 to 4)
  # The first one is the cumulative sum at 54 and the second one is cumulative
  # sum at 4. So sum(OI, i=5 to 54) = cumsum[54]-cum[4].
  # The general formula is sum(OI, i=n to 50+n) = cumsum[50+n]-cum[n].
  #
  # So we can calculate VPIN for the bucket 50, we shift the cumsum by 1
  # position. We calculate first the cumulative sum for OI and then use the
  # formula to find the value of VPIN

  # --------------------------------------------------------------------------
  # 7.1 CALCULATING VPIN VECTOR
  # --------------------------------------------------------------------------
  # VPIN is computed over a rolling window of 'samplength' volume buckets:
  #
  #   OI_k   = aoi_k = |agg.bvol_k - agg.svol_k|
  #   VPIN_k = (1 / (samplength * vbs)) * sum_{j = k-samplength+1}^k OI_j
  #
  # We implement this with cumulative sums of OI.

  if (nrow(bucketdata) < samplength) {

    estimatevpin@success <- FALSE

    estimatevpin@errorMessage <- vpin_err$largesamplength

    ux$show(c= verbose, m = vpin_ms$aborted)

    ux$show(ux$line())

    ux$stopnow(m = vpin_err$largesamplength, s = vpin_err$fn)

  }

  # Ensure buckets are ordered
  bucketdata <- bucketdata[order(bucketdata$bucket), , drop = FALSE]

  # Calculate the cumulative sum of OI: cumoi
  bucketdata$cumoi <- cumsum(bucketdata$aoi)

  # Lagged cumulative OI for a window of length 'samplength'
  # Construction:
  #   - first (samplength-1) entries remain NA
  #   - at index = samplength: lagcumoi = 0  (sum from 1 to samplength)
  #   - afterwards: lagcumoi[k] = cumoi[k - samplength]
  lagcumoi <- c(
    rep(NA_real_, samplength - 1L), 0, head(bucketdata$cumoi, -samplength)
  )

  bucketdata$lagcumoi <- lagcumoi

  # VPIN: windowed sum of OI divided by (samplength * vbs)
  bucketdata$vpin <- with(bucketdata, (cumoi - lagcumoi) / (samplength * vbs))

  # --------------------------------------------------------------------------
  # 7.2 ADJUST BUCKET DURATIONS FOR LIMITED TRADING HOURS
  # --------------------------------------------------------------------------
  # If the market trades only 'tradinghours' per day (e.g. 8 out of 24),
  # the raw bucket duration (in seconds) may include overnight non-trading time
  # when a bucket spans more than one calendar day.
  #
  # For each bucket with duration D (seconds), we decompose:
  #   D   = days * 24h + R,  where
  #   days = number of full 24h cycles
  #   R    = remainder in [0, 24h)
  #
  # Within each full day, only 'tradinghours' hours are trading time.
  # Let:
  #   TH  = tradinghours * 3600                  # trading seconds per day
  #   NTH = (24 - tradinghours) * 3600           # non-trading seconds per day
  #
  # The effective trading duration of the bucket is:
  #
  #   effective_trading_duration =
  #       days * TH          # trading time from full days
  #     + (R %% NTH)         # trading time from the partial day remainder
  #
  # Intuition for R %% NTH:
  #   - If the remainder R does not cross the non-trading block, it is all
  #     trading time ⇒ trading_from_R = R.
  #   - If the remainder is long enough to contain the entire non-trading block
  #     (of length NTH), then it consists of:
  #         NTH seconds non-trading + (R - NTH) seconds trading
  #     ⇒ trading_from_R = R - NTH.
  #   - Both cases are captured by R %% NTH.
  #
  # If tradinghours is 0 or 24 (no trading or 24h trading), we skip the adjustment.

  if (tradinghours > 0 && tradinghours < 24) {

    day_sec <- 24 * 3600
    TH      <- tradinghours * 3600
    NTH     <- (24 - tradinghours) * 3600

    # Decompose each bucket duration into full 24h cycles and a remainder
    days <- bucketdata$duration %/% day_sec
    R    <- bucketdata$duration %%  day_sec

    # Replace raw duration by effective trading duration
    bucketdata$duration <- days * TH + (R %% NTH)
  }



  # --------------------------------------------------------------------------
  # 7.3 STORE VPIN VECTOR
  # --------------------------------------------------------------------------

  estimatevpin@vpin <- bucketdata$vpin


  # --------------------------------------------------------------------------
  # 7.4 DAILY VPIN: UNWEIGHTED AND DURATION-WEIGHTED
  # --------------------------------------------------------------------------
  # For each calendar day (based on bucket start time) we compute:
  #   dvpin          : daily average of bucket VPINs
  #   dwvpin         : daily VPIN weighted by bucket duration

  dailyvpin <- bucketdata %>%
    mutate(day = as.Date(starttime, tz = "UTC")) %>%
    group_by(day) %>%
    dplyr::summarise(
      dvpin          = mean(vpin, na.rm = TRUE),
      sumD           = sum(duration, na.rm = TRUE),
      sumvD          = sum(vpin * duration, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    mutate(dwvpin = sumvD / sumD) %>%
    dplyr::select(day, dvpin, dwvpin)


  # --------------------------------------------------------------------------
  # 7.5 CLEAN-UP AND STORE RESULTS
  # --------------------------------------------------------------------------
  # At this stage:
  #   - bucketdata contains per-bucket VPIN inputs and outputs,
  #   - dailyvpin contains per-day VPIN summaries.
  # We now:
  #   (i) remove intermediate columns that were only needed for rolling sums,
  #   (ii) store the final objects in the estimation container,
  #   (iii) record running time (if not using the improved IVPIN branch).

  # Drop intermediate cumulative-sum columns no longer needed downstream
  columnstodrop <- c("cumoi", "lagcumoi")
  bucketdata <- bucketdata[, !(names(bucketdata) %in% columnstodrop), drop = FALSE]

  # Store final bucket-level and daily-level results in the S4 object
  estimatevpin@bucketdata <- bucketdata
  estimatevpin@dailyvpin  <- dailyvpin

  # Record total running time for the VPIN estimation
  time_off <- Sys.time()

  # If we are not continuing into the improved (IVPIN) part,
  # finalize and return the VPIN estimation object here.
  if (!improved) {
    estimatevpin@runningtime <- ux$timediff(time_on, time_off)
    ux$show(c = verbose, m = vpin_ms$complete)
    return(estimatevpin)
  }


  ############################################################################
  #        STEP 8 :  CALCULATING IVPIN VECTOR FOLLOWING KE & LIN 2017
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose & improved, m = vpin_ms$step8)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTION
  # --------------------------------------------------------------------------

  # The calculation of the IVPIN vector uses the vector of order imbalances
  # (OI = agg.bvol - agg.svol), the sample length (samplength), the volume
  # bucket size (vbs), and the vector of duration (duration).

  # Easley et al. (2012b) derived the VPIN estimator based on two moment
  # conditions from Poisson processes:
  # E[|VB - VS|] ≈ alpha * mu and E[|VB + VS|] = 2 * epsilon + alpha * mu.
  # Ke et al. (2017) suggested expressing these conditions as:
  # (EQ1) E[|VB - VS| | t; theta] ≈ alpha * mu * t, and
  # (EQ2) E[|VB + VS| | t; theta] = (2 * epsilon + alpha * mu) * t.
  # These equations correspond to Equations (3) on page 364 of Ke et al. (2017).

  # In our implementation:
  # - The variable "total_arrival_rate" or 'tar' is calculated as |VB + VS|/t.
  #   Its average would, therefore, represent 2 * epsilon + alpha * mu.
  # - The variable "informed_arrival_rate" or 'iar' is calculated as |VB - VS|/t.
  #   Its average would, therefore, approximate alpha * mu.

  # These two variables 'tar' and 'iar' allow us to estimate mu using (EQ1),
  # where mu = mean(iar) / alpha.
  # Once mu is estimated, we can determine epsilon using (EQ2), where
  # eps = mean(tar - iar) / 2.

  # In the first step, we calculate for each bucket, mean(iar) and mean(tar - iar),
  # which correspond to the average total arrival rate and uninformed arrival rate
  # for the last 'samplength' buckets.

  # Let's start by calculating the variables 'tar' and 'iar'
  # Per-bucket arrival rates (Ke & Lin, 2017, eq. (3))
  #   tar = (VB + VS) / t   ≈ 2ε + αμ   (total order arrival rate)
  #   iar = |VB − VS| / t   ≈ αμ        (informed arrival rate)
  # where:
  #   VB, VS : bucket buy/sell volumes (agg.bvol, agg.svol)
  #   t      : bucket duration in calendar time (seconds)

  # Avoid division by zero or near-zero duration
  eps_t <- 1e-8

  # Total arrival rate (TAR): (VB + VS) / t
  # In expectation: E[TAR] = 2*epsilon + alpha*mu (Eq. (3), Ke & Lin 2017).
  bucketdata$tar <- with (
    bucketdata, (agg.bvol + agg.svol)/pmax(duration, eps_t)
  )

  # Informed arrival rate (IAR): |VB - VS| / t
  # In expectation: E[IAR] ≈ alpha*mu (Eq. (3)).
  bucketdata$iar <- with (
    bucketdata, abs(agg.bvol - agg.svol)/pmax(duration, eps_t)
  )


  # To calculate the mean 'iar' and 'tar' over the last 'samplength' buckets,
  # we use the following strategy.
  # Assume samplength is 50. The formula for the first average 'tar'
  # observation is sum(tar[i], i = 1 to 50) / 50.
  # The formula for the 5th average 'tar' is sum(tar[i], i = 5 to 54) / 50.

  # Note that:
  # sum(tar[i], i=5 to 54) = sum(tar[i], i=1 to 54) - sum(tar[i], i=1 to 4).
  # The first one is the cumulative sum at 54 and the second one is the
  # cumulative sum at 4. So sum(tar[i], i=5 to 54) = cumsum[54] - cumsum[4].
  # The general formula is sum(tar[i], i=n to 50+n) = cumsum[50+n] - cumsum[n].

  # So, to calculate the mean 'tar' and mean 'iar' for the bucket 50, we shift
  # the cumsum by 1 position.

  # --------------------------------------------------------------------------
  # 8.1 CALCULATING AVERAGE TAR AND IAR VECTOR
  # --------------------------------------------------------------------------

  # Calculate the cumulative sum of 'TAR' and 'IAR': cumTAR and cumIAR

  if (nrow(bucketdata) < samplength) {

    estimatevpin@success <- FALSE

    estimatevpin@errorMessage <- vpin_err$largesamplength

    ux$show(c= verbose, m = vpin_ms$aborted)

    ux$show(ux$line())

    ux$stopnow(m = vpin_err$largesamplength, s = vpin_err$fn)

  }

  # Rolling averages of TAR and IAR over the last 'samplength' buckets
  # For bucket k (k ≥ samplength):
  #   avIAR_k = (1/L) * sum_{j=k-L+1}^k IAR_j
  #   avTAR_k = (1/L) * sum_{j=k-L+1}^k TAR_j
  #   avUAR_k = avTAR_k - avIAR_k  ≈ 2*epsilon
  L <- samplength

  cumTAR <- cumsum(bucketdata$tar)
  cumIAR <- cumsum(bucketdata$iar)

  lagcumTAR <- c(rep(NA_real_, L - 1L), 0, head(cumTAR, -L))
  lagcumIAR <- c(rep(NA_real_, L - 1L), 0, head(cumIAR, -L))


  # Recall our objective:
  # These two variables 'tar' and 'iar' allow us to estimate mu using (EQ1),
  # where mu = mean(iar) / alpha.
  # Once mu is estimated, we can determine epsilon using (EQ2), where
  # eps = mean(tar - iar) / 2.
  # These averages are taken over the last 'samplength' buckets.

  # The variable avIAR = mean(iar) for the last 'samplength' (say 50) buckets
  # can be easily obtained for bucket k by taking the sum of 'iar' over buckets
  # k-49 to bucket k, and dividing this sum by 50.
  # This sum at bucket k can be easily obtained by cumIAR[k] - lagcumIAR[k].

  # The variable avUAR = mean(tar - iar) for the last 'samplength' (say 50)
  # buckets, can be easily obtained for bucket k by subtracting the sum of 'iar'
  # over the buckets k-49 to bucket k from the sum of 'tar' over buckets k-49 to
  # bucket k, and dividing this sum by 50.
  # These sums at bucket k can be easily obtained by cumTAR[k] - lagcumTAR[k]
  # and cumIAR[k] - lagcumIAR[k].

  bucketdata$avIAR <- (cumIAR - lagcumIAR) / L
  bucketdata$avTAR <- (cumTAR - lagcumTAR) / L
  bucketdata$avUAR <- bucketdata$avTAR - bucketdata$avIAR

  # Now, we have obtained the variables we need to calculate mu and alpha.
  # (EQ1) becomes mu (at bucket k) = avIAR[k] / alpha.
  # (EQ2) becomes eps (at bucket k) = avUAR[k] / 2.


  # We can, therefore, delete the other variables
  cumIAR <- lagcumIAR <- bucketdata$avTAR <- NULL
  cumTAR <- lagcumTAR <- bucketdata$tar <- bucketdata$iar <- NULL

  # --------------------------------------------------------------------------
  # 8.2 MAXIMUM-LIKELIHOOD ESTIMATION
  # --------------------------------------------------------------------------

  # Implementation of this step involves the following:
  # We will loop over the buckets, collecting the necessary elements for the
  # maximum likelihood estimation for each bucket. These elements include:
  # 1. A dataframe composed of agg.bvol, agg.svol, and duration for the last
  #    'samplength' buckets.
  # 2. The values of avIAR and avUIR.

  # The value of avIAR will help us create a grid of initial parameter sets.
  # The values of alpha and delta are chosen within the interval (0, 1) following
  # the grid algorithm of Yang and Zhang (2012) and used by Ke and Lin (2017).
  # We use avIAR to estimate mu, given by (EQ1) above: mu = avIAR / alpha.
  # The value of eps is given by (EQ2) and is simply: eps = avUIR / 2.
  # For each bucket, we start by creating a set of initial parameter sets in
  # a dataframe called 'initials'.

  # We extract the relevant dataframe for the maximum likelihood estimation,
  # which consists of the variables agg.bvol, agg.svol, and duration for the
  # last 'samplength' buckets.
  # mldata <- buckets[(k - samplength + 1):k, c("agg.bvol", "agg.svol", "duration")]

  # For the dataframe 'initials', we apply a function 'findMLE'. The function
  # sapply() takes the values of the initial parameters, row by row, and sends
  # them alongside the dataframe mldata to the function findMLE().

  # The function findMLE receives the initial parameter set, the dataframe
  # mldata, and finds the likelihood-maximizing estimate. It returns a vector of
  # 4 estimates (alpha, delta, mu, eps) and the value of the likelihood.
  # Once we have all results, we pick the row that has the highest likelihood.
  # We then use the corresponding variables to calculate the value of ivpin using
  # Equation (19) in Ke and Lin (2017), page 370.

  # ivpin = (alpha* * mu*) / (eps.b* + eps.s* + mu*)

  # Define the function findMLE

  findMLE <- function(params, mlefn, s = FALSE){

    # If starting params already give non-finite nll, bail out immediately
    # nll: negative loglikelihood
    # return a clearly-bad "solution" with conv = 0

    base_nll <- mlefn(params)
    if (!is.finite(base_nll)) return(c(params, ll = -Inf, conv = 0L))

    # Calculate the default value for the function in case the estimation fails
    # or if the returned likelihood is infinite. This default value consists of
    # the initial parameter set and the likelihood value calculated from these
    # initial parameters. The estimation is considered to have failed if the
    # convergence value is below zero.

    default_value <- c(params, ll = -base_nll, conv = 0L)

    optimal <- tryCatch({
      lwbd <-  c( 1e-4,  1e-4, 1e-6, 1e-6, 1e-6)
      uppbd <- c(1-1e-4, 1-1e-4, Inf, Inf, Inf)
      est <- suppressWarnings(
        nloptr::neldermead(params, fn = mlefn, lower = lwbd, upper = uppbd)
      )

      status <- est$convergence

      # optimisation failed or returned nonsense → fall back

      if (!is.numeric(status) || status <= 0L || !is.finite(est$value)) {
        return(default_value)
      }

      # est$value is nll; we store ll = -nll
      c(est$par, ll = -est$value, conv = status)

    },
    error = function(e) default_value
    )

    return(optimal)

  }

  # Initialize the progress bar
  if (verbose) {
    pb_ivpin <- ux$progressbar(0, maxvalue = nrow(bucketdata) - L + 1)
    cat(uix$vpin()$progressbar)
  }

  # Create an NA vector of ivpin values
  bucketdata$ivpin <- NA

  # create a variable to store the optimal value from the previous optimization
  # The previous optimal parameters are pre_optimal
  pre_optimal <- NULL

  # We construct the grid of alpha_values and delta values here, once for all,
  # and we call it each time we need it in the for loop. We use the argument
  # grid_size, which sets the granularity of the partition of the parameter
  # space for alpha and delta.

  # Generate the set of values for alpha and delta:
  # (0.1, 0.3, 0.5, 0.7, 0.9) when grid_size is equal to 5.
  hstep <- 1 / (2 * grid_size)
  grid <- seq(hstep, 1 - hstep, 2 * hstep)
  base_grid <- expand.grid(alpha = grid, delta = grid)


  for(k in L:nrow(bucketdata)){

    # Obtain mldata which consists of Vb, Vs and t
    mldata <- bucketdata[(k - L + 1):k, c("agg.bvol", "agg.svol", "duration")]

    # Load the factorization of the likelihood function for the IVPIN model.
    # Details of the factorization can be found in footnote 8 on page 369 of
    # the paper.

    mlefn <- factorizations$ivpin(mldata)

    # If the variable pre_optimal is not NULL, we use it as the initial
    # value for the optimization. If the optimization is successful, i.e., the
    # last element of the output from the function findMLE is equal to 1, then
    # the first five values of the output represent the new optimal values for
    # the current bucket, and also become the new "pre_optimal" values
    # for the next iteration. We then move to next bucket. If the optimization
    # is not successful, we continue with the grid search.



    if (!is.null(pre_optimal)) {

      optimal <- as.list(findMLE(pre_optimal, mlefn))
      names(optimal) <- c("alpha", "delta", "mu", "eb", "es", "loglik", "conv")

      if(optimal$conv > 0){
        bucketdata$ivpin[k] <- with(optimal, (alpha * mu)/(eb + es + mu))
        pre_optimal <- unlist(optimal[1:5])
        # Update the progressbar
        if (verbose) setTxtProgressBar(pb_ivpin, k)
        next
      }
    }

    ############################################################################
    # THIS IS THE GRID #########################################################
    ############################################################################

    # Obtain currentavUAR and currentavIAR, which represent the current average
    # uninformed arrival rate and the current average informed arrival rate. We
    # use them to calculate the values of mu, eb, and es.
    # Recall from (EQ1) mu = avIAR / alpha and from (EQ2) eb = es = avUAR / 2.

    currentavUAR <- bucketdata$avUAR[k]
    currentavIAR <- bucketdata$avIAR[k]

    initials <- base_grid # The base grid is calculated before the for loop
    colnames(initials) <- c("alpha", "delta")

    # Now we add the values of mu, eb, and es using (EQ1) and (EQ2)
    initials$mu <- currentavIAR/initials$alpha
    initials$eps.b <- initials$eps.s <- currentavUAR/2

    # Now, we have everything we need to find the likelihood-maximizing
    # parameters starting from each initial parameter set.
    # Apply the function findMLE using apply(), pass mldata as an additional
    # argument, and store the results in a dataframe called 'results'.
    # This dataframe will contain six columns: the first five are the optimal
    # parameters (alpha*, delta*, mu*, eps.b*, eps.s*), and the last one is the
    # likelihood value 'likelihood'. If the optimization of the likelihood
    # function fails, the initial parameters are returned along with the
    # likelihood value calculated at these parameters. This ensures that we do
    # not have NA or infinite values.

    results <- as.data.frame(t(apply(
      initials, 1, function(row){findMLE(row, mlefn)})))
    colnames(results) <- c("alpha", "delta", "mu", "eb", "es", "loglik", "conv")

    # We select the entries for which the variable 'conv' is larger then 0,
    # indicating that the optimization algorithm has not failed, and save them
    # in a dataframe called 'optresults'. If this dataframe is empty, we use
    # the dataframe of the initial parameter sets with the corresponding
    # likelihood values.

    optresults <- results[results$conv > 0,]

    # Further filter out invalid (boundary) parameters and infinite likelihoods

    optresults <- subset(
      optresults, alpha > 0 & alpha < 1 & eb > 0 & es > 0 & is.finite(loglik))

    if (nrow(optresults) == 0L) optresults <- results

    # Pick the row from 'optresults' that has the highest log-likelihood

    optimalrow <- optresults[which.max(optresults$loglik), ]

    # We calculate IVPIN for bucket k (Ke & Lin eq. (19), pp 370)

    bucketdata$ivpin[k] <- with(optimalrow, (alpha * mu)/(eb + es + mu))

    # We store the optimal parameters in 'pre_optimal' for use in next iteration

    pre_optimal <- unlist(optimalrow[1:5])

    # Update the progressbar

    if (verbose) setTxtProgressBar(pb_ivpin, k)

  }

  ux$show(c= verbose, m = paste("\n", vpin_ms$step9, sep=""))

  # Store the vector of ivpin in the ivpin results vector
  estimatevpin@ivpin <- bucketdata$ivpin

  estimatevpin@bucketdata <- bucketdata

  time_off <- Sys.time()

  estimatevpin@runningtime <- ux$timediff(time_on, time_off)

  ux$show(c= verbose, m = vpin_ms$complete)

  return(estimatevpin)

}

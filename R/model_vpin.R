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
##    2024-10-22
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
#' @return Returns an object of class \code{estimate.vpin}, which
#' contains the following slots:
#' \describe{
#'   \item{\code{@improved}}{ A logical variable that takes the value `FALSE`
#'   when the classical VPIN model is estimated (using `vpin()`), and `TRUE`
#'   when the improved VPIN model is estimated (using `ivpin()`).}
#'   \item{\code{@bucketdata}}{ A data frame created as in
#' \insertCite{abad2012;textual}{PINstimation}.}
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
#' summary(estimate@vpin)
#'
#' # Store the computed data of the different buckets in a dataframe 'buckets'
#' # and display the first 10 rows of the dataframe.
#'
#' buckets <- estimate@bucketdata
#' show(head(buckets, 10))
#'
#' # Display the first 10 rows of the dataframe 'dayvpin'.
#' dayvpin <- estimate@dailyvpin
#' show(head(dayvpin, 10))
#'
#'
#' ### Estimation of the IVPIN model ###
#'
#' # Estimate the IVPIN model using the same parameters as above.
#' # The grid_size parameter is unspecified and will default to 5.
#'
#' iestimate <- ivpin(xdata, timebarsize = 300, samplength = 250, verbose = FALSE)
#'
#' # Display the summary statistics of the IVPIN vector
#' summary(iestimate@ivpin)
#'
#' # The output of ivpin() also contains the VPIN vector in the @vpin slot.
#' # Plot the VPIN and IVPIN vectors in the same plot using the iestimate object.
#'
#' # Define the range for the VPIN and IVPIN vectors, removing NAs.
#' vpin_range <- range(c(iestimate@vpin, iestimate@ivpin), na.rm = TRUE)
#'
#' # Plot the VPIN vector in blue
#' plot(iestimate@vpin, type = "l", col = "blue", ylim = vpin_range,
#'      ylab = "Value", xlab = "Bucket", main = "Plot of VPIN and IVPIN")
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
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by mutate
#' @importFrom stats lag
#' @importFrom tidyr fill
#' @aliases vpin ivpin
#' @name vpin_measures
NULL


#' @rdname vpin_measures
#' @aliases vpin ivpin
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
#' @aliases vpin ivpin
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

  # We want only three columns: timestamp, price and volume so we import into
  # the dataset only the three first columns

  dataset <- data[, 1:3]

  # We rename the first three columns to "timestamp", "price" and "volume"

  colnames(dataset) <- c("timestamp", "price", "volume")

  # We check if the columns price and volume are numeric so that we can make
  # operations on them. If they are not, convert them to numeric.

  dataset$price <- as.numeric(dataset$price)

  dataset$volume <- as.numeric(dataset$volume)

  dataset$timestamp <- as.POSIXct(dataset$timestamp, origin = "1970-01-01",
                                  tz = "UTC")

  rownames(dataset) <- NULL

  # If the argument 'timebarsize' is larger than the total duration of the
  # datasets in milliseconds, the abort.
  alltime <- 1000 * as.numeric(difftime(max(dataset$timestamp),
                                        min(dataset$timestamp), units = "secs"))

  if (timebarsize >= alltime) {
    ux$show(m = vpin_ms$aborted, warning = TRUE)
    ux$stopnow(m = vpin_err$largetimebarsize, s = vpin_err$fn)
  }


  ##############################################################################
  #             STEP 1 : CREATING THE T-SECOND TIMEBARS DATASET
  ##############################################################################

  # ----------------------------------------------------------------------------
  # USER MESSAGE
  # ----------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step2, skip = FALSE)

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

  # We need to find the trading hours. We'll assume that trading starts
  # at the hour of the day when the earliest trade occurred, and ends
  # at the hour of the day when the latest trade occurred across all days.

  # We will use "1970-01-01" as the origin for POSIXct conversion, so we will
  # give it a shorter name: orig. Similarly, we will use the timezone UTC, and
  # two formats: "%H:%M:%S", which we call hf for hour format; and the format
  # "%Y-%m-%d %H:%M:%S" which we call ff for full format.
  orig = "1970-01-01"
  hf <- "%H:%M:%S"
  ff <- "%Y-%m-%d %H:%M:%S"


  # Finding trading hours
  # ----------------------------------------------------------------------------

  # Find the bounds of trading dataset
  earliest_time <- as.POSIXct(min(dataset$timestamp),
                              origin = orig, tz = "UTC")
  latest_time <- as.POSIXct(max(dataset$timestamp),
                            origin = orig, tz = "UTC")


  # Partitioning the trading time into timebars requires considering time,
  # not just trade data. Timebars, even with no trades, impact the duration
  # of buckets. We'll create the 'partition' dataframe to divide the entire
  # trading hours into timebars. Non-trading days are excluded, and we focus
  # on unique days that have at least one trade.

  # Create a data frame with all possible intervals for each unique trading day
  partition <- data.frame(interval = levels(
    cut(seq(earliest_time, latest_time, by = tbsize), breaks = tbsize)))

  # Extract the intervals from partition as POSIXct format.
  intervals <- as.POSIXct(partition$interval, origin = orig, format = ff, tz = "UTC")

  # Fix any issue where POSIXct conversion causes dates at 00:00:00 to become NA
  intervals[1] <- earliest_time

  dataset$interval <- cut(dataset$timestamp, breaks = tbsize)

  dataset$interval <-  as.POSIXct(dataset$interval, origin = orig, format = ff,
                                  tz = "UTC")

  # Now we merge the dataset with 'partition' to obtain one dataset where
  # the entire trading time is partitioned into timebars. For timebars with
  # no trades, the price and volume will be set to 0. The timestamp will
  # default to the start of the interval to avoid NA values.

  # Merging the dataset with the 'partition' dataframe
  # ----------------------------------------------------------------------------

  # Merge the original dataset with the 'partition' dataframe
  partition$interval <- intervals
  dataset <- merge(partition, dataset, by = "interval", all.x = TRUE)


  # Finally, all missing volume observations are set to 'NA'. We need
  # to set them equal to zero. All missing price observations are set to 'NA'
  # we need to replace them with the latest positive price.
  dataset <- dataset %>%
    tidyr::fill(price, .direction = "down")
  dataset$volume[is.na(dataset$volume)] <- 0
  dataset$timestamp[is.na(dataset$timestamp)] <- dataset$interval[
    is.na(dataset$timestamp)]

  # For the dataset included in this package 'hfdata', we get the following
  # output
  # ......................................................................
  #            interval           timestamp   price volume
  # 2018-10-18 00:16:33 2018-10-18 00:16:33 15.4754      0
  # 2018-10-18 00:17:33 2018-10-18 00:17:52 15.5143      4
  # 2018-10-18 00:18:33 2018-10-18 00:18:33 15.5143      0
  # 2018-10-18 00:19:33 2018-10-18 00:19:33 15.5143      0
  # 2018-10-18 00:20:33 2018-10-18 00:21:01 15.5364    296
  # 2018-10-18 00:20:33 2018-10-18 00:21:01 15.5143     33
  # 2018-10-18 00:20:33 2018-10-18 00:21:05 15.4465    328
  # 2018-10-18 00:21:33 2018-10-18 00:21:33 15.4465      0




  # ----------------------------------------------------------------------------
  # I.2 CREATE THE T-MINUTE TIMEBAR DATASET CALLED 'MINUTEBARS'
  # ----------------------------------------------------------------------------

  # Create t-minute timebar dataset which will aggregate the transaction volume
  # (tbv: timebar volume) and price change: last price - first price (dp) per
  # interval


  # ----------------------------------------------------------------------------
  # -       THIS PART IS JUST TO ESTIMATE RUNNING TIME - NO OUTPUT USED        -
  # -                                                                          -
  diffseconds <- function(time_on, time_off) {
    dsecs <- difftime(time_off, time_on, units = "secs")
    return(dsecs)
  }

  if (nrow(dataset) >= 50000) {

    temptime_on <- Sys.time()

    chunk <- 5000

    tempbars <- aggregate(price ~ interval, data = dataset[1:chunk, ],
      FUN = function(x) dp <- tail(x, 1) - head(x, 1)
    )
    tempbars <- merge(tempbars,
                      aggregate(volume ~ interval, dataset[1:chunk, ], sum),
                      by = "interval"
    )
    tempbars$interval <- as.POSIXct(tempbars$interval, origin = orig, tz = "UTC")

    temptime_off <- Sys.time()

    exptime <-  ux$timediff(temptime_on, temptime_off,
                              5*log2(nrow(dataset) / (chunk)))

    ux$show(c= verbose, m = paste("[~", ceiling(exptime), "seconds]"))

  } else {

    ux$show(c= verbose, m = "")
  }

  # -                                                                          -
  # ----------------------------------------------------------------------------

  minutebars <- aggregate(price ~ interval, data = dataset,
    FUN = function(x) dp <- tail(x, 1) - head(x, 1)
  )
  minutebars <- merge(minutebars,
                      aggregate(volume ~ interval, dataset, sum),
                      by = "interval")
  minutebars$interval <- as.POSIXct(minutebars$interval, origin = orig, tz = "UTC")

  colnames(minutebars) <- c("interval", "dp", "tbv")
  minutebars <- minutebars[complete.cases(minutebars), ]

  ############################################################################
  #             STEP 2 : CALCULATING VOLUME BUCKET SIZE AND SIGMA(DP)
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step3)

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

  estimatevpin@parameters <- params

  # --------------------------------------------------------------------------
  # II.2 CALCULATE THE STANDARD DEVIATION OF DP (SDP)
  # --------------------------------------------------------------------------

  sdp <- sd(minutebars$dp)

  ############################################################################
  #           STEP 3 :  BREAKING UP LARGE T-MINUTE TIME BARS' VOLUME
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step4)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTON
  # --------------------------------------------------------------------------

  # To avoid that one time bar spans over many buckets, we have to make sure
  # to divide all large enough timebars into 'identical' timebars but with trade
  # volume lower or equal to a given threshold. The threshold is a function of
  # vbs.

  # The threshold is chosen in the code to be the following function of vbs:
  # threshold = (1-1/x)*vbs where x is a measure of precision. If x=1, then the
  # threshold is 0;if x=2 then the threshold = 50% of vbs; if x=10 then the
  # threshold = 90% vbs.

  # We give an example here about what we intend to do. Assume that vbs= 1250
  # and x=5; the threshold is then: threshold = (1-1/5) vbs = 80% vbs =
  # 0.8*1250 = 1000. This means we will break all minutebars with volume (tbv)
  # higher than 1000 into identical timebars but with volume lower or equal
  # to threshold= 1000.

  # Imagine we have the following timebar:
  #
  #   minute              dp    tbv
  # -------------------------------
  #   2019-04-02 04:53  53.0   5340.

  # Since threshold= 1000 we want to break this timebar into six 'identical'
  # timebars (time and dp) but will volume (tbv) smaller or equal to threshold.
  # The expected result is:
  #
  #   minute              dp    tbv
  # -------------------------------
  #   2019-04-02 04:53  53.0   1000.
  #   2019-04-02 04:53  53.0   1000.
  #   2019-04-02 04:53  53.0   1000.
  #   2019-04-02 04:53  53.0   1000.
  #   2019-04-02 04:53  53.0   1000.
  #   2019-04-02 04:53  53.0    340.

  # --------------------------------------------------------------------------
  # III.1 DEFINE THE THRESHOLD AND FIND ALL TIMEBARS WITH VOLUME > THRESHOLD
  # --------------------------------------------------------------------------

  # A variable 'id' is assigned to each timebar to easily identify them in
  # future tasks.

  minutebars$id <- seq.int(nrow(minutebars))

  # Define the precision factor (x) and calculate the threshold
  x <- 1
  threshold <- vbs #(1 - 1 / x) * vbs #vbs

  # Find all timebars with volume higher than the threshold and store them in
  # largebars
  # Find the position of all these timebars in the original dataset and store
  # them in the vector largebnum

  largebars <- subset(minutebars, tbv > threshold)

  largebnum <- NULL
  if (nrow(largebars) != 0)
    largebnum <- which(minutebars$id %in% largebars$id)

  # --------------------------------------------------------------------------
  # III.2 BREAK DOWN LARGE VOLUME TIMEBARS INTO SMALLER ONES
  # --------------------------------------------------------------------------

  # We break down these large volume timebars into replicated timebars with a
  # maximum volume equal to threshold. If, for example, the timebar has volume
  # 5340 and threshold = 1000, we create five (5)'identical' timebars with a
  # volume 1000 each and and one additional timebar containing the remainder
  # volume i.e. 340.
  # Mathematically, 5 is the integer division of 5340 by 1000. The normal
  # division gives 5.34 and the integer division takes the integer part of
  # 5.34 which is 5 (5340 %/% 5). To find the remainder, we substract from
  # 5340 the product of (5340 %/% 5) and 1000.The result is
  # 5340 - (5340 %/% 1000)*1000 = 5340 - 5*1000 = 5340 - 5000 = 340. There
  # is a function for finding the remainder in R which is %%.
  # Writing 5340 %% 1000 gives 340.

  # We start by placing the remainder in the original rows for large timebars.
  # We identify these timebars in the original dataset and replace the volume
  # (tbv) but the remainder of the division of tbv by the threshold.

  # To use the timebar in our example:
  #
  #   minute              dp    tbv
  # -------------------------------
  #   2019-04-02 04:53  53.0   5340.
  #
  # Replacing the volume (tbv) by the remainder gives:
  #
  #   minute              dp    tbv
  # -------------------------------
  #   2019-04-02 04:53  53.0    340.

  # ----------------------------------------------------------------------------
  # A new addition inspired by ivpin
  # ----------------------------------------------------------------------------
  # In addition to dividing the volume of the time bars into bars with volume
  # of 'threshold', we will also divide the duration of the time bar size if
  # the time bar spans over multiple buckets.

  # As discussed above, the time bar will be divided between a remainder (340)
  # and 5 bars of volume size of 1000. The time will be divided similarly, i.e.,
  # proportionally. If 'timebarsize' is equal to 60, then the duration given to
  # the remainder bar will be (340/5340)*60, while each of the 5 bars will get
  # a duration of (1000/5340)*60 each. The difference now, is that we will update
  # the timestamp in the variable 'interval': The remainder bar will keep the
  # same timestamp, while the first of 5 bars will see its timestamp increase by
  # (340/5340)*60 seconds, the second one, by (1340/5340)*60, the third one by
  # (2340/5340)*60... and so on, this is inline with the spirit of the ivpin
  # study that assume that trades are uniformly distributed over the span of the
  # time bar. The variable 'duration' will contain the duration just discussed.
  # The duration will be calculated as above for the large time bars, i.e., for
  # the time bars with volume larger than vbs. The remaining time bars will get
  # a duration equal to 'timebarsize', 60 in our example.

  minutebars$duration <- timebarsize
  # print(sum(minutebars$duration))

  if(!is.null(largebnum)) {

    # First we update the duration, and then the volume corresponding the
    # bar holding the remainder of volume.

    minutebars[largebnum, ]$duration <- timebarsize *
      ((minutebars[largebnum, ]$tbv %% threshold) / minutebars[largebnum, ]$tbv)

    minutebars[largebnum, ]$tbv <- minutebars[largebnum, ]$tbv %% threshold

    # We will now replicate the time bar with the same price movement (dp), the
    # same interval but with trade volume equal to threshold/x (x=10 in our case).
    # The number of replications we need is the integer division of (tbv) by
    # (threshold/x). n_rep stores these numbers of replication.
    # This number for each of these rows is the integer division of (tbv) by
    # (threshold/x), i.e., largebars$tbv %/% threshold

    n_rep <- x * largebars$tbv %/% threshold

    # All what is left now is to change the value of 'tbv' in 'largebars' to
    # (threshold/x) and then replicate the timebars using the corresponding value
    # in n_rep
    # The value of the duration is calculated analogously.

    largebars$duration = ((threshold / x) / largebars$tbv) * timebarsize
    largebars$tbv <- threshold / x

    largebars <- largebars[rep(seq_len(nrow(largebars)), n_rep), ]

    rm(n_rep)

    # In our example, largebars will contain 50 replicated timebars of the
    # following timebar:
    #
    #   minute              dp    tbv
    # -------------------------------
    #   2019-04-02 04:53  53.0    100.
    #
    # Since threshold is 1000 so threshold/x = 1000/10=100; so each of our
    # timebars should have a volume of 1000. Since we have already placed the
    # remainder (340) in the original dataset, we just need to distribute the
    # remaining volume 5000 into timebars with a volume of 100, which
    # gives us 50 such timebars

    # The last step now is to add these rows to the main dataset and sort it by
    # interval so that all timebars of the same interval will be neighbors.

    minutebars <- rbind(minutebars, largebars)
    minutebars <- minutebars[order(minutebars$interval), ]


    # NEW:
    # Now we update the timestamps of the time bars after they have been divided
    # This is a bit technical but it shall become clear once operated.

    # Let's look at this timebar
    #   minute              dp    tbv      duration
    # -----------------------------------------------
    # 2018-10-18 04:31:33  0.0228  3690      60

    # It has a volume of 3690 larger than 'threshold' that is equal to 3500.849
    # Therefore it will be divided in one remainder time bar of volume 189.1505
    # (3690 %% 3500.849 = 189.1505) and 10 (x = 10) time bars of volume 350.0849
    # (threshold/x = 3500.849/10 = 350.0849). The duration assigned to the first
    # time bar (remainder) is 3.075618 ((189.1505/3690) * 60 = 3.075618); while
    # the others will have a duration of 5.692437 each ((350.0849/3690) * 60).

    #   minute              dp    tbv      duration
    # -----------------------------------------------
    # 2018-10-18 04:31:33  0.0228  189.1505  3.075618
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438
    # 2018-10-18 04:31:33  0.0228  350.0849  5.692438

    # The remains now to update the time stamps, since the first time bar took
    # 3.075618 to complete, the timestamp of the second time bar should be
    # 2018-10-18 04:31:33 + 3.075618 = 2018-10-18 04:31:36. How to do that
    # recursively? We need to find the cumulative sum of duration for each
    # timestamp (the variable interval), and then take its lagged values. This
    # way, time bars with duration of 60 will have a lagged cumulative duration
    # of zero. Once we have the lagged cumulative duration, we can just add it
    # to the timestamp (interval) and obtain a proportional and uniform
    # distribution of the duration, with updated timestamps.

    minutebars <- minutebars %>%
      group_by(interval) %>%
      mutate(cum_duration = cumsum(duration),
             lag_cum_duration = lag(cum_duration, default = 0)) %>% dplyr::ungroup()

    # We obtain:

    # minute              dp    tbv      duration     lag_cum_duration
    # --------------------------------------------------------------
    # 2018-10-18 04:31:33 0.0228  189.     3.08         0
    # 2018-10-18 04:31:33 0.0228  350.     5.69         3.08
    # 2018-10-18 04:31:33 0.0228  350.     5.69         8.77
    # 2018-10-18 04:31:33 0.0228  350.     5.69         14.5
    # 2018-10-18 04:31:33 0.0228  350.     5.69         20.2
    # 2018-10-18 04:31:33 0.0228  350.     5.69         25.8
    # 2018-10-18 04:31:33 0.0228  350.     5.69         31.5
    # 2018-10-18 04:31:33 0.0228  350.     5.69         37.2
    # 2018-10-18 04:31:33 0.0228  350.     5.69         42.9
    # 2018-10-18 04:31:33 0.0228  350.     5.69         48.6
    # 2018-10-18 04:31:33 0.0228  350.     5.69         54.3


    minutebars$interval <- minutebars$interval + minutebars$lag_cum_duration
    minutebars <- data.frame(minutebars)

    # Now, we obtain a uniform distribution of the volume and the duration
    # of large time bars over multiple smaller time bars. The result looks as
    # follows:

    # minute              dp      tbv       duration
    # ------------------------------------------------
    # 2018-10-18 04:31:33 0.0228  189.1505  3.075618
    # 2018-10-18 04:31:36 0.0228  350.0849  5.692438
    # 2018-10-18 04:31:42 0.0228  350.0849  5.692438
    # 2018-10-18 04:31:48 0.0228  350.0849  5.692438
    # 2018-10-18 04:31:53 0.0228  350.0849  5.692438
    # 2018-10-18 04:31:59 0.0228  350.0849  5.692438
    # 2018-10-18 04:32:05 0.0228  350.0849  5.692438
    # 2018-10-18 04:32:10 0.0228  350.0849  5.692438
    # 2018-10-18 04:32:16 0.0228  350.0849  5.692438
    # 2018-10-18 04:32:22 0.0228  350.0849  5.692438
    # 2018-10-18 04:32:27 0.0228  350.0849  5.692438

    minutebars$cum_duration <- minutebars$lag_cum_duration <- NULL



    # Finally, we reassign new identifiers to timebars (id) to make it easier to
    # identify them in coming tasks.

    minutebars$id <- seq.int(nrow(minutebars))
    rm(largebars, largebnum)

  }

  ############################################################################
  #             STEP 4 : ASSIGNING T-MINUTE TIME BARS INTO BUCKETS
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step5)

  # --------------------------------------------------------------------------
  # IV.1 ASSIGN A BUCKET TO EACH TIMEBAR | FIND EXCESS VOLUME FOR EACH OF THEM
  # --------------------------------------------------------------------------

  # Find the cumulative volume (runvol) over all minutebars

  minutebars$runvol <- cumsum(minutebars$tbv)

  # Find the bucket to which belongs each timbar, using integerdivision by the
  # volume bucket size (vbs). If vbs = 100 and the cumulative volume is 70; it
  # means that we have not yet filled the first bucket so the timebar should
  # belong to bucket 1. We do that using the integer division 70/100 which
  # gives 0 and to which we add 1 to obtain bucket.In general the bucket size
  # is obtained by integer-dividing the cumulative volume (runvol) by vbs and
  # adding +1. If the cumulative volume is 472, we know that it must be be in
  # bucket 5. Indeed, the formula gives us bucket 5 since 472%/%100+1=4+1 = 5

  minutebars$bucket <- 1 + minutebars$runvol %/% vbs

  # The variable excess volume (exvol) calculate the excessive volume after we
  # have filled the bucket. As in the example above, if the timebar has
  # cumulative volume runvol = 472. The excess volume should be 72; which is
  # what is left after filling 4 buckets. Since we are in bucket 5; the excess
  # volume can be obtained as 72 = runvol - (5-1)*vbs = 472 - 4*100
  # In general the formula: exvol = runvol - (bucket -1)*vbs

  minutebars$exvol <- minutebars$runvol - (minutebars$bucket - 1) * vbs

  ############################################################################
  #       STEP 5 :  BALANCING TIMEBARS AND ADJUSTING BUCKET SIZES TO VBS
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step6)

  # --------------------------------------------------------------------------
  # TASK DESCRIPTON
  # --------------------------------------------------------------------------

  # We will give attention here to timebars that occur between buckets, that is
  # have volume that belong to two buckets at the same time. Assume vbs=100 and
  # we have the following:
  #
  #   interval          dp      tbv   runvol  bucket  exvol
  # --------------------------------------------------------
  #   2019-04-02 04:52  14.0    50.   90      1       90
  #   2019-04-02 04:53  53.0    30.   120     2       20
  #
  # The timebar of the interval "2019-04-02 04:52" has a tbv of 30. A volume of
  # 10 belongs to bucket 1 that use to have 90 and needs 10 to reach vbs=100;
  # and a volume of 20 belongs to bucket 2 (which we can find in the excess
  # volume exvol). Basically, for each first timebar of a bucket, the volume
  # that belongs to the previous bucket is tbv - exvol (30-20=10 in our
  # example) and the volume that belongs to the current bucket is exvol (20 in
  # our example.).
  #
  # In order to have balanced buckets, we want our code to 'divide' the first
  # timebar of bucket 2 into 'identical' timebars one containing a volume of
  # 10 and belonging to bucket 1 and the other containing a volume of 20 and
  # belonging to bucket 2. The output shall look like.
  #
  #   interval          dp      tbv   runvol  bucket  exvol
  # --------------------------------------------------------
  #   2019-04-02 04:52  14.0    50.   90      1       90
  #   2019-04-02 04:53  53.0    10.   120     1       100
  #   2019-04-02 04:53  53.0    20.   120     2       20

  # --------------------------------------------------------------------------
  # V.1 FINDING THE FIRST TIMEBAR IN EACH BUCKET
  # --------------------------------------------------------------------------

  # Find the first timbar in each bucket, we will have to ignore the first
  # timebar of bucket 1 as it does not share trade volume with a previous bucket

  xtrarows <-
    aggregate(. ~ bucket, subset(minutebars, bucket != 1), FUN = head, 1)

  # In our example, xtrarows should contain the following timebar
  #
  #   interval          dp      tbv   runvol  bucket  exvol
  # --------------------------------------------------------
  #   2019-04-02 04:53  53.0    30.   120     2       120

  # --------------------------------------------------------------------------
  # V.2 CHANGE THE VOLUME OF FIRST TIMEBAR IN EACH BUCKET TO EXVOL
  # --------------------------------------------------------------------------

  # In order to change any row in the main dataset, we need the row identifier
  # (id). For the timebars that we have extracted in the dataframe xtrarows, we
  # find their identifiers and store them in the vector xtraNum

  xtrarnum <- which(minutebars$id %in% xtrarows$id)

  # Now that we have found all the identifiers; for each minutebar having an
  # identifier in xtrarnum, change the value volume (tbv) to the value of excess
  # volume (exvol).
  minutebars[xtrarnum, ]$duration <- minutebars[xtrarnum, ]$duration *
    (minutebars[xtrarnum, ]$exvol/minutebars[xtrarnum, ]$tbv)
  minutebars[xtrarnum, "tbv"] <- minutebars[xtrarnum, "exvol"]

  # --------------------------------------------------------------------------
  # V.3 CREATE LAST MINUTEBAR IN EACH BUCKET TO REACH VBS
  # --------------------------------------------------------------------------

  # In the task description, we have established that we need to add a replicate
  # of the first minutebar of each bucket to the previous bucket and containing
  # the volume equal to tbv - exvol. Review the task description if this is not
  # clear. All such first timebars already exist in the dataframe xtrarows. We
  # will just change the bucket number to the one before it and set tbv to
  # tbv-exvol.

  # Before that, we find how much time is spent in the last timebar in each
  # bucket, which is proportional to (tbv-exvol). Once we find it, we add it to
  # to the timestamp (interval); so that the duration calculation at the end
  # becomes correct. The same timestamp should be that of the first timebar in
  # the next bucket.


  xtrarows$interval <- xtrarows$interval + xtrarows$duration *
    ((xtrarows$tbv - xtrarows$exvol)/xtrarows$tbv)

  xtrarows$duration <-  xtrarows$duration *
    ((xtrarows$tbv - xtrarows$exvol)/xtrarows$tbv)

  minutebars[xtrarnum, ]$interval <- as.POSIXct(
    xtrarows$interval, origin = orig, tz = "UTC")

  xtrarows$tbv <- xtrarows$tbv - xtrarows$exvol
  xtrarows$bucket <- xtrarows$bucket - 1


  # Now it it time to drop the duration variable of the dataframe minutebars
  # minutebars$duration <- NULL

  # --------------------------------------------------------------------------
  # V.4 CALCULATE ACCUMULATED BUCKET VOLUME FOR THE ORIGINAL DATASET
  # --------------------------------------------------------------------------

  # We add the replicated timebars in xtrarows to the main dataset
  # 'minutebars'and sort it by interval and by bucket so we have all bucket
  # minutebars next to one another.

  xtrarows$interval <- as.POSIXct(xtrarows$interval, origin = orig, tz = "UTC")
  xtrarows <- xtrarows[c("interval", "dp", "tbv", "id", "duration", "runvol",
                         "bucket", "exvol")]

  minutebars <- rbind(minutebars, xtrarows)
  minutebars <- minutebars[order(minutebars$interval, minutebars$bucket), ]

  # We calculate accumulated bucket volume which is the exvol for the new values
  # of tbv

  minutebars$runvol <- cumsum(minutebars$tbv)
  minutebars$exvol <- minutebars$runvol - (minutebars$bucket - 1) * vbs

  # Reinitialize rownames of minutebars.
  rownames(minutebars) <- NULL

  rm(xtrarnum, xtrarows)

  ############################################################################
  #             STEP 6 :  CALCULATING AGGREGATE BUCKET DATA
  ############################################################################

  # --------------------------------------------------------------------------
  # USER MESSAGE
  # --------------------------------------------------------------------------

  ux$show(c= verbose, m = vpin_ms$step7)

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

  # --------------------------------------------------------------------------
  # VI.2 CALCULATING AGGREGATE BUCKET DATA
  # --------------------------------------------------------------------------

  # Aggregate variables
  # ++++++++++++++++++++
  # agg.bvol    : sum of buy volume (bvol) per bucket
  # agg.svol    : sum of sell volume (svol) per bucket
  # OI          : the difference between agg.bvol and agg.svol
  # init.time   : the first timebar in the bucket
  # final.time  : the last timebar in the bucket
  # +++++++++++++++++++

  bucketdata <- setNames(
    aggregate(cbind(bvol, svol, duration) ~ bucket, minutebars, sum),
                         c("bucket", "agg.bvol", "agg.svol", "duration"))
  bucketdata$aoi <- abs(bucketdata$agg.bvol - bucketdata$agg.svol)

  bucketdata$starttime <- aggregate(interval ~ bucket,
                                                minutebars, head, 1)$interval
  bucketdata$endtime <- aggregate(interval ~ bucket,
                                                minutebars, tail, 1)$interval

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
  # VII.1 CALCULATING VPIN VECTOR
  # --------------------------------------------------------------------------

  # Calculate the cumulative sum of OI: cumoi

  if (nrow(bucketdata) < samplength) {

    estimatevpin@success <- FALSE

    estimatevpin@errorMessage <- vpin_err$largesamplength

    ux$show(c= verbose, m = vpin_ms$aborted)

    ux$show(ux$line())

    ux$stopnow(m = vpin_err$largesamplength, s = vpin_err$fn)

  }

  bucketdata$cumoi <- cumsum(bucketdata$aoi)
  bucketdata$lagcumoi <- head(c(rep(NA, samplength - 1), 0, bucketdata$cumoi),
                              nrow(bucketdata))
  bucketdata$vpin <- (bucketdata$cumoi - bucketdata$lagcumoi) /
    (samplength * vbs)
  bucketdata$vpin[samplength] <- bucketdata$cumoi[samplength] /
    (samplength * vbs)
  bucketdata$bduration <- as.numeric(
    difftime(bucketdata$endtime, bucketdata$starttime, units = "secs"))

  # --------------------------------------------------------------------------
  # VII.2 CORRECTING THE DURATION VECTOR
  # --------------------------------------------------------------------------

  corduration <- function(etime, stime, duration) {

    days <- round(as.numeric(difftime(etime, stime, units = "days")))

    duration <- as.numeric(duration)

    if (days > 0) {
      dseconds <- (days - 1) * 3600 * 24 + (24 - tradinghours) * 3600
      return(duration - dseconds)
    }
    return(duration)
  }

  if (tradinghours > 0 & tradinghours < 24) {
    bucketdata$bduration <- apply(bucketdata, 1, function(x)
      corduration(x[6], x[5], x[11]))
  }

  # Store the vector of vpin in the results vector

  estimatevpin@vpin <- bucketdata$vpin # [!is.na(bucketdata$vpin)]

  # Calculate daily unweighted and weighted VPINs

  bucketdata$day <- substr(bucketdata$starttime, 1, 10)

  bucketdata$vpindur <- bucketdata$vpin * bucketdata$duration

  dailyvpin <- setNames(aggregate(vpin ~ day, bucketdata,
                        function(x) mean(x, na.rm = TRUE)), c("day", "dvpin"))

  dailyvpin$day <- as.Date(dailyvpin$day, origin = orig, tz = "UTC")

  temp <- setNames(aggregate(cbind(duration, vpindur) ~ day, bucketdata,
                            function(x) sum(x, na.rm = TRUE)),
                                      c("day", "sumD", "sumvD"))

  dailyvpin$dvpin_weighted <- temp$sumvD / temp$sumD

  rm(temp)

  # Prepare the vectors to be stored in the estimation object

  columnstodrop <- c("vpindur", "cumoi", "day", "lagcumoi")

  bucketdata <- bucketdata[, !(names(bucketdata) %in% columnstodrop)]

  estimatevpin@bucketdata <- bucketdata

  estimatevpin@dailyvpin <- dailyvpin

  time_off <- Sys.time()

  if(!improved) {

    estimatevpin@runningtime <- ux$timediff(time_on, time_off)

    ux$show(c= verbose, m = vpin_ms$complete)

    return(estimatevpin)

  }

  bucketdata$duration <- bucketdata$duration /mean(bucketdata$duration)

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

  # Easley et al. (2012b) derived the VPIN estimator based on two moment conditions
  # from Poisson processes:
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
  bucketdata$tar <- with (bucketdata, (agg.bvol + agg.svol)/duration)
  bucketdata$iar <- with (bucketdata, abs(agg.bvol - agg.svol)/duration)

  # To calculate the mean 'iar' and 'tar' over the last 'samplength' buckets,
  # we use the following strategy.
  # Assume samplength is 50. The formula for the first average 'tar'
  # observation is sum(tar[i], i = 1 to 50) / 50.
  # The formula for the 5th average 'tar' is sum(tar[i], i = 5 to 54) / 50.

  # Note that sum(tar[i], i = 5 to 54) = sum(tar[i], i = 1 to 54) - sum(tar[i], i = 1 to 4).
  # The first one is the cumulative sum at 54 and the second one is the cumulative
  # sum at 4. So sum(tar[i], i = 5 to 54) = cumsum[54] - cumsum[4].
  # The general formula is sum(tar[i], i = n to 50 + n) = cumsum[50 + n] - cumsum[n].

  # So, to calculate the mean 'tar' and mean 'iar' for the bucket 50, we shift
  # the cumsum by 1 position.

  # --------------------------------------------------------------------------
  # VIII.1 CALCULATING AVERAGE TAR AND IAR VECTOR
  # --------------------------------------------------------------------------

  # Calculate the cumulative sum of 'TAR' and 'IAR': cumTAR and cumIAR

  if (nrow(bucketdata) < samplength) {

    estimatevpin@success <- FALSE

    estimatevpin@errorMessage <- vpin_err$largesamplength

    ux$show(c= verbose, m = vpin_ms$aborted)

    ux$show(ux$line())

    ux$stopnow(m = vpin_err$largesamplength, s = vpin_err$fn)

  }

  bucketdata$cumTAR <- cumsum(bucketdata$tar)
  bucketdata$cumIAR <- cumsum(bucketdata$iar)

  bucketdata$lagcumTAR <- head(c(rep(NA, samplength - 1), 0, bucketdata$cumTAR),
                               nrow(bucketdata))
  bucketdata$lagcumIAR <- head(c(rep(NA, samplength - 1), 0, bucketdata$cumIAR),
                               nrow(bucketdata))

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

  bucketdata$avIAR  <- (bucketdata$cumIAR - bucketdata$lagcumIAR)/samplength
  bucketdata$avTAR  <- (bucketdata$cumTAR - bucketdata$lagcumTAR)/samplength
  bucketdata$avUAR  <- bucketdata$avTAR - bucketdata$avIAR

  # Now, we have obtained the variables we need to calculate mu and alpha.
  # (EQ1) becomes mu (at bucket k) = avIAR[k] / alpha.
  # (EQ2) becomes eps (at bucket k) = avUAR[k] / 2.


  # We can, therefore, delete the other variables
  bucketdata$cumIAR <- bucketdata$lagcumIAR <- bucketdata$avTAR <- NULL
  bucketdata$cumTAR <- bucketdata$lagcumTAR <- NULL
  bucketdata$tar <- bucketdata$iar <- NULL

  # --------------------------------------------------------------------------
  # VIII.2 MAXIMUM-LIKELIHOOD ESTIMATION
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

  findMLE <- function(params, data, s = FALSE){

    # Load the factorization of the likelihood function for the IVPIN model.
    # Details of the factorization can be found in footnote 8 on page 369 of the paper.

    mlefn <- factorizations$ivpin(data)

    # Calculate the default value for the function in case the estimation fails
    # or if the returned likelihood is infinite. This default value consists of
    # the initial parameter set and the likelihood value calculated from these
    # initial parameters. The estimation is considered to have failed if the
    # convergence value is below zero.

    default_value <- c(params, mlefn(params), 0)

    optimal <- tryCatch({
      low <- c(0, 0, 0, 0, 0)
      up <- c(1, 1, Inf, Inf, Inf)
      estimates <- suppressWarnings(
        nloptr::lbfgs(params,
                      fn = mlefn,
                      lower = low,
                      upper = up))
      if(estimates$convergence < 0){
        return(default_value)
      }

      c(estimates$par, estimates$value, 1)
    })

    return(optimal)

  }

  # Initialize the progress bar
  if (verbose) {
    pb_ivpin <- ux$progressbar(0, maxvalue = nrow(bucketdata)- samplength + 1)
    cat(uix$vpin()$progressbar)
  }

  # Create an NA vector of ivpin values
  bucketdata$ivpin <- NA

  # create a variable to store the optimal value from the previous optimization
  previously_optimal <- NULL


  for(k in samplength:nrow(bucketdata)){

    # Obtain mldata which consits of Vb, Vs and t
    mldata <- bucketdata[(k-samplength+1):k, c("agg.bvol", "agg.svol", "duration")]

    # If the variable previously_optimal is not NULL, we use it as the initial
    # value for the optimization. If the optimization is successful, i.e., the
    # last element of the output from the function findMLE is equal to 1, then
    # the first five values of the output represent the new optimal values for
    # the current bucket, and also become the new "previously_optimal" values for
    # the next iteration. We then move to the next bucket. If the optimization is
    # not successful, we continue with the grid search.


    if (!is.null(previously_optimal)) {
      optimal <- as.list(findMLE(previously_optimal, mldata))
      names(optimal) <- c("alpha", "delta", "mu", "eb", "es", "likelihood", "conv")

      if(optimal$conv >= 0 & 
      optimal$alpha > 0 && optimal$alpha < 1 && 
      optimal$delta > 0 && optimal$delta < 1){
        bucketdata$ivpin[k] <- with(optimal, (alpha * mu)/(eb + es + mu))
        previously_optimal <- unlist(optimal[1:5])
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

    # Now, we create the grid of initial parameter sets using a variable called
    # grid_size, which determines the granularity of the partition of the parameter
    # space for alpha and delta.

    # Generate the set of values for alpha and delta:
    # (0.1, 0.3, 0.5, 0.7, 0.9) when grid_size is equal to 5.
    hstep <- 1 / (2 * grid_size)
    grid <- seq(hstep, 1 - hstep, 2 * hstep)
    alpha_values <- delta_values <- grid

    # Take the Cartesian product of the values of alpha and delta, and store them
    # in a dataframe called 'initials'.

    initials <- as.data.frame(expand.grid(alpha_values, delta_values))
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
      initials, 1, function(row){findMLE(row, mldata)})))
    colnames(results) <- c("alpha", "delta", "mu", "eb", "es", "likelihood", "conv")

    # We select the entries for which the variable 'conv' is equal to 1, indicating
    # that the optimization algorithm has converged, and save them in a dataframe
    # called 'optresults'. If this dataframe is empty, we use the dataframe
    # of the initial parameter sets with the corresponding likelihood values.

    optresults <- results[results$conv == 1,]
    if (nrow(optresults) == 0) optresults <- results
    # if (k > samplength) {
    #   print(optresults)
    #   browser()
    # }


    # Once the results are returned, we select the row from 'optresults' that has
    # the highest likelihood value, i.e., the likelihood-maximizing estimate.

    optimalrow <- results[which.max(results$likelihood),]

    # We calculate the value of IVPIN using Equation 19 on page 370 of
    # Ke and Lin (2017).

    bucketdata$ivpin[k] <- with(optimalrow, (alpha * mu)/(eb + es + mu))

    # We store the value of the optimal parameters in the variable 'previously_optimal'
    # to use it in the next iteration.

    previously_optimal <- unlist(optimalrow[1:5])

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

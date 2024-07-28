## - | FILE  HEADER |
##
## Script name:
##    model_vpin.R
##
## Purpose of script:
##    Implement the estimation of the Volume-synchronized PIN
##    measure (VPIN) of Easley et al. (2011, 2012)
##
## Author:
##    Montasser Ghachem
##
## Last updated:
##    2022-10-18
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
##    Estimates the Volume-Synchronized Probability of Informed
##    Trading as developed in Easley (2011, 2012).
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


#' @title Estimation of Volume-Synchronized PIN model
#'
#' @description Estimates the Volume-Synchronized Probability of Informed
#' Trading as developed in \insertCite{Easley2011;textual}{PINstimation}
#' and \insertCite{Easley2012;textual}{PINstimation}.
#'
#' @usage vpin(data, timebarsize = 60, buckets = 50, samplength = 50,
#'             tradinghours = 24, verbose = TRUE)
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
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the steps of the estimation of the VPIN model is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @details The dataframe data should contain at least three variables. Only the
#' first three variables will be considered and in the following order
#' \code{{timestamp, price, volume}}.
#'
#' The property \code{@bucketdata} is created as in
#' \insertCite{abad2012;textual}{PINstimation}.
#'
#' The argument `timebarsize` is in seconds enabling the user to implement
#' shorter than `1` minute intervals. The default value is set to `1` minute
#' (`60` seconds) following Easley et al. (2011, 2012).
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
#' @return Returns an object of class \code{estimate.vpin}.
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
#' # Estimate VPIN model, using the following parameter set where the time
#' # bar size is 5 minutes, i.e., 300 seconds (timebarsize = 300), 50
#' # buckets per average daily volume (buckets = 50), and a window size of
#' # 250 for the VPIN calculation (samplength = 250).
#'
#' estimate <- vpin(xdata, timebarsize = 300, buckets = 50, samplength = 250)
#'
#' # Display a description of the estimate
#'
#' show(estimate)
#'
#' # Plot the estimated VPIN vector
#'
#' plot(estimate@vpin, type = "l", xlab = "time", ylab = "VPIN", col = "blue")
#'
#' # Display the parameters of VPIN estimates
#'
#' show(estimate@parameters)
#'
#' # Store the computed data of the different buckets in a dataframe 'buckets'.
#' # Display the first 10 rows of the dataframe 'buckets'.
#'
#' buckets <- estimate@bucketdata
#' show(head(buckets, 10))
#'
#' # Store the daily VPIN values (weighted and unweighted) in a dataframe
#' # 'dayvpin'.
#'
#' # Display the first 10 rows of the dataframe 'dayvpin'.
#'
#' dayvpin <- estimate@dailyvpin
#' show(head(dayvpin, 10))
#'
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
  vpin_ms <- uix$vpin(timebarsize = timebarsize)

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

  # Convert data into a dataframe, in case it is a matrix or an array
  data <- as.data.frame(data)

  # initialize the local variables
  tbv <- vbs <- bucket <- NULL
  estimatevpin <- new("estimate.vpin")


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

  dataset$timestamp <- as.POSIXct(dataset$timestamp)

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
    tempbars$interval <- as.POSIXct(tempbars$interval, tz = "")

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
  minutebars$interval <- as.POSIXct(minutebars$interval, tz = "")

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

  x <- 10
  threshold <- (1 - 1 / x) * vbs

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
  minutebars[xtrarnum, ]$interval <- xtrarows$interval

  xtrarows$tbv <- xtrarows$tbv - xtrarows$exvol
  xtrarows$bucket <- xtrarows$bucket - 1


  # Now it it time to drop the duration variable of the dataframe minutebars
  minutebars$duration <- NULL

  # --------------------------------------------------------------------------
  # V.4 CALCULATE ACCUMULATED BUCKET VOLUME FOR THE ORIGINAL DATASET
  # --------------------------------------------------------------------------

  # We add the replicated timebars in xtrarows to the main dataset
  # 'minutebars'and sort it by interval and by bucket so we have all bucket
  # minutebars next to one another.

  xtrarows$interval <- as.POSIXct(xtrarows$interval,
                                  origin = "1970-01-01", tz = "")
  xtrarows <- xtrarows[c("interval", "dp", "tbv", "id", "runvol", "bucket",
                         "exvol")]

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
    aggregate(cbind(bvol, svol) ~ bucket, minutebars, sum),
                         c("bucket", "agg.bvol", "agg.svol"))
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

  ux$show(c= verbose, m = vpin_ms$step8)

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
  bucketdata$duration <- as.numeric(
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
    bucketdata$duration <- apply(bucketdata, 1, function(x)
      corduration(x[6], x[5], x[10]))
  }

  # Store the vector of vpin in the results vector

  estimatevpin@vpin <- bucketdata$vpin # [!is.na(bucketdata$vpin)]

  # Calculate daily unweighted and weighted VPINs

  bucketdata$day <- substr(bucketdata$starttime, 1, 10)

  bucketdata$vpindur <- bucketdata$vpin * bucketdata$duration

  dailyvpin <- setNames(aggregate(vpin ~ day, bucketdata,
                        function(x) mean(x, na.rm = TRUE)), c("day", "dvpin"))

  dailyvpin$day <- as.Date(dailyvpin$day)

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

  estimatevpin@runningtime <- ux$timediff(time_on, time_off)

  ux$show(c= verbose, m = vpin_ms$complete)

  return(estimatevpin)
}

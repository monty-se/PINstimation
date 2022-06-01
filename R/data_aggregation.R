## - | FILE  HEADER |
##
## Script name:
##    data_aggregation.R
##
## Purpose of script:
##    Implement four classification algorithms used to aggregate
##    high frequency data into daily data.
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
## Public functions:
## ++++++++++++++++++
##
## aggregate_trades():
##    Aggregates high-frequency trading data into aggregated
##    daily data using different trade classification algorithms.
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


#' @title Aggregation of high-frequency data
#'
#' @description Aggregates high-frequency trading data into aggregated
#' daily data using different trade classification algorithms.
#'
#' @usage aggregate_trades(data, algorithm = "Tick", timelag = 0, ...,
#'  verbose = TRUE)
#'
#' @param data A dataframe with 4 variables in the following
#' order (`timestamp`, `price`, `bid`, `ask`).
#' @param algorithm A character string refers to the algorithm used
#' to determine the trade initiator, a buyer or a seller. It takes one of four
#' values (`"Tick"`, `"Quote"`, `"LR"`, `"EMO"`). The default value is
#' `"Tick"`. For more information about the different algorithms, check the
#' details section.
#' @param timelag A number referring to the time lag in milliseconds
#' used to calculate the lagged midquote, bid and ask for the algorithms
#' \code{"Quote"}, \code{"EMO"} and \code{"LR"}.
#'
#' @param ... Additional arguments passed on to the function
#' `aggregate_trades()`. The recognized arguments are `reportdays`,
#' and `is_parallel`. Other arguments will be ignored.
#' \itemize{
#' \item `reportdays` is binary variable that determines whether the
#' variable `day` is returned. The default value is \code{FALSE}.
#' \item `is_parallel` is a logical variable that specifies whether
#' the computation is performed using parallel or sequential processing.
#' The default value is `TRUE`. For more details, please refer to the
#' vignette 'Parallel processing' in the package, or
#' \href{https://pinstimation.com/articles/parallel_processing.html}{online}.
#' }
#'
#' @param verbose A binary variable that determines whether detailed
#' information about the progress of the trade classification is displayed.
#' No output is produced when \code{verbose} is set to \code{FALSE}. The default
#' value is \code{TRUE}.
#'
#' @details
#' The argument `algorithm` takes one of four values:
#' \itemize{
#'    \item \code{"Tick"} refers to the tick algorithm: Trade is classified as a
#'    buy (sell) if the price of the trade to be classified
#'    is above (below) the closest different price of a previous trade.
#'    \item \code{"Quote"} refers to the quote algorithm: it classifies a
#'    trade as a buy (sell) if the trade price of the trade to be
#'    classified is above (below) the mid-point of the bid and ask spread.
#'    Trades executed at the mid-spread are not classified.
#'    \item \code{"LR"}  refers to `LR` algorithm as in
#'    \insertCite{LeeReady1991;textual}{PINstimation}. It classifies a trade
#'     as a buy (sell) if its price is above (below) the mid-spread (quote
#'     algorithm), and  uses the tick algorithm if the trade price is at
#'     the mid-spread.
#'     \item \code{"EMO"} refers to `EMO` algorithm as in
#'     \insertCite{Ellis2000;textual}{PINstimation}.
#'     It classifies trades at the bid (ask) as sells (buys) and uses the tick
#'     algorithm to classify trades within the then prevailing bid-ask spread.
#'  }
#'
#' `LR` recommend the use of mid-spread five-seconds earlier ('5-second'
#' rule) mitigating trade misclassifications for many of the \code{150}
#' NYSE stocks they analyze. On the other hand, in more recent studies such
#' as \insertCite{piwowar2006;textual}{PINstimation} and
#' \insertCite{Aktas2014;textual}{PINstimation}, the use of
#' 1-second lagged midquotes are shown to yield lower rates of
#' misclassifications. The default value is set to `0` seconds (no time-lag).
#' Considering the ultra-fast nature of today’s financial markets, time-lag
#' is in the unit of milliseconds. Shorter than 1-second lags can also be
#' implemented by entering values such as  `100` or `500`.
#'
#' @return Returns a dataframe of two (or three) variables. If \code{reportdays}
#' is set to \code{TRUE}, then the returned dataframe has three variables
#' `{day, b, s}`. If \code{reportdays} is set to \code{FALSE}, then the
#' returned dataframe has two variables `{b, s}`, and, therefore, can be
#' directly used for the estimation of the `PIN` and `MPIN` models.
#'
#' @references
#'
#' \insertAllCited
#'
#' @examples
#' # There is a preloaded dataset called 'hfdata' contained in the package.
#' # It is an artificially created high-frequency trading data. The dataset
#' # contains  100 000 trades and five variables 'timestamp', 'price',
#' # 'volume', 'bid', and 'ask'. For more information, type ?hfdata.
#'
#' xdata <- hfdata
#' xdata$volume <- NULL
#'
#' # Use the LR algorithm with a timelag of 0 milliseconds
#'
#' daytrades <- aggregate_trades(xdata, algorithm = "LR", verbose = FALSE)
#'
#' # Since the argument 'reportdays' is set to FALSE by default, then the
#' # output 'daytrades' can be used directly for the estimation of the PIN
#' # model, namely using pin_ea().
#'
#' estimate <- pin_ea(daytrades, verbose = FALSE)
#'
#' # Show the estimate
#'
#' show(estimate)
#'
#' @export
aggregate_trades <- function(data, algorithm = "Tick", timelag = 0, ...,
                             verbose = TRUE) {

  # Check that all variables exist and do not refer to non-existent variables
  # --------------------------------------------------------------------------
  allvars <- names(formals())
  allvars <- allvars[-4]
  environment(.xcheck$existence) <- environment()
  .xcheck$existence(allvars, err = uierrors$aggregation()$fn)

  # Assign the dot-dot-dot arguments
  # --------------------------------------------------------------------------
  reportdays <- .default$reportdays
  is_parallel <- .default$aggregation_parallel
  vargs <- list(...)
  # check for unknown keys in the argument "..."
  unknown <- setdiff(names(vargs), c("reportdays", "is_parallel"))
  ux$stopnow(length(unknown) > 0, s = uierrors$aggregation()$fn,
             m = uierrors$arguments()$unknown(u = unknown))

  if (length(vargs) > 0 && "reportdays" %in% names(vargs))
    reportdays <- vargs$reportdays
  if (length(vargs) > 0 && "is_parallel" %in% names(vargs))
    is_parallel <- vargs$is_parallel
  vargs <- NULL

  # Check that all arguments are valid
  # -------------------------------------------------------------------------
  largs <- list(data, algorithm, timelag, 0, verbose)
  names(largs) <- names(formals())
  largs[["..."]] <- NULL
  largs$is_parallel <- is_parallel
  largs$reportdays <- reportdays
  rst <- .xcheck$args(arglist = largs, fn = "aggregation")
  ux$stopnow(rst$off, m = rst$error, s = uierrors$aggregation()$fn)


  # Prepare the dataset
  # --------------------------------------------------------------------------
  # We rename the first four columns to "timestamp", "price", "bid", "ask"
  is_posixct <- function(x) inherits(x, "POSIXct")
  colnames(data) <- c("timestamp", "price", "bid", "ask")

  # We convert the columns price, bid and ask to numeric if they are not.
  # We also convert the timestamp variable to type 'PosixCT', if it does not
  # already have that type.

  if (!is_posixct(data$timestamp)) {
    stamps <- tryCatch({
      as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%OS")
    }, error = function(err) {
      NA
      })

    if (sum(is.na(stamps)) == 0) {
      data$timestamp <- stamps
    } else {
      data$timestamp <- as.POSIXlt(
        as.character(strptime(data$timestamp, format = "%H:%M:%S")))
    }
  }

  if (!is.numeric(data$price)) data$price <- as.numeric(data$price)

  if (!is.numeric(data$bid))  data$bid <- as.numeric(data$bid)

  if (!is.numeric(data$ask))  data$ask <- as.numeric(data$ask)

  # Sort the dataset by timestamp
  data <- data[order(data$timestamp), 1:4]

  colnames(data) <- c("timestamp", "price", "bid", "ask")

  # Check for errors in the case of the algorithm, use the algorithm Tick if
  # the algorithm name is unrecognized.

  unrecognized <- !any(toupper(algorithm) %in% c("TICK", "LR", "QUOTE", "EMO"))

  if (missing(algorithm) | unrecognized) algorithm <- "Tick"

  aggregate_ms <- uix$classification(
    nrow(data), method = algorithm, timelag, "", isparallel = is_parallel)

  # Show messages

  ux$show(verbose, m = aggregate_ms$start)

  ux$show(unrecognized, m = aggregate_ms$unrecognized, warning = TRUE)

  ux$show(verbose, m = aggregate_ms$algorithm)

  ux$show(verbose, m = aggregate_ms$number)

  ux$show(verbose & algorithm != "Tick", m = aggregate_ms$lag)

  ux$show(verbose & algorithm != "Tick", m = aggregate_ms$computing)

  # The vector returned by .get_quote() takes the value 'TRUE' for
  # buys, and 'FALSE' for sells.
  isbuy <- day <- NULL
  data$isbuy <- .get_quote(data, timelag, algorithm, is_parallel, verbose)
  data <- data[!is.na(data$isbuy), ]
  data$bid <- data$ask <- data$price <- NULL

  ux$show(verbose, m = aggregate_ms$aggregating)

  # Transform the timestamp variable into a day variable, and use it
  # to aggregate by day the number of buys, and the number of sells.
  data$day <- as.Date(data$timestamp, format = "%Y/%m/%d")

  data$isbuy <- as.numeric(data$isbuy)
  db <- data %>%
    group_by(day) %>%
    summarize(b = sum(isbuy), s = sum(!isbuy))
  db <- as.data.frame(db)

  if (!reportdays) db$day <- NULL

  # Show complete message
  ux$show(verbose, m = aggregate_ms$complete)

  return(invisible(db))
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


.get_quote <- function(data, timelag, method, is_parallel, verbose) {
# computes the logical vector quote that takes the value 'TRUE' if buy, and
# FALSE if sell
#
# Args:
#   data    : a dataframe with 4 variables
#   ('timestamp', 'price', 'bid', 'ask')
#   method  : a character string refers to the method used to determine the
#   trade initiator, a buyer or a seller. It takes one of four values
#   (Tick, Quote, LR, EMO).
#   is_parallel : a logical variable specifying whether parallel computing is
#   used.
#   timelag : a timelag in milliseconds used to calculate the lagged midquote
#   verbose : if TRUE, shows the progress of classification process.
#
# Returns:
#   A binary vector identifying the buyer-initiated trades

  aggregate_ms <- uix$classification()

  .get_tick_vector <- function() {

    # Get the sign of the (initial) price difference
    data$diffprice <- sign(c(0, diff(data$price)))

    data$diffprice[is.na(data$diffprice)] <- 0

    # Replace the zeros by the previous non-zero value.
    data$diffprice <- Reduce(
      function(x, y) if (y == 0) x else y, data$diffprice,
                             accumulate = TRUE)

    # Using the (updated) price difference, get the tick value
    # The signs take the values -1, 0, and 1; to get 1, 2, 3; we
    # then add +2
    tradeclass <- function(diffprice)
      return(switch(diffprice + 2, FALSE, NA, TRUE))

    data$buy <- vapply(data$diffprice, tradeclass, logical(1))

    return(invisible(data$buy))
  }

  .get_lagged_value <- function(cindex) {

    if (!exists(".lwbound") || cindex == 1) .lwbound <- 1

    pasttimes <- data$timestamp[.lwbound:cindex]

    currenttime <- data$timestamp[cindex]

    threshold <- currenttime - (timelag / 1000)

    atorbelowthreshold <- .lwbound - 1 + findInterval(threshold, pasttimes)

    atorbelowthreshold <- max(atorbelowthreshold, 0)

    .lwbound <<- atorbelowthreshold

    if (verbose) setTxtProgressBar(pblagged, cindex)

    return(atorbelowthreshold)

  }



  if (method == "Tick") {

    return(.get_tick_vector())

  } else {

    # The method is either "EMO", "LR", or "Quote". They all rely on
    # lagged values if timelag > 0. If timelag == 0, then the lagged
    # values are the values themselves, so they have the same index.
    # If timelag > 0, then the lagged values are computed using
    # the lagged indices.

    laggedindices <- seq_len(nrow(data))
    xs <- seq_len(nrow(data))

    # If timelag > 0, update the lagged indices.
    if (timelag > 0) {

      if (verbose) {
        pblagged <- ux$progressbar(minvalue = 0, maxvalue = nrow(data))
        cat(aggregate_ms$progressbar)

      }

      time_on <- Sys.time()

      if (is_parallel) {

        oplan <- future::plan(multisession, gc = TRUE,
                              workers = .default$parallel_cores())

        on.exit(plan(oplan), add = TRUE)

        laggedindices <- furrr::future_map(xs, .get_lagged_value)

        laggedindices <- unlist(laggedindices)

      } else {

        laggedindices <- vapply(xs, .get_lagged_value, double(1))

      }

      time_off <- Sys.time()

      cat("\n")

      actualtime <- ux$showtime(ux$timediff(time_on, time_off), full = FALSE)

      aggregate_ms <- uix$classification(time = actualtime)

      ux$show(c = verbose, m = aggregate_ms$time)

    }

    zeros <- sum(laggedindices == 0)
    laggedindices <- laggedindices[laggedindices > 0]

    if (method == "EMO") { # EMO ALGORITHM

      data$lbid <- c(rep(NA, zeros), data$bid[laggedindices])
      data$bid <- NULL
      data$lask <- c(rep(NA, zeros), data$ask[laggedindices])
      data$ask <- NULL

      data$quote <- .get_tick_vector()

      at_laggedbid <- which(data$price == data$lbid)
      if (length(at_laggedbid) > 0) data[at_laggedbid, ]$quote <- FALSE
      at_laggedask <- which(data$price == data$lask)
      if (length(at_laggedask) > 0) data[at_laggedask, ]$quote <- TRUE

    } else { # QUOTE OR LR ALGORITHM ALGORITHM

      data$midquote <- (data$bid + data$ask) / 2
      data$bid <- data$ask <- NULL
      data$lmidquote <- c(rep(NA, zeros), data$midquote[laggedindices])

      data$priceminusmidquote <- data$price - data$lmidquote

      # Use tradeclass() to get the class of the trade "BUY", "SELL" or "NONE"
      tradeclass <- function(diffprice)
        return(switch(diffprice + 2, FALSE, NA, TRUE))

      data$quote <- lapply(sign(data$priceminusmidquote), tradeclass)

      # If the algorithm is quote, we stop here. If the algorithm is LR, then
      # there is an additional step

      if (method == "LR") { # LR ALGORITHM

        tick <- .get_tick_vector()

        # Replace the value of trade class by the value of the tick rule when
        # the price is at the midpoint i.e. when quote is equal to NA
        data$quote <- ifelse(data$priceminusmidquote == 0, tick, data$quote)

      }

    }

  }

  return(invisible(data$quote))

}

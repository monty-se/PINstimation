## - | FILE  HEADER |
##
## Script name:
##    utilities_functions.R
##
## Purpose of script:
##    Implement custom supporting functions common to two or more files.
##    It contains a main public function called set_display_digits(),
##    and a list of supporting functions called 'ux', that gathers
##    various files needed for the technical running of other functions
##    in the package
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
## Public functions:
## ++++++++++++++++++
##
## set_display_digits():
##    Sets the number of digits to display in the output of the
##    different package functions.
##
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


#' @title Package-wide number of digits
#'
#' @description Sets the number of digits to display in the output of the
#' different package functions.
#'
#' @usage set_display_digits(digits = list())
#'
#' @param digits A list of numbers corresponding to the different
#' display digits. The default value is `list()`.
#'
#' @details The parameter `digits` is a named list. It will be containing:
#' \itemize{
#'   \item  `d1`: contains the number of display digits for the values of
#'   probability estimates such as \eqn{\alpha}, \eqn{\delta}, `pin`, `mpin`,
#'   \code{mpin(j)}, `adjpin`, `psos`, \thetaB, and \thetaS.
#'   \item  `d2`: contains the number of display digits for the values of
#'   \eqn{\mu}, \eb and \es, as well as information criteria: `AIC`, `BIC`, and
#'   `AWE`.
#'   \item  `d3`: contains the number of display digits for the remaining values
#'   such as `vpin` statistics and `likelihood` value .
#' }
#'
#' If the function is called with no arguments, the display digits will be reset
#' to the default values, i.e., \code{list(d1 = 6, d2 = 2, d3 = 3))}.
#' If the argument `digits` is not omitted, the function will only accept a list
#' containing exactly three numerical values, each ranging
#' between `0` and `10`. The list can be named or unnamed. If the numbers in the
#' argument `digits` are not integers, they will be rounded.
#'
#' @examples
#' # There is a preloaded quarterly dataset called 'dailytrades' with 60
#' # observations. Each observation corresponds to a day and contains the total
#' # number of buyer-initiated transactions ('B') and seller-initiated
#' # transactions ('S') on that day. To know more, type ?dailytrades
#'
#' xdata <- dailytrades
#'
#' # We show the output of the function pin_ea() using the default values
#' # of display digits. We then change these values using the function
#' # set_display_digits(), before displaying the same estimate.pin object
#' # again to see the difference.
#'
#' model <- pin_ea(xdata, verbose = FALSE)
#' show(model)
#'
#' # Change the number of digits for d1 to 3, of d2 to 0 and of d3 to 2
#'
#' set_display_digits(list(3, 0, 2))
#'
#' # No need to run the function mpin_ml() again to update the display of an
#' # estimate.mpin object.This holds for all estimate* S4 objects.
#'
#' show(model)
#'
#' @export
set_display_digits <- function(digits = list()) {

  defaultlist <- .default$displaydigits

  # This is for the first run, if the option is not set, set it to the
  # default value. It is invoked by the function .onLoad() in the file
  # zzz.R
  if (!is.null(digits$firstrun)) {
    options("PIN-digits" = defaultlist)
    return()
  }


  if (missing(digits)) {
    options("PIN-digits" = defaultlist)
    return(cat("Display digits reset to default values!"))
  }

  if (is.list(digits) && length(digits) == 3) {
    is_numeric <- suppressWarnings(all(digits >= 0) && all(digits <= 10))

    if (is.na(is_numeric) | is.null(is_numeric)) {
      return(message("Error: Update Failed! The argument digits should",
                     " only contain numeric elements!"))
    }

    if (is_numeric) {
      digits <- lapply(digits, round)

      newdigits <- setNames(
        list(d1 = digits[[1]], d2 = digits[[2]], d3 = digits[[3]]),
        c("d1", "d2", "d3"))

      options("PIN-digits" = newdigits)
      cat("Display digits updated successfully!")

    } else {

      message("Error: Update failed! The argument digits ",
              "should only contains numbers between 0 and 10!", sep = "")
    }
  } else {
    message("Error: Update failed! The argument digits should ",
            "be a list of 3 elements")
  }
}


##       +++++++++++++++++++++++++
## ++++++| | PRIVATE FUNCTIONS | |
##       +++++++++++++++++++++++++


ux <- list(

  line = function() cat("\r------------------\n"),

  #### Colors

  color = function(fg = 39, bg = 49, x) {

  #    | Color        | Foreground | Background |
  #    | -------------|------------|------------|
  #    |  0 - black   | 30         | 40         |
  #    |  1 - red     | 31         | 41         |
  #    |  2 - green   | 32         | 42         |
  #    |  3 - yellow  | 33         | 43         |
  #    |  4 - blue    | 34         | 44         |
  #    |  5 - magenta | 35         | 45         |
  #    |  6 - cyan    | 36         | 46         |
  #    |  7 - white   | 37         | 47         |

    col <- paste(fg, bg, sep = ";")
    colored <- paste0("\033[0;", col, "m", x, "\033[0m")
    return(colored)
  },

  bold = function(fg = 39, x) {
    ux$color(fg = paste(fg, ";1"), x = x)
  },

  warn = function(x) cat(ux$color(fg = 36, x = x)),

  # c for condition, m for message, and skip for skip line
  show = function(c = TRUE, m, warning = FALSE, skip = TRUE) {
    if (c) {
      if (skip) m <- paste(m, "\n", sep = "")
      if (warning) ux$warn(m) else cat(m)
    }
  },

  # c for condition, m for message and s of function signature
  stopnow = function(c = TRUE, m, s) {
    if (c) {
      message(m)
      ux$line()
      stop(s, call. = FALSE)
    }
  },

  sep = function(x) {
    format(round(as.numeric(x)), nsmall = 0, big.mark = " ",
           scientific = FALSE)
  },

  todframe = function(alist) {
    data.frame(
      matrix(unlist(alist), nrow = length(alist), byrow = TRUE))
  },

  listtodataframe <- function(alist) {
    data.frame(do.call(rbind, lapply(alist, function(x) x)))
  },

  tolist = function(df) as.list(as.data.frame(t(df))),

  forcedf = function(alist, lnames) {
  # If a list of length one, we force it into a dataframe, by
  # binding two copies of the list, transforming the result into
  # a dataframe, then deleting one copy.
    if (all(lengths(alist) == 1) == 1) {

      tempdf <- as.data.frame(rbind(alist, alist))
      colnames(tempdf) <- lnames
      tempdf <- head(tempdf, -1)
      return(tempdf)

    } else {

      return(ux$todframe(alist))

    }
  },

  parentheses =  function(number) {
    return(paste(ifelse(number < 0, "(", ""), ux$round3(abs(number)),
                 ifelse(number < 0, ")", ""), sep = ""))
  },

  randomshow = function(freq = 0.5){
    rnd <- runif(n = 1)
    showit <- if (rnd < freq) TRUE else FALSE
    return(showit)
  },

  integer = function(x) (floor(x) == x),

  is.integer = function(x) {

    if (is.null(x) || !is.numeric(x) || floor(x) != x)
      return(FALSE)
    return(TRUE)
  },

  is.logical = function(x) {

    if (is.null(x) || !is.logical(x))
      return(FALSE)
    return(TRUE)

  },

  is.numeric = function(x) {

    if (is.null(x) || !is.numeric(x)) return(FALSE)
    return(TRUE)

  },

  is.timestamp = function(x) {
    if (is.null(x) ||
        (!is.character(x) & !inherits(x, "POSIXct")))
      return(FALSE)

    return(TRUE)

  },

  is.convertible.to.date = function(x) {

    valid <- tryCatch({
      x <- as.POSIXct(x); TRUE
      }, error = function(err) {
                        FALSE
        })
    if (!valid) {
      valid <- tryCatch({
        x <- as.POSIXlt(
          as.character(strptime(x, format = "%H:%M:%S")))
        valid <- ifelse(is.na(x), FALSE, TRUE)
      }, error = function(err) {
        FALSE
        })
    }

    return(valid)
  },

  exists = function(x) {
    tryCatch({
      suppressWarnings(class(object)); return(TRUE)
    }, error = function(cond) {
      return(FALSE)
    })
  },

  showtime = function(seconds, full = TRUE) {
  # transforms the number of seconds into a readable character string to be
  # displayed when dislaying S4 objects containing results of simulation or
  # estimation
  #
  # Args:
  #   seconds   : number of seconds
  #   time_off  : the final timestamp
  #
  # Returns:
  #   a character string of the time difference in minutes and seconds
    hours <- 0
    if (seconds %/% 3600 > 0) {
      hours <- seconds %/% 3600
      seconds <- round(seconds %% 3600, 2)
    }

    minutes <- 0
    if (seconds %/% 60 > 0) {
      minutes <- seconds %/% 60
      seconds <- round(seconds %% 60, 2)
    }
    run_time <- paste(
      ifelse(hours == 0, "", paste(hours, "hour(s) and ")),
      ifelse(minutes == 0, "", paste(minutes, "minute(s) and ")),
      seconds, " seconds", sep = "")

    if (full) run_time <- paste("\n-------\nRunning time: ", run_time, sep = "")
    return(run_time)
  },

  timediff =  function(time_on, time_off, multiplier = 1) {
  # Computes the (expected) time difference between two timestamps in seconds
  #
  # Args:
  #   time_on   : the initial timestamp
  #   time_off  : the final timestamp
  #   multiplier: a number multiplying the time difference in case the function
  #               is used to estimate the expected running time and 'time_on'
  #               and 'time_off' corresponds to a small share of the total task
  #
  # Returns:
  #   a number corresponding to the difference between time stamps in seconds

    total_duration <- difftime(time_off, time_on, units = "secs")
    seconds <- multiplier * round(total_duration[[1]], 3)

    return(seconds)

  },

  exptime = function(seconds, n, processes = 1) {
  # Computes the (expected) running time of a task where the running time
  # is a convex function of the number of the elementary tasks to be
  # performed
  #
  # Args:
  #   seconds : the number of seconds required to finish an elementary task
  #   n       : the number of elementary tasks
  #  processes: number of parallel processes of calculating lagged values. It
  #  is equal to 1 for "LR", and "Quote" algorithms, an 2 for "EMO"
  #  algorithm (bid and ask)
  #
  # Returns:
  #   a character string of the expected time in minutes and seconds

    xseconds <- ceiling(seconds / 0.1) * 0.1
    if (xseconds == 0) xseconds <- seconds
    minutes <- 0

    seconds <- round((n ^ (xseconds * 2)) * processes)

    if (seconds %/% 60 > 0) {
      minutes <- seconds %/% 60
      seconds <- round(seconds %% 60, 2)
    }

    run_time <- paste(ifelse(minutes == 0, "",
                             paste(minutes, "minute(s) and ")),
                      seconds, " seconds", sep = "")
    return(run_time)
  },

  progressbar = function(minvalue = 0, maxvalue) {
  # draws a progress bar with provided bounds
  #
  # Args:
  #   minvalue: lower bound for the progress bar
  #   maxvalue: upper bound for the progres bar
  #
  # Returns:
  #   a progress bar of type 'txtProgressBar'

    return(txtProgressBar(
      minvalue, maxvalue, style = 3, width = 37, char = "+"))
  },

  percent = function(x, digits = 2, format = "f", ...) {
  # format a number as a percentage with a provided number of digits
  #
  # Args:
  #   x     : a real number to be formatted as a percentage
  #   digits: the number of digits in the formatted number
  #
  # Returns:
  #   A number formated as a percentage with 'digits' number of digits

    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  },

  round1 = function(number) {
  # rounds the numbers to the number of digits valued at the first element
  # of the vector of three digits in the option "PIN-digits" (d1).
  #
  # Args:
  #   number  : a real number
  #
  # Returns:
  #   a number rounded to 'd1' digits

    digits <- getOption("PIN-digits")
    return(round(number, digits$d1))
  },

  round2 = function(number) {
  # rounds the numbers to the number of digits valued at the second element
  # of the vector of three digits in the option "PIN-digits" (d2).
  #
  # Args:
  #   number  : a real number
  #
  # Returns:
  #   a number rounded to 'd2' digits

    digits <- getOption("PIN-digits")
    return(round(number, digits$d2))
  },

  round3 = function(number) {
  # rounds the numbers to the number of digits valued at the third element
  # of the vector of three digits in the option "PIN-digits" (d3).
  #
  # Args:
  #   number  : a real number
  #
  # Returns:
  #   a number rounded to 'd3' digits

    digits <- getOption("PIN-digits")

    return(round(number, digits$d3))
  },

  prepare = function(data) {
  # prepares the data for the different PIN estimations by only keeping the
  # first two columns, renaming them to 'b' and 's' and deleting all rows
  # containing NA.
  #
  # Args:
  #   data: a dataframe
  #
  # Returns:
  #   A dataframe with two variable 'b' and 's'.

    data <- as.data.frame(data[, 1:2])
    colnames(data) <- c("b", "s")
    data <- data[complete.cases(data), ]
    data <- na.omit(data)
    data <- round(data)
    return(data)
  },

  is_sub = function(x, y, strict = TRUE) {
    return((min(x) >= y[1] & max(x) <= y[2]))
  },

  strict_sub = function(x, y, strict = TRUE) {
    return((min(x) > y[1] & max(x) < y[2]))
  },

  update_optimal = function(old, new, cond = TRUE) {
    if (cond & (new$likelihood > old$likelihood)) return(new)
    return(old)
  },

  finite_sum = function(x) sum(x[is.finite(x)]),

  get_cores = function() {
    xcores <- tryCatch({
      ecores <- future::availableCores()
      if (!is.numeric(ecores)) ecores <- 1
      return(ecores)
    }, error = function(err) {
      1
    })
    return(xcores)
  }

)

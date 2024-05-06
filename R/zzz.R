.onLoad <- function(libname = find.package("PINstimation"),
                    pkgname = "PINstimation") {

  # Initialize the display digits to its default values.
  set_display_digits(list(firstrun = TRUE))

  # Initialize the option "numberoftradingdays" which will store
  # the number of trades in the dataset 'data'. It will serve as
  # the cap for the arguments, layers, and xtraclusters.
  options("numberoftradingdays" = +Inf)


  # Set the threshold for using parallel processing when the
  # argument is_parallel is set to TRUE.
  options("pinstimation.parallel.threshold" = 100)

  # Set the number of parallel cores to be used in the parallel
  # processing for the different PIN functions. If the number of
  # cores is larger than 1, all but one core are used for parallel
  # processing.
  xcores <- ux$get_cores()
  if (xcores > 1) xcores <- 2
  options("pinstimation.parallel.cores" = xcores)

}





.onAttach <- function(libname, pkgname) {

  contentlist <- list(
    "An R Package for estimating the probability of informed trading",
    "Website  : https://www.pinstimation.com",
    "         : https://github.com/monty-se/PINstimation",
    "Contact  : contact@pinstimation.com ",
    paste("Issues   : Report issues at ",
    "https://github.com/monty-se/PINstimation/issues", sep = ""),
    "Copyright: GPL License"
  )

  boxtext <- function(title, body, size = 85) {

    xtitle <- paste("\n", ux$color(
      fg = "1;34", bg = 49,
      x = paste("| ", title, "\n", sep = "")), sep = "")
    xbar <- paste(strrep("-", size), "\n", sep = "")
    xcontent <- lapply(body, function(x)
      paste("| ", x, strrep(" ", size - 3 - nchar(x)), "|\n", sep = ""))
    xcontent <- paste(
      xbar,
      paste(unlist(xcontent), collapse = "", sep = ""),
      xbar, sep = "")
    xcontent <- ux$color(fg = 34, x = xcontent)
    xmessage <- paste(xtitle, xcontent, sep = "")
    xmessage <-  ux$color(fg = 34, x = xmessage)

    return(xmessage)

  }

  startup <- boxtext(title = "WELCOME TO PINSTIMATION 0.1.2",
                     body = contentlist)


  if (ux$randomshow(freq = 1))
    packageStartupMessage(startup)

}

load_pinstimation_for_good <- function() {

  line <- "if (interactive()) suppressMessages(require(PINstimation))"
  profilepath <- file.path(Sys.getenv("HOME"), ".Rprofile")

  readprofile <- readLines(profilepath)
  alreadyadded <- any(unname(
    vapply(readprofile, function(x) (x == line), FUN.VALUE = logical(1))))

  if (alreadyadded) {

    cat(ux$color(fg = 31, x = paste(
      "No changes have been made.\n")))

    cat(ux$color(fg = 31, x = paste(
      "PINstimation is already added to your R profile.\n")))

  }  else {

    write(line,
          file = file.path(Sys.getenv("HOME"), ".Rprofile"),
          append = TRUE)
    cat(ux$color(fg = 32, x = paste(
      "Your R profile has been successfully modified.\n")))
    cat(ux$color(fg = 32, x = paste(
      "PINstimation will be automatically loaded",
      "in future R sessions.\n")))
    cat(ux$color(fg = 36, x = paste(
      "Restart R for changes to take effect")))
  }

}

## - | FILE HEADER | - ---------------------------
##
## Script name:
##    datasets.R
##
## Purpose of script:
##    Describe example datasets included in the package.
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
## Example datasets:
## ++++++++++++++++++
##
## hfdata:
##    A simulated dataset containing sample 'timestamp', 'price',
##    'volume', 'bid' and 'ask' for `100 000` high-frequency
##    transactions.
##
## dailytrades:
##    An example dataset representative of quarterly data containing
##    the aggregate numbers of buyer-initiated and seller-initiated
##    trades for each trading day.
##
## ++++++++++++++++++
##
##
## --
## Package: PINstimation
## website: www.pinstimation.com
## Authors: Montasser Ghachem and Oguz Ersan


## - | DATASETS    | - ---------------------------


#' @title High-frequency trade-data
#'
#' @description A simulated dataset containing sample `timestamp`, `price`,
#' `volume`, `bid` and `ask` for `100 000` high frequency transactions.
#'
#' @format A data frame with `100 000` observations with `5` variables:
#' \itemize{
#'   \item `timestamp`: time of the trade.
#'   \item `price`: transaction price.
#'   \item `volume`: volume of the transactions, in asset units.
#'   \item `bid`: best bid price.
#'   \item `ask`: best ask price.
#' }
#' @source Artificially created data set.
"hfdata"



#' @title Example of quarterly data
#'
#' @description An example dataset representative of quarterly data containing
#' the aggregate numbers of buyer-initiated and seller-initiated trades for
#' each trading day.
#'
#' @format A data frame with `60` observations and `2` variables:
#' \itemize{
#'   \item `B`: total number of buyer-initiated trades.
#'   \item `S`: total number of seller-initiated trades.
#' }
#' @source Artificially created data set.
"dailytrades"

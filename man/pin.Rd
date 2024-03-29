% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_pin.R
\name{pin}
\alias{pin}
\title{PIN estimation - custom initial parameter sets}
\usage{
pin(data, initialsets, factorization = "E", verbose = TRUE)
}
\arguments{
\item{data}{A dataframe with 2 variables: the first
corresponds to buyer-initiated trades (buys), and the second corresponds
to seller-initiated trades (sells).}

\item{initialsets}{A dataframe with the following variables in
this order (\eqn{\alpha}, \eqn{\delta}, \eqn{\mu}, \eb, \es).}

\item{factorization}{A character string from
\code{\{"EHO", "LK", "E", "NONE"\}} referring to a given factorization. The
default value is set to \code{"E"}.}

\item{verbose}{A binary variable that determines whether detailed
information about the steps of the estimation of the PIN model is displayed.
No output is produced when \code{verbose} is set to \code{FALSE}. The default
value is \code{TRUE}.}
}
\value{
Returns an object of class \code{estimate.pin}
}
\description{
Estimates the Probability of Informed Trading (\code{PIN})
using custom initial parameter sets
}
\details{
The argument 'data' should be a numeric dataframe, and contain
at least two variables. Only the first two variables will be considered:
The first variable is assumed to correspond to the total number of
buyer-initiated trades, while the second variable is assumed to
correspond to the total number of seller-initiated trades. Each row or
observation correspond to a trading day. \code{NA} values will be ignored.

The factorization variable takes one of four values:
\itemize{
\item \code{"EHO"} refers to the factorization in
\insertCite{Easley2010;textual}{PINstimation}
\item \code{"LK"}  refers to the factorization in
\insertCite{WilliamLin2011;textual}{PINstimation}
\item \code{"E"}  refers to the factorization in
\insertCite{Ersan2016;textual}{PINstimation}
\item \code{"NONE"} refers to the original likelihood function - with no
factorization
}
}
\examples{
# There is a preloaded quarterly dataset called 'dailytrades' with 60
# observations. Each observation corresponds to a day and contains the
# total number of buyer-initiated trades ('B') and seller-initiated
# trades ('S') on that day. To know more, type ?dailytrades

xdata <- dailytrades

#--------------------------------------------------------------
# Using generic function pin()
#--------------------------------------------------------------

# Define initial parameters:
# initialset = (alpha, delta, mu, eps.b, eps.s)

initialset <- c(0.3, 0.1, 800, 300, 200)

# Estimate the PIN model using the factorization of the PIN likelihood
# function by Ersan (2016)

estimate <- pin(xdata, initialsets = initialset, verbose = FALSE)

# Display the estimated PIN value

show(estimate@pin)

# Display the estimated parameters

show(estimate@parameters)

# Store the initial parameter sets used for MLE in a dataframe variable,
# and display its first five rows

initialsets <- estimate@initialsets
show(head(initialsets, 5))

}
\references{
\insertAllCited
}

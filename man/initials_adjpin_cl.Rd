% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_adjpin.R
\name{initials_adjpin_cl}
\alias{initials_adjpin_cl}
\title{AdjPIN initial parameter sets of Cheng and Lai (2021)}
\usage{
initials_adjpin_cl(data, restricted = list(), verbose = TRUE)
}
\arguments{
\item{data}{A dataframe with 2 variables: the first
corresponds to buyer-initiated trades (buys), and the second corresponds
to seller-initiated trades (sells).}

\item{restricted}{A binary list that allows estimating restricted
AdjPIN models by specifying which model parameters are assumed to be equal.
It contains one or multiple of the following four elements
\verb{\{theta, mu, eps, d\}}. For instance, If \code{theta} is set to \code{TRUE},
then the probability of liquidity shock in no-information days, and in
information days is assumed to be the same (\thetaB\code{=}\thetaS). If any of
the remaining rate elements \verb{\{mu, eps, d\}} is set to \code{TRUE},
(say  \code{mu=TRUE}), then the rate is assumed to be the same on the buy side,
and on the sell side (\mub\code{=}\mus). If more than one element is set to
\code{TRUE}, then the restrictions are combined. For instance, if the argument
\code{restricted} is set to \code{list(theta=TRUE, eps=TRUE, d=TRUE)}, then the
restricted AdjPIN model is estimated, where \thetaB\code{=}\thetaS, \eb\code{=}\es,
and \Db\code{=}\Ds. If the value of the argument \code{restricted} is the empty list,
then all parameters of the model are assumed to be independent,
and the unrestricted model is estimated. The default value is the empty
list \code{list()}.}

\item{verbose}{a binary variable that determines whether information messages
about the initial parameter sets, including the number of the initial
parameter sets generated. No message is shown when \code{verbose} is set
to \code{FALSE}. The default value is \code{TRUE}.}
}
\value{
Returns a dataframe of numerical vectors of ten elements
\{\eqn{\alpha}, \eqn{\delta}, \eqn{\theta}, \eqn{\theta'},
\eb, \es, \mub, \mus, \Db, \Ds\}.
}
\description{
Based on an extension of the algorithm in
\insertCite{ChengLai2021;textual}{PINstimation}, generates sets of initial
parameters to be used in the maximum likelihood
estimation of \code{AdjPIN} model.
}
\details{
The argument 'data' should be a numeric dataframe, and contain
at least two variables. Only the first two variables will be considered:
The first variable is assumed to correspond to the total number of
buyer-initiated trades, while the second variable is assumed to
correspond to the total number of seller-initiated trades. Each row or
observation correspond to a trading day. \code{NA} values will be ignored.
\cr\cr The function implements an extension of the algorithm of
\insertCite{ChengLai2021;textual}{PINstimation}. In their paper, the authors
assume that the probability of liquidity shock is the same in no-information,
and information days, i.e., \thetaB\code{=}\thetaS, and use a procedure similar to
that of \insertCite{Yan2012;textual}{PINstimation} to generate 64 initial
parameter sets. The function implements an extension of their algorithm,
by relaxing the assumption of equality of liquidity shock probabilities,
and generates thereby \code{256} initial parameter sets for the unrestricted
\code{AdjPIN} model.
}
\examples{
# There is a preloaded quarterly dataset called 'dailytrades' with 60
# observations. Each observation corresponds to a day and contains the
# total number of buyer-initiated trades ('B') and seller-initiated
# trades ('S') on that day. To know more, type ?dailytrades

xdata <- dailytrades

# The function adjpin(xdata, initialsets="CL") allows the user to directly
# estimate the AdjPIN model using the full set of initial parameter sets
# generated using the algorithm Cheng and Lai (2021)
\donttest{
estimate.1 <- adjpin(xdata,  initialsets="CL", verbose = FALSE)
}

# Obtaining the set of initial parameter sets using initials_adjpin_cl
# allows us to estimate the PIN model using a subset of these initial sets.

# Use initials_adjpin_cl() to generate 256 initial parameter sets using the
# algorithm of Cheng and Lai (2021).

initials_cl <- initials_adjpin_cl(xdata, verbose = FALSE)

# Use 20 randonly chosen initial sets from the dataframe 'initials_cl' in
# order to estimate the AdjPIN model using the function adjpin() with custom
# initial parameter sets

numberofsets <- nrow(initials_cl)
selectedsets <- initials_cl[sample(numberofsets, 20),]

estimate.2 <- adjpin(xdata, initialsets = selectedsets, verbose = FALSE)

# Compare the parameters and the pin values of both specifications
\donttest{
comparison <- rbind(
c(estimate.1@parameters, adjpin = estimate.1@adjpin, psos = estimate.1@psos),
c(estimate.2@parameters, estimate.2@adjpin, estimate.2@psos))

rownames(comparison) <- c("all", "50")

show(comparison)
}

}
\references{
\insertAllCited
}

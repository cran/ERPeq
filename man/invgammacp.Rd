% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invgammacp.R
\name{invgammacp}
\alias{invgammacp}
\title{Probabilistic estimation of earthquake recurrence interval using inverse gamma distribution}
\usage{
invgammacp(fit, r, te)
}
\arguments{
\item{fit}{Fit is the fitinvgamma object. See ?fitinvgamma for details.}

\item{r}{The specified time in which the probability of an earthquake is desired to be calculated.}

\item{te}{Elapsed time since the last earthquake}
}
\value{
A numeric value
}
\description{
Computes the probability of an earthquake within a specified time "r" and elapsed time "te".
}
\examples{
fit=fitinvgamma(c(1,1),data=data_earthquake_7)
invgammacp(fit,r=2,te=5)
}
\references{
Pasari, S. and Dikshit, O. (2014). Impact of three-parameter Weibull models in probabilistic assessment of earthquake hazards. Pure and Applied Geophysics, 171, 1251-1281.
}

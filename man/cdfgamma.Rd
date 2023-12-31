% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdfgamma.R
\name{cdfgamma}
\alias{cdfgamma}
\title{Cumulative distribution function of the Gamma distribution}
\usage{
cdfgamma(par, x)
}
\arguments{
\item{par}{parameter vector of the gamma distribution.
First parameter is the shape and second is the scale parameter}

\item{x}{vector of quantiles}
}
\value{
return the value of the cdf of the gamma distribution
}
\description{
Cumulative distribution function of the Gamma distribution
}
\examples{
cdfgamma(c(2,3),5)
}
\references{
Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
}

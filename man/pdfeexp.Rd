% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdfeexp.R
\name{pdfeexp}
\alias{pdfeexp}
\title{Probability density function of the exponentiated exponential distribution}
\usage{
pdfeexp(par, x)
}
\arguments{
\item{par}{parameter vector of the exponentiated exponential distribution.
First parameter is the shape, second is the scale parameter.}

\item{x}{vector of observations or single value}
}
\value{
return the value of the pdf of the exponentiated exponential distribution
}
\description{
Probability density function of the exponentiated exponential distribution
}
\examples{
pdfeexp(c(0.5,0.3),2)
}
\references{
Gupta, R. D., & Kundu, D. (1999). Theory & methods: Generalized exponential distributions. Australian & New Zealand Journal of Statistics, 41(2), 173-188.
Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
}

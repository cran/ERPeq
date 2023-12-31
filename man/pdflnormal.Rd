% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdflnormal.R
\name{pdflnormal}
\alias{pdflnormal}
\title{Probability density function of the log-normal distribution}
\usage{
pdflnormal(par, x)
}
\arguments{
\item{par}{parameter vector of the log-normal distribution.
First parameter is the shape and second is the scale parameter}

\item{x}{vector of observations or single value}
}
\value{
return the value of the pdf of the log-normal distribution
}
\description{
Probability density function of the log-normal distribution
}
\examples{
pdflnormal(c(2,3),5)
}
\references{
Heyde, C. C. (1963). On a property of the lognormal distribution. Journal of the Royal Statistical Society: Series B (Methodological), 25(2), 392-393.
}

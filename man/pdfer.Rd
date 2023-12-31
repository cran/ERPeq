% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdfer.R
\name{pdfer}
\alias{pdfer}
\title{Probability density function of the exponentiated Rayleigh  distribution}
\usage{
pdfer(par, x)
}
\arguments{
\item{par}{parameter vector of the exponentiated Rayleigh  distribution.
First parameter is the scale, second is the shape parameter.}

\item{x}{vector of observations or single value}
}
\value{
return the value of the pdf of the exponentiated Rayleigh  distribution
}
\description{
Probability density function of the exponentiated Rayleigh  distribution
}
\examples{
pdfer(c(0.5,0.3),2)
}
\references{
Vodă, V. G. (1976). Inferential procedures on a generalized Rayleigh variate. I. Aplikace matematiky, 21(6), 395-412.
}

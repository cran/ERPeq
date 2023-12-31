% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitpareto.R
\name{fitpareto}
\alias{fitpareto}
\title{Fitting the Pareto distribution}
\usage{
fitpareto(starts, data)
}
\arguments{
\item{starts}{A vector defining the starting values for the Nelder-Mead algorithm.}

\item{data}{A vector containing the observations}
}
\value{
List the estimated parameters of the distribution with standard errors and goodness-of-fit statistics.
}
\description{
Fitting the Pareto distribution
}
\examples{
library(VGAM)
data=VGAM::rpareto(500,5,2)
fitpareto(starts =c(2),data=data)
}

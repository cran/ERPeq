% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rlevy.R
\name{rlevy}
\alias{rlevy}
\title{Generate random observations from Levy distribution}
\usage{
rlevy(n, mu, c)
}
\arguments{
\item{n}{number of observations to be generated}

\item{mu}{location parameter of the Levy distribution}

\item{c}{scale parameter of the Levy distribution}
}
\value{
return the random sample generated from Levy distribution
}
\description{
Generate random observations from Levy distribution
}
\examples{
rlevy(500,2,3)
}
\references{
Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
}

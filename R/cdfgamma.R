#' @title Cumulative distribution function of the Gamma distribution
#'
#' @param par parameter vector of the gamma distribution.
#' First parameter is the shape and second is the scale parameter
#' @param x vector of quantiles
#'
#' @return return the value of the cdf of the gamma distribution
#' @export
#' @import stats
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.

#' @examples
#' cdfgamma(c(2,3),5)

cdfgamma=function(par,x)
{
  a=par[1]
  b=par[2]
  g=dgamma(x,a,b)
  G=pgamma(x,a,b)
  return(G)
}

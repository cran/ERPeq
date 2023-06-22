#' @title Cumulative distribution function of the Weibull distribution
#'
#' @param par parameter vector of the Weibull distribution.
#' First parameter is the shape and second is the scale parameter
#' @param x vector of quantiles
#'
#' @return return the value of the cdf of the weibull distribution
#' @export
#' @import stats
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.

#' @examples
#' cdfweibull(c(2,3),5)

cdfweibull=function(par,x)
{
  a=par[1]
  b=par[2]
  g=stats::dweibull(x,a,b)
  G=stats::pweibull(x,a,b)
  return(G)
}

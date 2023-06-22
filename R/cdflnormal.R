#' @title Cumulative distribution function of the log-normal distribution
#'
#' @param par parameter vector of the log-normal distribution.
#' First parameter is the shape and second is the scale parameter
#' @param x vector of quantiles
#'
#' @return return the value of the cdf of the log-normal distribution
#' @export
#' @import stats
#' @references Heyde, C. C. (1963). On a property of the lognormal distribution. Journal of the Royal Statistical Society: Series B (Methodological), 25(2), 392-393.
#' @examples
#' cdflnormal(c(2,3),5)

cdflnormal=function(par,x)
{
  alpha=par[1]
  beta=par[2]

  g=dlnorm(x,alpha,beta)
  G=plnorm(x,alpha,beta)
  return(G)
}

#' @title Cumulative distribution function of the Pareto distribution
#'
#' @param par parameter vector of the Pareto distribution.
#' First parameter is the shape and second is the scale parameter
#' @param x vector of quantiles
#'
#' @return return the value of the cdf of the Pareto distribution
#' @export
#' @importFrom VGAM dpareto ppareto
#' @references Arnold, B. C. (1983). Pareto Distributions, International Cooperative Publishing House.
#' @examples
#' cdfpareto(c(2,5),2)

cdfpareto=function(par,x)
{
  k=par[1]
  beta=par[2]
  g=dpareto(x,k,beta)
  G=ppareto(x,k,beta)
  return(G)
}

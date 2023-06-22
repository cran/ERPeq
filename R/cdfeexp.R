
#' @title Cumulative distribution function of the exponentiated exponential distribution
#'
#' @param par parameter vector of the exponentiated exponential distribution.
#' First parameter is the shape, second is the scale parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the exponentiated exponential distribution
#' @export
#' @importFrom VGAM dgumbel pgumbel
#' @references Gupta, R. D., & Kundu, D. (1999). Theory & methods: Generalized exponential distributions. Australian & New Zealand Journal of Statistics, 41(2), 173-188.
#' @examples
#' cdfeexp(c(0.5,0.3),2)

cdfeexp=function(par,x)
{
  alpha=par[1]
  lambda=par[2]

  g=alpha*lambda*(1-exp(-lambda*x))^(alpha-1)*exp(-lambda*x)
  G=(1-exp(-lambda*x))^alpha
  return(G)
}

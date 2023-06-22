
#' @title Cumulative distribution function of the exponentiated Weibull distribution
#'
#' @param par parameter vector of the exponentiated Weibull distribution.
#' First parameter is the shape, second is the scale parameter and third parameter is shape parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the exponentiated Weibull distribution
#' @export
#' @importFrom VGAM dgumbel pgumbel
#' @references Mudholkar, G. S., & Srivastava, D. K. (1993). Exponentiated Weibull family for analyzing bathtub failure-rate data. IEEE transactions on reliability, 42(2), 299-302.
#' @examples
#' cdfew(c(0.5,0.3,0.6),2)

cdfew=function(par,x)
{
  alpha=par[1]
  beta=par[2]
  theta=par[3]

  g=alpha*theta/(beta^alpha)*x^(alpha-1)*exp(-(x/beta)^alpha)*(1-exp(-(x/beta)^alpha))^(theta-1)
  G=(1-exp(-(x/beta)^alpha))^theta
  return(G)
}

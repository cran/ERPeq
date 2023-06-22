
#' @title Probability density function of the inverse Weibull distribution
#'
#' @param par parameter vector of the inverse Weibull distribution.
#' First parameter is the shape and second is the scale parameter
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the inverse Weibull distribution
#' @export
#' @import stats
#' @references Mudholkar, G. S., & Kollia, G. D. (1994). Generalized Weibull family: a structural analysis. Communications in statistics-theory and methods, 23(4), 1149-1171.

#' @examples
#' pdfiweibull(c(2,3),5)

pdfiweibull=function(par,x)
{
  alpha=par[1]
  beta=par[2]

  g=beta*alpha^beta*x^(-beta-1)*exp(-(x/alpha)^-beta)
  G=exp(-(x/alpha)^-beta)
  return(g)
}


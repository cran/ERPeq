
#' @title Probability density function of the exponentiated Rayleigh  distribution
#'
#' @param par parameter vector of the exponentiated Rayleigh  distribution.
#' First parameter is the scale, second is the shape parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the exponentiated Rayleigh  distribution
#' @export
#' @importFrom VGAM dgumbel pgumbel
#' @references Vodă, V. G. (1976). Inferential procedures on a generalized Rayleigh variate. I. Aplikace matematiky, 21(6), 395-412.
#' @examples
#' pdfer(c(0.5,0.3),2)

pdfer=function(par,x)
{
  alpha=par[1]
  beta=par[2]

  g=2*alpha*beta*x*exp(-beta*x^2)*(1-exp(-beta*x^2))^(alpha-1)
  G=(1-exp(-beta*x^2))^(alpha)

  return(g)
}

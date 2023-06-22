#' @title Cumulative distribution function of the Rayleigh distribution
#'
#' @param par scale parameter vector of the Rayleigh distribution.
#'
#' @param x vector of quantiles
#'
#' @return return the value of the cdf of the Rayleigh distribution
#' @export
#' @importFrom VGAM drayleigh prayleigh
#' @references Siddiqui, M. M. (1964). Statistical inference for Rayleigh distributions. Journal of Research of the National Bureau of Standards, Sec. D, 68(9), 1005-1010.
#' @examples
#' cdfrayleigh(c(2),5)

cdfrayleigh=function(par,x)
{
  alpha=par[1]

  g=drayleigh(x,alpha)
  G=prayleigh(x,alpha)
  return(G)
}

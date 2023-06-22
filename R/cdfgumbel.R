
#' @title Cumulative distribution function of the gumbel distribution
#'
#' @param par parameter vector of the gumbel distribution.
#' First parameter is the location, second is the scale parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the gumbel distribution
#' @export
#' @importFrom VGAM dgumbel pgumbel
#' @references Gumbel, E. J. (1941). The return period of flood flows. The annals of mathematical statistics, 12(2), 163-190.
#' @examples
#' pdfgumbel(c(0.5,0.3),2)

cdfgumbel=function(par,x)
{
  alpha=par[1]
  beta=par[2]

  g=dgumbel(x,alpha,beta)
  G=pgumbel(x,alpha,beta)
  return(G)
}


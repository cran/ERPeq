
#' @title Probability density function of the Levy  distribution
#'
#' @param par parameter vector of the Levy  distribution.
#' First parameter is the location, second is the scale parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the Levy  distribution
#' @export
#' @importFrom VGAM dlevy plevy
#' @references Nolan, J. P. (2003). Modeling financial data with stable distributions. In Handbook of heavy tailed distributions in finance (pp. 105-130). North-Holland.
#' @examples
#' pdflevy(c(0.5,0.3),2)

pdflevy=function(par,x)
{
  alpha=par[1]
  beta=par[2]
  g=VGAM::dlevy(x,alpha,beta)
  G=VGAM::plevy(x,alpha,beta)

  return(g)
}

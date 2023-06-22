
#' @title Probability density function of the Pareto distribution
#'
#' @param par parameter vector of the Pareto distribution.
#' First parameter is the scale and second is the shape parameter
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the Pareto distribution
#' @export
#' @importFrom VGAM dpareto ppareto
#' @references Arnold, B. C. (1983). Pareto Distributions, International Cooperative Publishing House.
#'
#' @examples
#' pdfpareto(c(2,5),3)

pdfpareto=function(par,x)
{
  k=par[1]
  beta=par[2]
  g=dpareto(x,k,beta)
  G=ppareto(x,k,beta)
  return(g)
}



#' @title Probability density function of the generalized gamma distribution
#'
#' @param par parameter vector of the generalized gamma distribution.
#' First parameter is the dispersion, second is the location parameter and third is the family parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the generalized gamma distribution
#' @export
#' @importFrom rmutil dggamma pggamma
#' @references Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of mathematical statistics, 1187-1192.
#' @examples
#' pdfggamma(c(2,5,3),3)

pdfggamma=function(par,x)
{
  alpha=par[1]
  beta=par[2]
  c=par[3]

  g=dggamma(x,alpha,beta,c)
  G=pggamma(x,alpha,beta,c)
  return(g)
}


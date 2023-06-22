
#' @title Cumulative distribution function of the inverse gamma distribution
#'
#' @param par parameter vector of the inverse gamma distribution.
#' First parameter is the shape, second is the rate parameter.
#' @param x vector of observations or single value
#'
#' @return return the value of the pdf of the inverse gamma distribution
#' @export
#' @importFrom invgamma dinvgamma pinvgamma
#' @references Cook, J. D. (2008). Inverse gamma distribution. online: http://www. johndcook. com/inverse gamma. pdf, Tech. Rep.
#' @examples
#' cdfinvgamma(c(2,5,3),3)

cdfinvgamma=function(par,x)
{
  alpha=par[1]
  beta=par[2]

  g=dinvgamma(x,alpha,beta)
  G=pinvgamma(x,alpha,beta)
  return(G)
}


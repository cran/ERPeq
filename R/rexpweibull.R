
#' @title Generate random observations from exponentiated Weibull distribution
#'
#' @param n number of observations to be generated
#' @param beta scale parameter of the exponentiated Weibull distribution
#' @param theta shape parameter of the exponentiated Weibull distribution
#' @param alpha shape parameter of the exponentiated Weibull distribution
#'
#' @return return the random sample generated from exponentiated Weibull distribution
#' @export
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' rexpweibull(100,2,3,2)

rexpweibull=function(n,alpha,beta,theta)
{
  p=vector(length =n)
  p=runif(n)
  g=beta*(-log(1-p^(1/theta)))^(1/alpha)
  return(g)
}

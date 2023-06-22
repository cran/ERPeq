
#' @title Generate random observations from exponentiated Rayleigh distribution
#'
#' @param n number of observations to be generated
#' @param beta scale parameter of the exponentiated Rayleigh distribution
#' @param alpha shape parameter of the exponentiated Rayleigh distribution
#'
#' @return return the random sample generated from exponentiated exponential distribution
#' @export
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' rexprayleigh(100,2,3)

rexprayleigh=function(n,alpha,beta)
{
  p=vector(length =n)
  p=runif(n)
  g=(-beta/(log(1-p^(1/alpha))))^(-1/2)
  return(g)
}


#' @title Generate random observations from exponentiated exponential distribution
#'
#' @param n number of observations to be generated
#' @param alpha shape parameter of the exponentiated exponential distribution
#' @param lambda scale parameter of the exponentiated exponential distribution
#'
#' @return return the random sample generated from exponentiated exponential distribution
#' @export
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' rexpexp(100,2,3)

rexpexp=function(n,alpha,lambda)
{
p=vector(length =n)
p=runif(n)
  g=-log(1-p^(1/alpha))/lambda
  return(g)
}

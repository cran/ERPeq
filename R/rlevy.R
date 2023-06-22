
#' @title Generate random observations from Levy distribution
#'
#' @param n number of observations to be generated
#' @param mu location parameter of the Levy distribution
#' @param c scale parameter of the Levy distribution
#'
#' @return return the random sample generated from Levy distribution
#' @export
#' @importFrom pracma erfc erfinv erfcinv
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' rlevy(500,2,3)

rlevy=function(n,mu,c)
{
  p=vector(length =n)
  p=runif(n)
  g=mu+c/(qnorm(1-p/2))^2
  return(g)
}

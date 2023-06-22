
#' @title Generate random observations from Birnbaum-Saunders-Generalized Pareto distribution
#'
#' @param n number of observations to be generated from the Birnbaum-Saunders-Generalized Pareto
#' @param beta lower bound parameter of the
#' @param gamma shape parameter of the Birnbaum-Saunders-Generalized Pareto distribution
#' @param alpha scale parameter of the Birnbaum-Saunders-Generalized Pareto distribution
#'
#' @return return the random sample generated from scale parameter of the Birnbaum-Saunders-Generalized Pareto distribution distribution
#' @export
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' rbsgpd(100,2,3,5)

rbsgpd=function(n,beta,alpha,gamma)
{

qq=function(x){
    qt=-(((1-x)^gamma-1)*(1-x)^-gamma)/gamma
    t=beta*(alpha*qt/2+sqrt((alpha*qt/2)^2+1))^2
    return(t)
}

  p=vector(length =n)
  p=runif(n)
  g=qq(p)
  return(g)
}



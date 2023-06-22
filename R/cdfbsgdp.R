
#' @title Cumulative distribution function of the Birnbaum-Saunders-Generalized Pareto distribution
#' @param par parameter vector of the Birnbaum-Saunders-Generalized Pareto distribution.
#' First parameter is the shape, second parameter is the scale parameter. Third parameter is the lower bound parameter.
#'
#' @param x vector of observations or single value
#'
#' @return return the value of the cdf of the Birnbaum-Saunders-Generalized Pareto  distribution
#' @export
#' @importFrom VGAM dgumbel pgumbel
#' @references Altun, E., Ozel, G. A novel approach to probabilistic hazard assessment: BSGPD model. (Under Review)
#' @examples
#' cdfbsgdp(c(0.5,2,0.5),3)
cdfbsgdp=function(par,x){
  alpha=par[1]
  gamma=par[2]
  beta=par[3]

  at=(sqrt(x/beta)-sqrt(beta/x))/alpha
  att=x^(-3/2)*(x+beta)/(2*alpha*sqrt(beta))

  f=1-(1+gamma*at)^(-1/gamma)
  return(f)
}


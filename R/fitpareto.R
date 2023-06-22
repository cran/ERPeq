#' @title Fitting the Pareto distribution
#' @param starts A vector defining the starting values for the Nelder-Mead algorithm.
#' @param data A vector containing the observations
#'
#' @return List the estimated parameters of the distribution with standard errors and goodness-of-fit statistics.
#' @export
#' @importFrom stats ks.test optim
#' @importFrom VGAM dpareto ppareto rpareto
#' @import graphics
#' @examples
#' library(VGAM)
#' data=VGAM::rpareto(500,5,2)
#' fitpareto(starts =c(2),data=data)

fitpareto=function(starts,data){
  x=NULL
  data2=data[data>min(data)]
  pdfpareto=function(par,x)
  {
    beta=par[1]

    g=VGAM::dpareto(x,min(data),beta)
    G=VGAM::ppareto(x,min(data),beta)
    return(g)
  }

  cdfpareto=function(par,x)
  {
    beta=par[1]

    g=VGAM::dpareto(x,min(data),beta)
    G=VGAM::ppareto(x,min(data),beta)
    return(G)
  }
  ll = function(par, x) {-sum(log(pdfpareto(par, x))) }
  result = optim(par = starts, fn = ll, x = data2,method = "Nelder-Mead", hessian = TRUE)
  parameters=result$par
  hess=result$hessian
  datasort=sort(data2)
  n=length(data2)
  p=length(parameters)
  logll = -1 * ll(parameters, data2)
  AICc = -2 * logll + 2 * p + 2 * (p * (p + 1))/(n - p - 1)
  AIC = -2 * logll + 2 * p
  BIC = -2 * logll + p * log(n)
  HQIC = -2 * logll + 2 * log(log(n)) * p
  KS = ks.test(x = data, y = "cdfpareto", par = as.vector(parameters))
  result = (list(KS = KS, mle = parameters,lower.bound.parameter=min(data),
                 AIC = AIC, CAIC = AICc, BIC = BIC, HQIC = HQIC,
                 StdError = sqrt(diag(solve(hess))), logll = result$value,
                 Convergence = result$convergence))
  #class(result) <- "list"
  oldpar = par(mfrow=c(1,2),no.readonly = TRUE)
  on.exit(par(oldpar))
  hist(data, freq = FALSE,main="",xlab="x",
       ylab="f(x)",lwd=1,col="white")
  curve(pdfpareto(parameters,x),min(data),max(data+1),add=T,col="1",lwd=2,lty=1)
  title("Fitted density")
  probDist <- cdfpareto(parameters,data)
  plot(ppoints(length(data)), sort(probDist), main =, xlab = "Observed Probability", ylab = "Expected Probability",col=1)
  abline(0,1)
  title("Probability-Probability Plot")
  return(result)
}


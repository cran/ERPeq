#' @title Fitting the Levy distribution
#' @param starts A vector defining the starting values for the Nelder-Mead algorithm.
#' @param data A vector containing the observations
#'
#' @return List the estimated parameters of the distribution with standard errors and goodness-of-fit statistics.
#' @export
#' @importFrom stats ks.test optim
#' @importFrom VGAM dlevy plevy rlevy
#' @import graphics
#' @examples
#' library(VGAM)
#' data=ERPeq::rlevy(100,2,0.1)
#' fitlevy(starts =c(0.1),data=data)

fitlevy=function(starts,data){
  x=NULL
  data2=data[data>min(data)]
  pdflevy1=function(par,x)
  {
    c=par[1]
    mu=min(data)
   g=sqrt(c/(2*pi))*exp(-c/(2*(x-mu)))/(x-mu)^(3/2)
  return(g)
   }

  cdflevy1=function(par,x)
  {
    c=par[1]
    mu=min(data)
    G=2-2*pnorm(sqrt(c/(x-mu)),0,1)
    return(G)
  }
  ll = function(par, x) {-sum(log(pdflevy1(par, x))) }
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
  KS = ks.test(x = data2, y = "cdflevy1", par = as.vector(parameters))
  result = (list(KS = KS, mle = parameters,lower.bound.parameter=min(data),
                 AIC = AIC, CAIC = AICc, BIC = BIC, HQIC = HQIC,
                 StdError = sqrt(diag(solve(hess))), logll = result$value,
                 Convergence = result$convergence))
  #class(result) <- "list"
  oldpar = par(mfrow=c(1,2),no.readonly = TRUE)
  on.exit(par(oldpar))
  hist(data, freq = FALSE,main="",xlab="x",
       ylab="f(x)",lwd=1,col="white")
  curve(pdflevy1(parameters,x),min(data),max(data+1),add=T,col="1",lwd=2,lty=1)
  title("Fitted density")
  probDist <- cdflevy1(parameters,data2)
  plot(ppoints(length(data2)), sort(probDist), main =, xlab = "Observed Probability", ylab = "Expected Probability",col=1)
  abline(0,1)
  title("Probability-Probability Plot")
  return(result)
}


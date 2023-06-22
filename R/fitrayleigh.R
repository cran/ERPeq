#' @title Fitting the Rayleigh  distribution
#' @param starts A vector defining the starting values for the Nelder-Mead algorithm.
#' @param data A vector containing the observations
#'
#' @return List the estimated parameters of the distribution with standard errors and goodness-of-fit statistics.
#' @export
#' @importFrom stats ks.test optim
#' @importFrom VGAM drayleigh prayleigh rrayleigh
#' @import graphics
#' @examples
#' library(VGAM)
#' data=rrayleigh(500,2)
#' fitrayleigh(starts =c(2),data=data)

fitrayleigh=function(starts,data){
  x=NULL
  ll = function(par, x) {-sum(log(pdfrayleigh(par, x))) }
  result = optim(par = starts, fn = ll, x = data,method = "Nelder-Mead", hessian = TRUE)
  parameters=result$par
  hess=result$hessian
  datasort=sort(data)
  n=length(data)
  p=length(parameters)
  logll = -1 * ll(parameters, data)
  AICc = -2 * logll + 2 * p + 2 * (p * (p + 1))/(n - p - 1)
  AIC = -2 * logll + 2 * p
  BIC = -2 * logll + p * log(n)
  HQIC = -2 * logll + 2 * log(log(n)) * p
  KS = ks.test(x = data, y = "cdfrayleigh", par = as.vector(parameters))
  result = (list(KS = KS, mle = parameters,
                 AIC = AIC, CAIC = AICc, BIC = BIC, HQIC = HQIC,
                 StdError = sqrt(diag(solve(hess))), logll = result$value,
                 Convergence = result$convergence))
  #class(result) <- "list"
  oldpar = par(mfrow=c(1,2),no.readonly = TRUE)
  on.exit(par(oldpar))
  hist(data, freq = FALSE,main="",xlab="x",
       ylab="f(x)",lwd=1,col="white")
  curve(pdfrayleigh(parameters,x),min(data),max(data+1),add=T,col="1",lwd=2,lty=1)
  title("Fitted density")
  probDist <- cdfrayleigh(parameters,data)
  plot(ppoints(length(data)), sort(probDist), main =, xlab = "Observed Probability", ylab = "Expected Probability",col=1)
  abline(0,1)
  title("Probability-Probability Plot")
  return(result)
}


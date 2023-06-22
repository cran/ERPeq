#' @title Fitting the Birnbaum-Saunders-Generalized Pareto distribution
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
#' data=ERPeq::rbsgpd(500,5,0.7,0.2)
#' fitbsgpd(starts =c(1,1),data=data)

fitbsgpd=function(starts,data){
  x=NULL
  data=data[data>min(data)]
  pdfbsgp=function(par,x){
    alpha=par[1]
    beta=min(data)
    gamma=par[2]

    at=(sqrt(x/beta)-sqrt(beta/x))/alpha
    att=x^(-3/2)*(x+beta)/(2*alpha*sqrt(beta))

    f=(1+gamma*at)^(-(1/gamma+1))*att
    return(f)
  }

  cdfbsgp=function(par,x){
    alpha=par[1]
    beta=min(data)
    gamma=par[2]

    at=(sqrt(x/beta)-sqrt(beta/x))/alpha
    att=x^(-3/2)*(x+beta)/(2*alpha*sqrt(beta))

    f=1-(1+gamma*at)^(-1/gamma)
    return(f)
  }
  ll = function(par, x) {-sum(log(pdfbsgp(par, x))) }
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
  KS = ks.test(x = data, y = "cdfbsgp", par = as.vector(parameters))
  result = (list(KS = KS, mle = parameters,lower.bound.parameter=min(data),
                 AIC = AIC, CAIC = AICc, BIC = BIC, HQIC = HQIC,
                 StdError = sqrt(diag(solve(hess))), log_ll = result$value,
                 Convergence = result$convergence))
  #class(result) <- "list"
  oldpar = par(mfrow=c(1,2),no.readonly = TRUE)
  on.exit(par(oldpar))
  hist(data, freq = FALSE,main="",xlab="x",
       ylab="f(x)",lwd=1,col="white")
  curve(pdfbsgp(parameters,x),min(data),max(data+1),add=T,col="1",lwd=2,lty=1)
  title("Fitted density")
  probDist <- cdfbsgp(parameters,data)
  plot(ppoints(length(data)), sort(probDist), main =, xlab = "Observed Probability", ylab = "Expected Probability",col=1)
  abline(0,1)
  title("Probability-Probability Plot")
  return(result)
}


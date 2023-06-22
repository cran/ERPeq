#' @title Probabilistic estimation of earthquake recurrence interval using exponentiated Weibull distribution
#' @description Computes the probability of an earthquake within a specified time "r" and elapsed time "te".
#'
#' @param fit Fit is the fitexpweibull object. See ?fitexpweibull for details.
#' @param r The specified time in which the probability of an earthquake is desired to be calculated.
#' @param te Elapsed time since the last earthquake
#' @references Pasari, S. and Dikshit, O. (2014). Impact of three-parameter Weibull models in probabilistic assessment of earthquake hazards. Pure and Applied Geophysics, 171, 1251-1281.
#' @export
#' @return A numeric value
#' @importFrom stats ks.test optim
#' @importFrom VGAM dpareto ppareto rpareto
#' @examples
#' fit=fitexpweibull(c(1,1,1),data=data_earthquake_7)
#' expweicp(fit,r=2,te=5)

expweicp=function(fit,r,te){
  if (missingArg(fit) == TRUE)
    stop("fit object is missing. The fit object must be fitexpweibull object. See ?fitexpweibull for details.")
  if (missingArg(r) == TRUE)
    stop("r object is missing.")
  if (missingArg(te) == TRUE)
    stop("te object is missing.")

  GG=cdfew(fit$mle,r+te)
  G=cdfew(fit$mle,te)
  f=(GG-G)/(1-G)
  return(f)
}

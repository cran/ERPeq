#' @title Probabilistic estimation of earthquake recurrence interval using exponentiated Rayleigh distribution
#' @description Computes the probability of an earthquake within a specified time "r" and elapsed time "te".
#'
#' @param fit Fit is the fitexprayleigh object. See ?fitexprayleigh for details.
#' @param r The specified time in which the probability of an earthquake is desired to be calculated.
#' @param te Elapsed time since the last earthquake
#' @return A numeric value
#' @references Pasari, S. and Dikshit, O. (2014). Impact of three-parameter Weibull models in probabilistic assessment of earthquake hazards. Pure and Applied Geophysics, 171, 1251-1281.
#' @export
#' @importFrom stats ks.test optim
#' @importFrom VGAM dpareto ppareto rpareto
#' @examples
#' fit=fitexprayleigh(c(0.5,0.5),data=data_earthquake_7)
#' expraycp(fit,r=2,te=5)

expraycp=function(fit,r,te){
  if (missingArg(fit) == TRUE)
    stop("fit object is missing. The fit object must be fitexprayleigh object. See ?fitexprayleigh for details.")
  if (missingArg(r) == TRUE)
    stop("r object is missing.")
  if (missingArg(te) == TRUE)
    stop("te object is missing.")

  GG=cdfer(fit$mle,r+te)
  G=cdfer(fit$mle,te)
  f=(GG-G)/(1-G)
  return(f)
}

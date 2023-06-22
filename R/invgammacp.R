#' @title Probabilistic estimation of earthquake recurrence interval using inverse gamma distribution
#' @description Computes the probability of an earthquake within a specified time "r" and elapsed time "te".
#'
#' @param fit Fit is the fitinvgamma object. See ?fitinvgamma for details.
#' @param r The specified time in which the probability of an earthquake is desired to be calculated.
#' @param te Elapsed time since the last earthquake
#' @references Pasari, S. and Dikshit, O. (2014). Impact of three-parameter Weibull models in probabilistic assessment of earthquake hazards. Pure and Applied Geophysics, 171, 1251-1281.
#' @export
#' @return A numeric value
#' @importFrom stats ks.test optim
#' @importFrom VGAM dpareto ppareto rpareto
#' @examples
#' fit=fitinvgamma(c(1,1),data=data_earthquake_7)
#' invgammacp(fit,r=2,te=5)

invgammacp=function(fit,r,te){
  if (missingArg(fit) == TRUE)
    stop("fit object is missing. The fit object must be fitinvgamma object. See ?fitinvgamma for details.")
  if (missingArg(r) == TRUE)
    stop("r object is missing.")
  if (missingArg(te) == TRUE)
    stop("te object is missing.")

  GG=cdfinvgamma(fit$mle,r+te)
  G=cdfinvgamma(fit$mle,te)
  f=(GG-G)/(1-G)
  return(f)
}

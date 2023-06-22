#' @title Probabilistic estimation of earthquake recurrence interval using Rayleigh distribution
#' @description Computes the probability of an earthquake within a specified time "r" and elapsed time "te".
#'
#' @param fit Fit is the fitrayleigh object. See ?fitrayleigh for details.
#' @param r The specified time in which the probability of an earthquake is desired to be calculated.
#' @param te Elapsed time since the last earthquake
#' @references Pasari, S. and Dikshit, O. (2014). Impact of three-parameter Weibull models in probabilistic assessment of earthquake hazards. Pure and Applied Geophysics, 171, 1251-1281.
#' @export
#' @return A numeric value
#' @importFrom stats ks.test optim
#' @importFrom VGAM dpareto ppareto rpareto
#' @examples
#' fit=fitrayleigh(c(1),data=data_earthquake_7)
#' rayleighcp(fit,r=2,te=5)

rayleighcp =function(fit,r,te){
  if (missingArg(fit) == TRUE)
    stop("fit object is missing. The fit object must be fitrayleigh object. See ?fitrayleigh for details.")
  if (missingArg(r) == TRUE)
    stop("r object is missing.")
  if (missingArg(te) == TRUE)
    stop("te object is missing.")

  GG=cdfrayleigh(fit$mle,r+te)
  G=cdfrayleigh(fit$mle,te)
  f=(GG-G)/(1-G)
  return(f)
}

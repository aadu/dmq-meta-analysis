#' Highest density interval
#'
#' @import R2jags
#' @param x Data
#' @param mass Percent of posterior distrubtion to include
#'   in highest density interval; Default = 0.95.
#' @return Lower and upper bounds of density interval
#' @export
hdi = function(x, mass=0.95) {
  sorted   = sort(x)
  # Number of elements inside the HDI
  middle   = floor(mass * length(x))
  # Number of elements outside the HDI
  outside  = length(x) - middle
  # Vector of length outside
  ci = rep(0, outside)
  for (i in 1:outside) {
    ci[i] = sorted[i + middle] - sorted[i]
  }
  hdi.min = sorted[which.min(ci)]
  hdi.max = sorted[which.min(ci) + middle]
  return(c(hdi.min , hdi.max))
}


#' @describeIn hdi
#' @export
get_hdi = function(var.name, fitobj){
  hdi(as.vector(fitobj$BUGSoutput$sims.list[[var.name]]))
}

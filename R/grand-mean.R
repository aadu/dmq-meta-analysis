#' Grand mean
#'
#' @param m vector of Means
#' @param n vector of sample sizes
#' @return Grand mean
#' @export
grand.mean <- function(m, n) {weighted.mean(m, n)}


#' @describeIn grand.mean
#' @export
grand.sd   <- function(s, m, n) {sqrt(weighted.mean(s^2 + m^2, n) -
                                        weighted.mean(m, n)^2)}

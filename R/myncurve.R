#' Creates a norm curve with a probability region
#'
#' @param mu The mean for the distribution
#' @param sigma The standard deviation of the distribution
#' @param a The number to calculate probability using
#'
#' @return Plots a norm curve with a probability region
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics polygon
#'
#' @export
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  xcurve = seq(mu - 3 * sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col="Red")
  area <-pnorm(a, mu, sigma)
  arear <- round(area, 4)
  text(x = a, y = dnorm(a, mu, sigma), paste0("Area = ", arear))

  return(list(mu = mu, sigma = sigma, area = arear))
}

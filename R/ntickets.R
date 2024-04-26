#' Calculate the number of tickets given the overbooking problem within the acceptable range
#'
#' @param N The number of tickets sold
#' @param gamma The acceptable range of overbooking
#' @param p The probability of a ticket holder showing up
#'
#' @importFrom stats pbinom
#'
#' @return A list containing the number of tickets for the discrete and continuous cases, the number of tickets sold, the probability of a ticket holder showing up, and the acceptable range of overbooking
#'
#' @export ntickets
ntickets <- function(N, gamma, p) {
  # create the range
  n_range = seq(N, N + 20, by = 1)

  # discrete function
  discrete_func <- function(n) {
    1 - gamma - pbinom(N - 1, n, p)
  }
  disc_vals <- sapply(n_range, function(n) discrete_func(n))
  nd <- n_range[which.min(abs(disc_vals))]

  # plotting
  plot(n_range, disc_vals, type = "l", col = "blue", xlab = "n", ylab = "Objective Function", main = "Objective Function vs. n (Discrete)")
  abline(v = nd, col = "red", lty = 2)
  text(nd, 0, labels = paste("n =", nd), pos = 4)

  # normal function
  norm_func <- function(n) {
    1 - gamma - pnorm(N - 0.5, n * p, sqrt(n * p * (1 - p)))
  }
  norm_vals <- sapply(n_range, function(n) norm_func(n))
  nc <- n_range[which.min(abs(norm_vals))]

  # plotting
  plot(n_range, norm_vals, type = "l", col = "red", xlab = "n", ylab = "Objective Function", main = "Objective Function vs. n (Continuous)")
  abline(v = nc, col = "blue", lty = 2)
  text(nc, 0, labels = paste("n =", nc), pos = 4)

  # return the list
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

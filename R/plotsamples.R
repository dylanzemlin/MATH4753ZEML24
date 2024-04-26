#' Plots a given number of iterations that have a size of choose(n)
#'
#' @param n The number of samples
#' @param iter The number of iterations
#' @param time The time to wait
#'
#' @return Plots that represent sample sizes
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @export
plotsamples <- function(n, iter = 10, time = 0.5)
{
  for(i in 1 : iter)
  {
    s <- sample(1:10, n, replace = TRUE)
    sf <- factor(s, levels = 1:10)
    barplot(table(sf) / n, beside = TRUE, col = rainbow(10),
            main = paste("Example Sample()", " Iteration ", i, " n = ", n, sep = ""),
            ylim = c(0,0.2))

    Sys.sleep(time)
  }
}

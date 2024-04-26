#' Plots a linear piece-wise regression
#'
#' @param xk The x value to split the regression
#' @param dataframe The data frame to plot
#' @param xvar The name of the x var
#' @param yvar The name of the y var
#'
#' @return Returns a plot of the piece-wise regression
#'
#' @importFrom graphics abline curve text
#' @importFrom stats lm as.formula
#'
#' @export
customplot <- function(xk, dataframe, xvar, yvar){
  xvar <- noquote(xvar)
  yvar <- noquote(yvar)

  df = within(dataframe, X <- (dataframe[[xvar]] - xk) * (dataframe[[xvar]] > xk))
  lmfunc <- as.formula(paste(paste(yvar, xvar, sep = "~"), "X", sep = "+"))
  lmp = lm(lmfunc, data = df)
  coef = summary(lmp)$coefficients[,"Estimate"]

  f = function(x) {
    coef[1] + coef[2] * (x) + coef[3] * (x-xk) * (x-xk>0)
  }

  plot(dataframe, main = "Piecewise Regression", pch=21)
  curve(f, add = TRUE, lwd = 2, col = "Blue")
  abline(v = xk)
  text(xk, 16, paste("R sq. = ", round(summary(lmp)$r.squared, 4)))
}

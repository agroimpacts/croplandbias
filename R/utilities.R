#' GAM Prediction function
#' @param model Model object
#' @param newdat data.frame havng dummy values for predictors
#' @return Vector of predicted values with confidence intervals, for curve 
#' plotting
#' @keywords internal
#' @export
predfunc <- function(model, newdat) {
  pred <- predict(model, newdata = newdat, type = "response", se.fit = TRUE)
  pdat <- transform(newdat, fit = pred$fit)
  pdat <- transform(pdat, up = fit + (1.96 * pred$se.fit),
                    lo = fit - (1.96 * pred$se.fit))
  return(pdat)
}

#' Predict values from single GAM model term
#' @param model Model object
#' @param newdat data.frame havng dummy values for predictors
#' @return Vector of predicted values with confidence intervals, for curve 
#' plotting
#' @keywords internal
#' @export 
termfunc <- function(model, newdat) {
  pred <- predict(model, newdata = newdat, type = "terms", se.fit = TRUE)
  pdat <- transform(newdat, fit = pred$fit[, 1] + coef(model)[1])
  pdat <- transform(pdat, up = fit + (1.96 * pred$se.fit[, 1]),
                    lo = fit - (1.96 * pred$se.fit[, 1]))
  return(pdat)
}

#' Create arbitrary polygon on plot
#' @param x coordinate on which to center polygon on x-axis
#' @param y upper and lower coordinates for polygon on y-axis
#' @param w polygon width in x dimension
#' @param col fill color
#' @param bcol border color
#' @param lwd border width
#' @return plotted polygon on previously called plot
#' @keywords internal
#' @export
polyfunc2 <- function(x, y, w = 0.5, col, bcol, lwd = 1) {
  x <- c((x - 0.5 * w), (x + 0.5 * w))
  polygon(cbind.data.frame("x" = c(x, rev(x)), "y" = y[c(1, 1, 2, 2)]), 
          col = col, lwd = lwd, border = bcol)
}

#' Make any color transparent
#' @param someColor Color name, e.g. "red"
#' @param alpha Transparency value, from 0-255
#' @return Transparent color value
#' @note Function found at 
#' stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
#' @keywords internal
#' @export
makeTransparent<-function(someColor, alpha = 100) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, maxColorValue=255)
  })
}

#' For over-plotting custom point or short horizontal lines
#' @param x x-coordinate for point or center of line
#' @param y single y-coordinate
#' @param w if line, specify total width it should be
#' @param col Color
#' @param size cex value for points, lwd for line
#' @param type "line" or "pt"
#' @param pch For points, e.g. "*", 20
#' @return point or lines on plot
#' @keywords internal
#' @export
lpfunc <- function(x, y, w = NULL, col, size = 1, type, pch = NULL) {
  if(type == "line") {
    lines(c((x - 0.5 * w), (x + 0.5 * w)), rep(y, 2), col = col, lwd = size)
  } else if(type == "pt") {
    points(x, y, pch = pch, cex = size, col = col)
  } else {
    stop("Need to specify either 'pt' or 'line'")
  }
}

#' Helps get paths to system files installed with package
#' @param x Name of system file to find
#' @param y Sub-directory under inst/extdata where file is stored (details)
#' @return Full path to system file
#' @details For argument y, use "lc" for landcover, "yl" for "yield", "cl" 
#' for "climate", "ab" for "abm", "cb" for "carbon"
#' @keywords internal
#' @export 
spathfunc <- function(x, y = "lc") {
  mat <- cbind(c("lc", "yl", "cl", "ab", "cb"), 
               c("landcover", "yieldprod", "climate", "abm", "carbon"))
  base <- mat[which(mat[, 1] == y), 2]
  system.file(paste0("extdata/", base), x, package = "croplandbias")
}



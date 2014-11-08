# function-devel.R

#' Generic function to create adjacent rectangular polygons from vectors of coordinates passed to it
#' 
#' @param x Vector of x coordinates, arranged in ascending order
#' @param y Vector of y coordinates, arrange in acending order
#' @return A polygon "chain": segmented polygons with interior vertical x or horizontal y axis
#' @details This is modeled on the rect_coords function used by flex_legend, but the resulting polygon 
#' can be of arbitrary shape. It can be used to make either a horizontal or vertical chain of polygons.  If 
#' horizontal, then x should be an ascending vector providing one x value per polygon 
poly_chain <- function(x, y) {
  rects <- lapply(1:(length(x) - 1), function(z) {
    rbind("ll" = c("x" = x[z], y2[z]),
          "ul" = c("x" = x[z], y1[z]),
          "ur" = c("x" = x[z + 1], y1[z + 1]),    
          "lr" = c("x" = x[z + 1], y2[z + 1]), 
          "ll" = c("x" = x[z], y2[z]))
  })
  return(rects)
}
#' Generic function to create adjacent rectangular polygons from vectors of coordinates passed to it
#' 
#' @param x Vector of x coordinates, arranged in ascending order
#' @param y Vector of y coordinates, arrange in acending order
#' @param fillcol A 1 to n - 1 length vector of colors for filling. n = length of x or y (the longest)
#' @param linecol A 1 to n - 1 length vector of polygon border colors. n = length of x or y (the longest)
#' @return A polygon "chain": segmented polygons with interior vertical x or horizontal y axis
#' @details This is modeled on the rect_coords function used by flex_legend, but the resulting polygon 
#' can be of arbitrary shape. It can be used to make either a horizontal (i.e. a sequence of polygons
#' separated on the X-axis, with the long dimension filling the Y-axis) or vertical chain of polygons (the
#' opposite). To make the former, provide a vector giving the X-coordinates separating each polygon, and a 2 
#' parameter vector specifying the lower and upper Y coordinates. For the opposite case, give the left and 
#' right X coordinates to X, and a vector of Y coordinates to Y. 
#' @examples
#' a <- c(-50, -25, 0, 25, 50)
#' b <- c(-1, 21)
#' plot(c(-100, 100), c(-1, 22), pch = "")
#' rect_shade(b, a)
#' 
#' plot(c(-100, 100), c(-1, 22), pch = "")
#' rect_shade(a, b)
#' 
#' cols <- sapply(c(0.8, 0.5, 0.2, 0.1), function(x) rgb(1, 0, 0, alpha = x))
#' plot(c(-1, 22), c(-100, 100), pch = "")
#' # rect_shade(b, a, fillcol = cols)
#' rect_shade(b, a, fillcol = cols, linecol = cols)
#' @export

rect_shade <- function(x, y, fillcol = "grey", linecol = "black", lwd = 1) {
  vlengths <- c(length(x), length(y))
  xydir <- which.max(vlengths)
  if(length(fillcol) != (vlengths[xydir] - 1)) fillcol <- rep(fillcol[1], vlengths[xydir] - 1)
  if(length(linecol) != (vlengths[xydir] - 1)) linecol <- rep(linecol, vlengths[xydir] - 1)
  rects <- lapply(1:(vlengths[xydir] - 1), function(j) {
      if(xydir == 1) {
        pols <- polygon(x = c(x[j], rep(x[j + 1], 2), x[j]), y = c(y[1], y[1], y[2], y[2]), col = fillcol[j], 
                        border = linecol[j], lwd = lwd)
      }
      if(xydir == 2) {
        pols <- polygon(x = c(x[1], x[1], x[2], x[2]), y = c(y[j], rep(y[j + 1], 2), y[j]), col = fillcol[j], 
                        border = linecol[j], lwd = lwd)
      }
    })
}



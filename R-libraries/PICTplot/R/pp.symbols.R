#' Add symbols to a PICT plot
#'
#' @description Draws symbols on a plot. One of six symbols; circles, squares, rectangles, stars, thermometers, and boxplots, can be plotted at a specified set of x and y coordinates. Specific aspects of the symbols, such as relative size, can be customized by additional parameters.
#' @param x, y  the x and y co-ordinates for the centres of the symbols.
#' @details Adds symbols to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.symbols"<-
function(x, y=NULL, add=TRUE, ...)
{
  if(is.matrix(x))
    x <- as.data.frame(x)
  if(is.list(x)) {
    if(all(c("long","lat") %in% names(x))) {
      long <- x$long
      lat  <- x$lat
    } else if(all(c("x","y") %in% names(x))) {
      long <- x$x
      lat  <- x$y
    } else {
      long <- x[[1]]
      lat  <- x[[2]]
    }
  } else {
    long <- x
    lat  <- y
  }
  symbols(.PICT$mlong(long), .PICT$mlat(lat), add=add, ...)
  invisible()
}

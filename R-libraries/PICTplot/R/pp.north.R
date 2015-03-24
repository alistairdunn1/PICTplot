#' Add north arrow to a PICT plot
#'
#' @description Draws an arrow indicating north
#' @param x, y  numeric vectors of coordinates where tarrow should be written
#' @param size size (in proportion of y limits of the plot) or the arrow
#' @param label if true, then adds a label with 'N' at the top of the arrow
#' @param length
#' @param x, y
#' @details Adds a north pointer arrow to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.north" <-
function(x, y, size = 0.1, label = T, length = 0.25, angle = 30, code = 2, ...)
{
  if(missing(x) | missing(y)) {
    cat("Using function \"locator(1)\" to place point\n")
    x <- pp.locator(1)
    long <- x$x
    lat <- x$y
    print(unlist(c(long, lat)))
  }
  size <- size * 0.5
  long1 <- long - diff(.PICT$xlim) * size * 0.4
  long2 <- long + diff(.PICT$xlim) * size * 0.4
  lat1 <- lat - 0.67 * diff(.PICT$ylim) * size * 2
  lat2 <- lat + 0.33 * diff(.PICT$ylim) * size * 2
  pp.arrows(long, lat1, long, lat2,length=length,code=code,angle=angle,...)
  pp.segments(long1, lat, long2, lat,...)
  if(label)
    pp.text(long, lat2 + diff(.PICT$ylim) * 0.02, "N", ...)
  invisible()
}

#' Add arrows to a PICT plot
#'
#' @description Adds arrows to a PICT plot
#' @param x0, y0 coordinates of points from which to draw.
#' @param x1, y1 coordinates of points to which to draw. At least one must the supplied
#' @param length length of the edges of the arrow head (in inches).
#' @param ... arguments passes to arrows()
#' @details Adds arrows to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.arrows"<-
function(x1, y1, x2, y2, length = 0.1, ...)
{
  mlong <- .PICT$mlong
  mlat  <- .PICT$mlat
  arrows(mlong(x1), mlat(y1), mlong(x2), mlat(y2), length = length, ...)
  invisible()
}

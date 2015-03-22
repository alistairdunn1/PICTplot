#' Add segments to a PICT plot
#'
#' @description Draw line segments between pairs of points.
#' @param x0, y0  coordinates of points from which to draw.
#' @param x1, y1  coordinates of points to which to draw. At least one must be supplied.
#' @export
#'
"pp.segments" <-
function(x0, y0, x1, y1, ...)
{
  segments(.PICT$mlong(x0), .PICT$mlong(y0), .PICT$mlong(x1), .PICT$mlong(y1), ...)
  invisible()
}

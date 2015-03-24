#' Add a legend to a PICT plot
#'
#' @description Adds a legend to a plot.
#' @param x, y  the x and y co-ordinates for the location of legend
#' @param ... arguments passes to legend()
#' @return Returns no value
#' @export
#'
"pp.legend"<-
function(x, y, ...)
{
  legend(.PICT$mlong(x), .PICT$mlat(y), ...)
  invisible()
}

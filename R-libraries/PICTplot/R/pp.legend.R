#' Add a legend to a PICT plot
#'
#' @description This function adds a legend to a plot.
#' @param x, y  the x and y co-ordinates for the location of legend.
#' @export
#'
"pp.legend"<-
function(x, y, ...)
{
  legend(.PICT$mlong(x), .PICT$mlat(y), ...)
  invisible()
}

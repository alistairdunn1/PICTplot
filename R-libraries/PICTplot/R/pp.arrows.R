# TODO

#' Add arrows to a PICT plot
#'
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

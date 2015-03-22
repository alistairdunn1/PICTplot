#' return a lat/long location from a PICT plot
#'
#' @export
#'
"pp.locator"<-
function(...)
{
  RES <- locator(...)
    RES$x <- .PICT$mlonginv(RES$x)
    RES$y <- .PICT$mlatinv(RES$y)
  return(RES)
}

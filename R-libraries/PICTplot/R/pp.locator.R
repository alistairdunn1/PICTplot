#' return a lat/long location from a PICT plot
#'
#' @description Uses the cursor to identify a lat/long location on a PICT plot
#' @param ... additional parameters passes to locator()
#' @return Returns a list of lat/long points identifed by the cursor
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

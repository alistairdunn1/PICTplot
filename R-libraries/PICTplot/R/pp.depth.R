#' Add depth contours to a PICT plot
#'
#' @description Adds depth contours to a PICT plot
#' @param contour The depth contours to be plotted
#' @details Adds a bathymetric contour to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.depth"<-
function(contour = NULL, lty = 1, lwd = 1, col = 1)
{
  if(is.null(contour)) stop("contour must be specified")
  cnam <- paste("m", contour, sep = "")
  if(exists(cnam)) {
    pp.lines(get(cnam), lty = lty, lwd = lwd, col = col)
  } else {
    message("The selected depth contour does not exist")
 }
  invisible()
}


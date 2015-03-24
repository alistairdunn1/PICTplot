#' Add a polygon to a PICT plot
#'
#' @description Draws polygons whose vertices are given in x and y.
#' @param x, y  the x and y co-ordinates for the centres of the symbols.
#' @details Adds a polygon to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.polygon"<-
function(x, y=NULL, ...)
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
  polygon(.PICT$mlong(long), .PICT$mlat(lat), ...)
  invisible()
}

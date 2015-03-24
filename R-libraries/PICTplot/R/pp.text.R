#' Add text to a PICT plot
#'
#' @description Draws the strings given in the vector labels at the coordinates given by x and y. y may be missing since xy.coords(x, y) is used for construction of the coordinates.
#' @param x, y  numeric vectors of coordinates where the text labels should be written. If the length of x and y differs, the shorter one is recycled.
#' @details Adds text to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.text"<-
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
  text(.PICT$mlong(long), .PICT$mlat(lat), ...)
  invisible()
}

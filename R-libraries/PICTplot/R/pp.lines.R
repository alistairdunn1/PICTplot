#' Add lines to a PICT plot
#'
#' @description Draws lines whose vertices are given in x and y.
#' @param x, y  numeric vectors of coordinates where the lines should be written. If the length of x and y differs, the shorter one is recycled.
#' @details Adds a line to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.lines"<-
function(x, y = NULL, ...)
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
  lines(.PICT$mlong(long), .PICT$mlat(lat), ...)
  invisible()
}

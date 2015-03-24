#' Add an image to a PICT plot
#'
#' @description See R graphics function image for details of parameters that are passed to image by ...
#' @param x, y  locations of grid lines at which the values in z are measured. These must be finite, non-missing and in (strictly) ascending order. By default, equally spaced values from 0 to 1 are used.
#' @param z a matrix containing the values to be plotted (NAs are allowed).
#' @param ... arguments passed to image()
#' @return Returns no value
#' @export
#'
"pp.image" <-
function(x = 1:dim(z)[1], y = 1:dim(z)[2], z, ...)
{
  image(.PICT$mlong(x), .PICT$mlat(y), z, ...)
  invisible()
}

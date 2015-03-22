#' Add an image to a PICT plot
#'
#' @description See R graphics function image for details of parameters that are passed to image by ...
#'
#' @export
#'
"pp.image" <-
function(x = 1:dim(z)[1], y = 1:dim(z)[2], z, ...)
{
  image(.PICT$mlong(x), .PICT$mlat(y), z, ...)
  invisible()
}

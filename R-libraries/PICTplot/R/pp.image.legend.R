#' Add an image legend to a PICT plot
#'
#' @description See R graphics function image for details of parameters that are passed to image by ...
#'
#' @export
#'
"pp.image.legend"<-
function(zmat, x, y, size = c(0.25, 2), horizontal = F, nint = 50, plus = F, lab, ...)
{
  old.par <- par()
  par(xaxs = "i", yaxs = "i")
  if(missing(zmat))
    stop("missing argument zmat\n")
  if(is.list(zmat))
    zmat <- zmat$z
  if(missing(x)) {
    if(missing(size)) {
      cat("Using function \"pp.locator(2)\" to place opposite corners of image.legend\n")
      x <- pp.locator(2)
    }
    else {
      cat("Using function \"pp.locator(1)\" to place upper left corner of image.legend\n")
      x <- pp.locator(1)
    }
  }
  else {
    mlong <- .PICT$mlong
    mlat  <- .PICT$mlat
  }
  irgz <- seq(min(zmat, na.rm = T), max(zmat, na.rm = T), length = nint)
  lirgz <- length(irgz)
  if(missing(lab)) lab <- par()$lab
  if(plus) {
    if(horizontal) {
      sub.par <- subplot(x = mlong(x), y = mlat(y), size = rev(size), fun = image(x = irgz, y = 1:lirgz, z= matrix(irgz, lirgz, lirgz), axes = F, ...), hadj = 0, vadj = 1)
      par(sub.par)
      xat <- seq(sub.par$xaxp[1], sub.par$xaxp[2], length = labs[1])
      xlabs <- format(xat)
      xlabs[length(xlabs)] <- paste(xlabs[length(xlabs)], "+",sep = "")
      print(xlabs)
      axis(side = 1, at = xat, labels = xlabs, ...)
      axis(side = 2, labels = F)
      box()
    }
    else {
      sub.par <- subplot(x = mlong(x), y = mlat(y), size = size, fun = image(x = 1:lirgz, y = irgz, z = matrix(irgz, lirgz, lirgz, byrow = T), axes = F, ...), hadj = 0, vadj = 1)
      par(sub.par)
      yat <- seq(sub.par$yaxp[1], sub.par$yaxp[2], length = lab[2])
      ylabs <- format(yat)
      ylabs[length(ylabs)] <- paste(ylabs[length(ylabs)], "+", sep = "")
      axis(side = 2, at = yat, labels = ylabs, adj = 1, ...)
      axis(side = 1, labels = F, ticks = F)
      box()
    }
  }
  else {
    if(horizontal)
      sub.par <- subplot(x = mlong(x), y = mlat(y), size = rev(size), fun = image(x = irgz, y = 1:lirgz, z = matrix(irgz, lirgz, lirgz), yaxt = "n", ...), hadj = 0, vadj = 1)
    else sub.par <- subplot(x = mlong(x), y = mlat(y), size = size, fun = image(x = 1:lirgz, y = irgz, z = matrix(irgz, lirgz, lirgz, byrow = T), xaxt = "n", ...), hadj = 0, vadj = 1)
  }
  par(old.par)
  invisible()
}

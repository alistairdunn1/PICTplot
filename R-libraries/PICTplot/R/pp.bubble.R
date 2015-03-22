# TODO
#' Add a bubble plot to a PICT plot
#
## Description
#   Function to draw and/or fill single or multiple circles on an PICTPlot map.
#   The circles drawn appear as exact circles in the projection used but are
#   not generally circles on the earth's surface. If multiple circles are drawn
#   they will all have the same fill, lines, and border colours. Default
#   settings draw the circles unfilled. Function can be used to draw draw
#   "bubble" plots or a single circle.
#
## Arguments
#   x, y        vectors for the locations of the centres of the bubbles.
#               Alternatively, x can be a matrix, a list or a data frame with
#               names that include either {"long", "lat"}, or {"x", "y"}.
#               Otherwise the first two columns or components will assumed to
#               be x and y respectively. If x and y are vectors, they are the
#               longs and lats of the centres.
#   radius      a vector of circle radii which are interpreted as the relative
#               sizes of the radii. If scale is NULL then the radii are
#               interpreted as fractions of the x-axis length. If scale is
#               specified then the radii are interpreted as relative sizes and
#               need not be normalised.
#   scale       scales the circles so that the circle of largest radius is the
#               proportion of the length of the x axis specified by scale.
#               Default value is NULL, which means radius values are
#               interprested as fractions of the x-axis.
#   plot.centre logical determining if a point is to be plotted at the centre
#               of each circle.
#   resolution  an integer specifying the numbers of points used to draw the
#               circle. Default is 360, which corresponds to a point every
#               degree around the circle.
#   border      colour of circle border and centre point. The default is to use
#               par("fg"). Use border = NA to omit borders. (See the R function
#               polygon for more information about border, col, density, and
#               angle)
#   col         solid fill colour for circle. If col = NA circle is not filled.
#               If density is positive then col sets the colour of the stripe
#               lines.
#   density     density of stripe lines for filling the circle. Use density =
#               NULL to omit stripes but allow solid fill.
#   angle       angle of fill stripe lines
#   ...         other plotting parameters to pass to the polygon and points
#               functions, e.g. lty for ploygon border, pch and cex for circle
#               centre points.
#
## Updates
#    8 Mar 2010 Added centres if required and allowed separate scaling for radii
#   17 Mar 2010 Reordered the arguments, fixed typos and tidied wording in the
#               comments above.
#    2 Feb 2011 Small wording change to x, y argument
#'
#' @export
#'
"pp.bubble"<-
function(x, y = NULL, radius, scale = NULL, plot.centre = FALSE, resolution = 360,
  border = par("fg"), col = NA, density = NULL, angle = 45, ...)
{
  # get centre coords from x (and y) depending on class of x
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
  asp <- .PICT$asp
  tc <- rev((0:resolution)/resolution * 2 * pi)
  circle <- data.frame(x = cos(tc + pi/2), y = asp[1]/asp[2] * sin(tc + pi/2))
  mlong <- .PICT$mlong
  mlat  <- .PICT$mlat
  if(is.null(scale))
    size <- diff(mlong(.PICT$xlim))*radius
  else
    size <- diff(mlong(.PICT$xlim))* scale *radius/max(radius)
  pts <- data.frame(x = mlong(long), y = mlat(lat), r = size)
  #  set default plot parameters for centre of circle if not specified in ... .
  if(plot.centre) {
    ne <- names(list(...))
    if(!"pch" %in% ne)
      pch = 20
    if(!"cex" %in% ne)
      cex = 0.3
  }
  invisible(apply(pts,1, function(x) {
    polygon(x[3]*circle$x + x[1], x[3]*circle$y + x[2], density = density,
      angle = angle, border = border, col = col, ...)
    if(plot.centre)
      points(x[1],x[2], col = border, pch = pch, cex = cex)
  }))
}

#' Add a pie chart to a PICT plot
#'
#' @description Draws pie charts at specified locations on a map drawn by PICTplot using either the Mercator or rectangular projection.
#' @param x, y  numeric vectors of coordinates where the text labels should be written. If the length of x and y differs, the shorter one is recycled.
#' @usage pp.pie(x, y = NULL, z, size = 0.2, resolution = 360, start.angle = 1, col = rainbow(length(z)), border = T, ...)
#' @param x, y x, y give the location of the centre of the pie charts. x is either a scalar giving the longitude of the centre or a vector of length 2 giving the long and lat (in that order) of the centre
#' @param z a vector of relative sizes of each slice (unscaled)
#' @param size diameter of the pie as a proportion of the length of the x axis
#' @param resolution  an integer specifying the numbers of points used to draw the circle. Default is 360, which corresponds to a point every degree around the circle.
#' @details Adds a pie graph onto a pp() plot
#' @return Returns no value
#' @export
#'
"pp.pie"<-
function(x, y, z, size = 0.2, resolution = 360, start.angle = 1, col = rainbow(length(z)), border = T, ...)
{
  if(length(x) == 2) {
    x <- x[1]
    y <- x[2]
  }
  x <- .PICT$mlong(x)
  y <- .PICT$mlat(y)
  asp <- .PICT$asp
  asp <- c(1, asp[2]/asp[1])
  size <- size/2 * diff(.PICT$mlong(.PICT$xlim))/asp
  # unit circle
  tc <- rev((0:resolution)/resolution*2*pi) + pi/2 - start.angle/180 * pi
  circle <- data.frame(x = size[1]*cos(tc), y = size[2]*sin(tc))
  # scale z to add to 1
  sz <- sum(z)
  if(sz > 0) {  # only draw pie if sz > 0
      z <- z/sz
    if(any(z == 1)) {
      new.circle <- circle
      col <- col[z==1]
    } else {
      z <- c(0, round(cumsum(z)*resolution, digits = 0))
      new.circle <- data.frame(x = 0, y = 0)
      for(i in 2:length(z))
        new.circle <- rbind(new.circle,circle[z[i-1]:z[i], ],c(0, 0),c(NA, NA))
    }
    polygon(new.circle$x + x, new.circle$y + y, border = border, col = col, ...)
  }
  invisible()
}

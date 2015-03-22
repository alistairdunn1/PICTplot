#' Add constant distance circle to a PICT plot
#'
#' @description Draws circles with specified great circle radii (in nautical miles) around specified centres
#
#' @param distance The circle radius in nautical miles (multiply by 1.852 to get kilometers)
#' @param long, lat Location of the circle centre in decimal degrees
#' @param fillcol if fillcol != 0, then then circle will be filled with colour fill.col
#' @param plot if plot = FALSE, the function will return the point co-ordinates, and not plot
#'
#' @export
#'
"pp.circle"<-
function(distance, long, lat, plot = TRUE, fill.col = 0, fill = !fill.col %in% 0, ...)
{
  CalcLatLong <- function(long, lat, d, tc) {
  # calcluates the lat and long (in decimal degrees)
  # for a direction tc of distance d
    mod <- function(y, x) {
      if(y >= 0) ANS <- y - x * floor(y/x)
      else ANS <- y + x * (floor( - y/x) + 1)
      return(ANS)
    }
    d <- (pi/(180 * 60)) * d
    lon1 <- (long * pi)/180
    lat1 <- ifelse(lat > 180, 360 - lat,  - lat)
    lat1 <- (lat1 * pi)/180
    lat <- asin(sin(lat1) * cos(d) + cos(lat1) * sin(d) * cos(tc))
    if(cos(lat) == 0) lon <- lon1
    else lon <- mod(lon1 - asin((sin(tc) * sin(d))/cos(lat)) + pi, 2 * pi) - pi
    # endpoint a pole
    lat <- (180/pi) * lat
    lon <- (180/pi) * lon
    if(lon < 0) lon <- lon + 360
    return(unlist(c(long = lon, lat =  - lat)))
  }
  NROW <- 73
  ANS <- matrix(NA, nrow = NROW, ncol = 2)
  # if missing long, lat, call locator to find point
  if(missing(long) | missing(lat)) {
    cat("Using function \"locator(1)\" to locate circle centre\n")
    x <- pp.locator(1)
    long <- x$x
    lat <- x$y
    print(unlist(c(long, lat)))
  }
  for(i in 1:NROW) {
    tc <- (i * 5)/180 * pi
    RES <- CalcLatLong(long, lat, distance, tc)
    ANS[i,  ] <- c(RES[1], RES[2])
  }
  ANS <- data.frame(x = ANS[, 1], y = ANS[, 2])
  if(plot) {
    pp.lines(ANS$x, ANS$y, ...)
    if(fill)
      pp.polygon(ANS$x, ANS$y, col = fill.col, ...)
    invisible()
  }
  else return(ANS)
}

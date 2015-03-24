#' Add a scale to a PICT plot
#'
#' @description Add a scale to a PICT plot
#' @param x, y  coordinates of where the scale is located
#' @param at Scale measurements
#' @param legend Scale legend label
#' @details Adds a scale to a pp() plot
#' @return Returns no value
#' @export
#'
"pp.scale"<-
function(x, y=NULL, at = c(50, 100, 200), legend = "Scale (nm)", scale = 1, angle = 90, tick, cex, ...)
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

# Use scale=1/1.852 and legend="scale (km)" for kilometers
# if missing long, lat, call locator to find point
  if(missing(long) | missing(lat)) {
    cat("Using function \"pp.locator(1)\" to place point\n")
    x <- pp.locator(1)
    long <- x$x
    lat <- x$y
    print(unlist(c(long, lat)))
  }
  mlong <- .PICT$mlong
  mlat  <- .PICT$mlat
  CalcLatLong <- function(long, lat, d, tc) {
  # calcluates the lat and long (in decimal degrees)
  # for a direction tc of distance d
    mod <- function(y, x) {
      if(y >= 0)
        ANS <- y - x * floor(y/x)
      else ANS <- y + x * (floor( - y/x) + 1)
      return(ANS)
    }
    d <- (pi/(180 * 60)) * d
    lon1 <- (long * pi)/180
    lat1 <- ifelse(lat > 180, 360 - lat,  - lat)
    lat1 <- (lat1 * pi)/180
    lat <- asin(sin(lat1) * cos(d) + cos(lat1) * sin(d) * cos(tc))
    if(cos(lat) == 0) lon <- lon1 else lon <- mod(lon1 - asin((sin(tc) * sin(d))/cos(lat)) + pi, 2 * pi) - pi
    # endpoint a pole
    lat <- (180/pi) * lat
    lon <- (180/pi) * lon
    if(lon < 0)
      lon <- lon + 360
    return(unlist(c(long = lon, lat =  - lat)))
  }
  at <-  - sort(unique(c(0, at)))
  longlat.at <- matrix(NA, ncol = 2, nrow = length(at))
  tc <- angle/180 * pi
  for(i in 1:length(at))
    longlat.at[i, 1:2] <- unlist(CalcLatLong(long, lat, d = at[i] * scale, tc = tc))
  x <- mlong(longlat.at[, 1])
  y <- mean(mlat(longlat.at[, 2]))
  segments(min(x), y, max(x), y, ...)
  if(missing(tick))
    tick <- diff(par()$yaxp[1:2]) * 0.01
  if(missing(cex))
    cex <- .PICT$cex * 0.5
  text(mean(c(min(x), max(x))), y + 1.2 * tick, legend, cex = cex)
  for(i in 1:length(x)) {
    segments(x[i], y - tick, x[i], y, ...)
    text(x[i], y - 1.8 * tick, format(abs(at))[i], cex = cex, adj = 0.5)
  }
  invisible()
}

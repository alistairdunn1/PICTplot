# Utility function
#
"pp.select"<- function(xlim, ylim)
{
  out <- list(lat = 0, long = 0)
  out$lat <- NA
  out$long <- NA
  for(i in floor(xlim[1]):ceiling(xlim[2])) {
    for(j in floor(ylim[1]):ceiling(ylim[2])) {
      iii <- pp.select2(i, j)
      if(!is.null(iii)) {
        out$lat <- c(out$lat, iii$lat, NA)
        out$long <- c(out$long, iii$long, NA)
      }
    }
  }
  out
}

#' Utility function
#' @description inverse translation function for the mercator projection
#'
"pp.merc.lat2"<-
function(lat)
return((atan(exp(lat)) - pi/4) * (360/pi))

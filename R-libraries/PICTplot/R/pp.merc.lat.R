# Utility function
#
# translation function for the mercator projection
#
"pp.merc.lat"<-
function(lat)
return(log(tan(pi/4 + (pi * lat)/360)))

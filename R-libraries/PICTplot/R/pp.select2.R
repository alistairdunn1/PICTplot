# Utility function
#
"pp.select2"<-
function(x, y)
{
  yy <- abs(floor(y))
  if(x <= 180) {
    xx <- floor(x - 100)
    ss <- paste("s", yy, "e", xx, sep = "")
  }
  else {
    xx <- 360 - floor(x) - 100
    ss <- paste("s", yy, "w", xx, sep = "")
  }
  if(exists(ss))
    get(ss)
  else NULL
}

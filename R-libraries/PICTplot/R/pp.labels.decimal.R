# Utility function
#
# degree label label function
#
"pp.labels.decimal"<-
function(side, range, cex, lty, mgp, tcl, minor.tcl, label.text, hadj, padj, ...)
{

  ## Function to add labels that include degree symbol and compass point
  pp.EWNS <- function(posn, degree) {
    posn <- ((posn + 180) %% 360) - 180
    if(posn %in% c(0, 180)) suffix <- c("", "")
    if(posn < 0) suffix <- c("S", "W")
    if(posn > 0) suffix <- c("N", "E")
    return(paste(abs(posn), degree, suffix[side %% 2 + 1], sep = ""))
  }
  # Projection function
  #  side is x or y
  xory <- (side - 1) %% 2 + 1     # 1 = x (long), 2 = y (lat)
  remap <- .PICT[[c("mlong", "mlat")[xory]]]

  if(.Device != "postscript")
    degree <- c("\260")
  else
    degree <- c("\312")

  #  Ensure first value is less than the second value
  range <- sort(range)
  lim <- c(floor(range[1]), ceiling(range[2]))
  majtic <- pretty(lim) #majlab<-paste(format(abs(majtic)),degree,sep="")
  majlab <- paste(abs(((majtic + 180) %% 360) - 180), degree, sep = "")
  deg180 <- match(180, majtic)
  a1 <- (1:length(majtic))[majtic > range[1]][1]
  a2 <- (1:length(majtic))[majtic < range[2]][1]
  if(!is.na(deg180)) {
    if(a1 < deg180)
      majlab[a1] <- pp.EWNS(majtic[a1], degree)
    if(a2 > deg180)
      majlab[a2] <- pp.EWNS(majtic[a2], degree)
  }
  else {
    majlab[a1] <- pp.EWNS(majtic[a1], degree)
  }
  majlab[majtic == range[1]] <- ""
  majlab[majtic == range[2]] <- ""

  if(!label.text)
    majlab <- FALSE
#    majlab <- rep("", length(majlab))
  axis(side = side, labels = majlab, tcl = tcl, at = remap(majtic),
    cex.axis = cex, mgp = mgp, lty = lty, hadj = hadj, padj = padj, ...)
  axis(side = side, labels = FALSE, tcl = minor.tcl, at = remap(sort(
    unique(c(majtic + mean(diff(majtic))/2, majtic - mean(diff(majtic))/2,
    majtic)))), lty = lty)
  # get majtics for passing to caller of pp.labels.degree
  majtic <- majtic[majtic >= range[1] & majtic <= range[2]]
  invisible(majtic)
}

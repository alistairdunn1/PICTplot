# Utility function
#
# degree label label function
#
"pp.labels.degree"<-
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
  # x or y axis is being labelled?
  xory <- (side - 1) %% 2 + 1
  remap <- .PICT[[c("mlong", "mlat")[xory]]]

#  if(.Device != "postscript")
  degree <- c("\260")
#  else
#    degree <- c("\312")
#  if(.Device == "x11") degree <- c(" ")
  #  Ensure first value is less than the second value in range
  range <- sort(range)

  # lim are the limits of whole degrees spanning the range (xlim or ylim)
  lim <- c(floor(range[1]), ceiling(range[2]))

  ## Build up axis tick labels
  #  span determines spacing of major (and if span <= 2 of minor tics)
  span <- range[2] - range[1]
  markby <- 0
#  if(span > 50)    MHS 5-Aug-09, put between 3 and 6 major ticks
#    ticstep <- 30
  if(span >= 180)
    ticstep <- 60
  else if(span >= 110)
    ticstep <- 30
  else if(span >= 60)
    ticstep <- 20
  else if(span >= 40)
    ticstep <- 10
  else if(span >= 10)
    ticstep <- 5
  else if(span >= 5)
    ticstep <- 2
  else if(span > 2) {
    ticstep <- 1
    markby  <- 1/2
    min.ticstep <- 1/2
    labrev <- c("", "30'")
    labels <- c("", "30'")
  }
  else if(span * 2 > 1) {
    ticstep <- 1
    markby  <- 1/4
    if(span > 1)
      min.ticstep <- 1/4
    else
      min.ticstep <- 1/12
    labrev <- c("", "45'", "30'", "15'")
    labels <- c("", "15'", "30'", "45'")
  }
  else {
    ticstep <- 1
    markby  <- 1/12
    min.ticstep <- 1/12
    labrev <- c("", "55'", "50'", "45'", "40'", "35'", "30'", "25'", "20'",
        "15'", "10'", "5'")
    labels <- c("", "5'", "10'", "15'", "20'", "25'", "30'", "35'", "40'",
        "45'", "50'", "55'")
    if (span * 4 <= 1)
      min.ticstep <- 1/24
    if (span * 10 <= 1)
      min.ticstep <- 1/60
  }
  #BB additions to two minute symbols.
  #MHS changed to 2.5 minute minor ticks, unlabelled if not a multiple of 5'

  majtic <- seq(round(lim[1]/ticstep - 1) * ticstep,
    round(lim[2]/ticstep + 1) * ticstep, by = ticstep)
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
  # no major tick labels if label.text = FALSE
  if(!label.text)
    majlab <- FALSE
  axis(side = side, labels = majlab, tcl = tcl, at = remap(majtic),
    cex.axis = cex, mgp = mgp, lty = lty, hadj = hadj, padj = padj, ...)

  ## Check if a whole number of degrees will be written, which happens if and
  #   only if diff(lim) > 1 (whole degrees on axis limits are not written).
  if(diff(lim) == 1) {
    # because no degrees value would be plotted, place one manually at deg+0.5
    # if that is still outside the range place at deg+0.25 or deg+0.75
    aa <- lim[1] + 0.5
    bb <- 2
    if(aa <= range[1]) {
      aa <- lim[1] + 0.75
      bb <- 3
    } else if(aa >= range[2]) {
      aa <- lim[1] + 0.25
      bb <- 1
    }
    do.rev <- ((side == 1 | side == 3) & lim[1] >= 180) |
                ((side == 2 | side == 4) & lim[1] < 0)
    if(do.rev)
      lab <- pp.EWNS(lim[2], degree)
    else
      lab <- pp.EWNS(lim[1], degree)
    # set majtic when diff(lim) = 1 for returning
    majtic <- aa
    # add tick label
    if(label.text ) {
      nl <- nchar(lab)
      x <- c("15'", "30'", "45'")
      x <- matrix(c(x, rev(x)), 3,2)
      aa.min <- x[bb, 1 + do.rev]
      lab <- paste(substr(lab,1,nl - 1), aa.min, substr(lab, nl,nl), sep = "")
      labels[(length(labels))%/%4 * bb + 1] <- ""
      labrev[(length(labrev))%/%4 * bb + 1] <- ""

      axis(side, at = remap(aa), labels = lab, tcl = tcl, cex.axis = cex,
        mgp = mgp, lty = lty, hadj = hadj, padj = padj, ...)
    }
  }
  # assign majtic for passing to caller of pp.labels.degree
  majtic <- majtic[majtic >= range[1] & majtic <= range[2]]

# Build up labels for minor ticks but only if label.text = TRUE
  if(markby & label.text) {
    mintic <- seq(lim[1], lim[2], by = markby)
    submintic <- seq(lim[1], lim[2], by = min.ticstep)
    # reverse minor tick labels if required.
    # only long at this stage
    if(side %% 2 == 1) {    # longitudes
      if(lim[2] <= 180) {       # no labels reversed
        minlab <- c(rep(labels, lim[2] - lim[1]), "")
      } else {
        if(lim[1] < 180)        # reverse some labels
          minlab <- c(rep(labels, 180 - lim[1]), rep(labrev, lim[2] - 180), "")
        else                    # reverse all labels
          minlab <- c(rep(labrev, lim[2] - lim[1]), "")
      }
    } else {                        # latitudes
      if(lim[2] <= 0) {         # all labels reversed for southern lats
        minlab <- c(rep(labrev, lim[2] - lim[1]), "")
      } else {
        if(lim[1] < 0)          # reverse some labels
          minlab <- c(rep(labrev, 0 - lim[1]), rep(labels, lim[2] - 0), "")
        else                    # no labels reversed
          minlab <- c(rep(labels, lim[2] - lim[1]), "")
      }
    }
    # next 2 lines unnecessary??
    minlab[mintic == range[1]] <- ""
    minlab[mintic == range[2]] <- ""
    # add minor labels and minor ticks
    axis(side = side, at = remap(mintic), labels = minlab, tcl = minor.tcl,
      cex.axis = cex, mgp = mgp, lty = lty, hadj = hadj, padj = padj)  ##
    # add sub-minor ticks between labels
    axis(side = side, labels = FALSE, tcl = minor.tcl,
      at = remap(submintic), lty = lty, ...)
  } else
    axis(side = side, labels = FALSE, tcl = minor.tcl,
      at = remap(sort(unique(c(majtic + mean(diff(majtic))/2,
      majtic - mean(diff(majtic))/2, majtic)))), lty = lty)
  invisible(majtic)
}

# TODO
#' Add an additional axis to a PICT plot
#'
#' @export
#'
"pp.addaxis" <-
    function(side = 1, labels.type, cex, lty, mgp, tcl, minor.tcl, label.text = TRUE, hadj, padj, inside, ...)
{
  if(side == 1 || side == 3)
    lim <- .PICT$xlim
  else
    lim <- .PICT$ylim
  if(missing(cex))
    cex <- .PICT$cex
  if(missing(lty)) {
    lty <- .PICT$lty
    if(is.null(lty))
      lty = 1
  }
  if(missing(mgp))
    mgp <- .PICT$mgp
  if(missing(tcl))
    tcl <- .PICT$tcl
  if(missing(minor.tcl))
    minor.tcl <- .PICT$minor.tcl
  valid <- c("degree", "decimal")
  if(missing(labels.type))
    labels <- valid[pmatch(as.character(.PICT$labels), valid, nomatch = 1)]
  else
    labels <- valid[pmatch(as.character(labels.type), valid, nomatch = 1)]
  labfun <- get(paste("pp.labels.", labels, sep = ""))

  # determine latest setting for tick labels orientation
  las <- par("las")

  # set default hadj and padj depending on side and whether labels inside or not
  if(label.text) {
    # are tick labels inside or out?
    if(missing(inside))
      inside <- .PICT$inside
    # set up adj values for tick labels according to side, las, and inside
    if(side == 1 & las %in% 0:1 || (side == 4 & las %in% c(0, 3)))
      mgp[2] <- mgp[2] - 1
    if(missing(hadj)) {
      hadj <- NA
      if(las == 1 & inside) {
        if(side == 4)
          hadj <- 1
        if(side == 2)
          hadj <- 0
      }
      if(las == 2 & inside)
        if(side %in% 3:4)
          hadj  <- 1
        else
          hadj = 0
      if(las == 3) {
        if(inside) {
          if(side == 1)
            hadj <- 0
          if(side == 3)
            hadj <- 1
        }
        if(!inside & side == 1)
          hadj <- 1
      }
    }
    if(missing(padj)) {
      padj <- NA
      if(las == 0)
        if((!inside & side == 4) || (inside & side == 2))
          padj <- 1
      if(las %in% 0:1)
        if((!inside & side == 1) || (inside & side == 3))
          padj <- 1
      if(las %in% 2:3)
      if(las == 3)
        if((inside & side == 2) || (!inside & side == 4))
          padj <- 1
    }
  } else {
    hadj <- padj <- NA
  }
  # set mgp[1] to avoid warning if sign different from mgp[2]
  mgp[1] <- mgp[2]
  x <- labfun(side, lim, cex, lty, mgp, tcl, minor.tcl, label.text, hadj, padj, ...)
  invisible(x)
}

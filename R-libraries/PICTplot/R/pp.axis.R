# TODO
#' Add an axis to a PICT plot
#'
#' @export
#'
"pp.axis" <-
    function(side, at = NULL, labels = TRUE, ...)


## Description

#   Wrapper function for the R function axis to add an axis to a plot generated
#   by PICT, allowing full freedom to format an axis and tick mark labels.


## Arguments

#   side    integer specifying which side of the plot the axis is drawn on.

#   at      the points (in degrees) at which tickmarks are to be drawn.
#           If NULL (default) then the ticks will placed at 'prettied' degree
#           points and, if labels = TRUE, labels are attached to whole degrees.
#
#   labels  either a character or expression vector of labels to be placed at
#           the tickpoints, or a logical value specifying whether no labels are
#           written or whether whole degree annotations are to be made at the
#           appropriate tickmarks. If labels is not logical, at should also be
#           supplied and of the same length. If FALSE no labels are written.

#   ...     other arguments to be passed to the R graphics package function
#           axis including parameters passed by ... in the axis function.
#           E.g. tick to determine if tickmarks and the axis are both drawn,
#           tcl for tickmark length, col and tick.col for the colour of the
#           axis and tickmarks, mgp to position the axes and the labels.


## Value

#   A vector of numeric locations on the axis scale at which tick marks were
#   drawn. Side effect is to add an axis to the existing plot.


## Details

#   If at is NULL,

#   If labels = TRUE (default), only the ticks which are whole numbers of
#   degrees are labelled, in which case labels will include the degree symbol
#   and the initial label on the axis will also include the appropriate
#   hemisphere symbol (E, W, N, S).


## See
#   axis, pp.addaxis.

## Examples

#   pp.axis()


{

  ## projection coord function for side of axis
  #  side is x or y
  xory <- (side - 1) %% 2 + 1     # 1 = x (long), 2 = y (lat)
  #  projection functions
  remap  <- .PICT[[c("mlong", "mlat")[xory]]]
  invmap <- .PICT[[c("mlonginv", "mlatinv")[xory]]]
  xy <- c("x", "y")[xory]

  # If at = NULL, prepare values for at
  if(is.null(at)) {
    # sort axis limits
    range <- sort(.PICT[[paste(xy, "lim", sep = "")]])
    # pretty degree tick placements
    at <- pretty(range)
    # prepare labels if labels = TRUE
    }

  # If labels = TRUE, prepare values for labels
  if(is.logical(labels) && labels) {
    ## Function to add labels that include degree symbol and compass point
    #   when at = NULL and labels = TRUE
    pp.EWNS <- function(posn, degree) {
      posn <- ((posn + 180) %% 360) - 180
      if(posn %in% c(0, 180)) suffix <- c("", "")
      if(posn < 0) suffix <- c("W", "S")
      if(posn > 0) suffix <- c("E", "N")
      return(paste(abs(posn), degree, suffix[xory], sep = ""))
    }
    # degree symbol
    if(.Device != "postscript")
      degree <- c("\260")
    else
      degree <- c("\312")

    # get whole number ticks
    whole.no <- at%%1 == 0
    majtic <- at[whole.no]
    # if no whole numbers, no labels to draw
    if(length(majtic) == 0)
      labels = FALSE
    else {
      majlab <- paste(abs(((majtic + 180) %% 360) - 180), degree, sep = "")
      deg180 <- match(180, majtic)
      a1 <- (1:length(majtic))[majtic > range[1]][1]
      a2 <- (1:length(majtic))[majtic < range[2]][1]
      if(!is.na(deg180)) {
        if(a1 < deg180)
        majlab[a1] <- pp.EWNS(majtic[a1], degree)
      if(a2 > deg180)
        majlab[a2] <- pp.EWNS(majtic[a2], degree)
      } else {
        majlab[a1] <- pp.EWNS(majtic[a1], degree)
      }
      majlab[majtic <= range[1]] <- ""
      majlab[majtic >= range[2]] <- ""
      labels <- rep("", length(at))
      labels[whole.no] <- majlab
    }
  }

  ## Draw axis and save tick mark positions
  x <- axis(side, at = remap(at), labels = labels, ...)
  invisible(invmap(x))
}

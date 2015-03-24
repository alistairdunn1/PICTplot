#' PICTplot high level plotting function
#'
#' High level map plotting function for the lands and oceans in the PICTn Pacific region.
#' The plotting parameters, set in pp, are saved in the list .PICT in the
#' global environment, for use as required with some low level plotting
#' functions.
#'
#' @author Alistair Dunn
#' @param xlim vector containing the plot x limits in decimal degrees with longitudes in the range 0 to 360.
#' @param ylim vector containing the plot y limits in decimal degrees with longitudes in the range 0 to 360.
#' @param coastline logicals determining whether the island coastline are to be drawn and/or filled.
#' @param coastline logical determining whether coastline is plotted (or filled)
#' @param lty line type of the borders of the land features drawn.
#' @param lwd line width of the borders of the land features drawn.
#' @param col line colour of the borders of the land features drawn.
#' @param fill.col polygon drawing arguments. Use fill.col = 0 and density = NA to ensure that no polygon is drawn. If fill.col is not 0 then polygon is filled with fill.col and/or if density is numeric then angled lines (density gives number of lines per inch and angle gives their angle. See the R Graphics function polygon for parameter settings and their effects).
#' @param projection character string, either "merc" or "rect", (mercator or rectangular).
#' @param aspect character string, one of "ratio", "square", or "maximum". "ratio" preserves the ratio of the lengths of the x and y axes. "square" always plots in a square. "maximum" uses all the available plot area without fixing the NS to EW aspect ratio. Default is aspect = "ratio".
#' @param axes arguments determining whether both or neither axes are drawn.
#' @param xaxt If axes is FALSE then neither axis is drawn and no box is placed around the plot area. If axes = TRUE the axes are drawn on the bottom and left of the figure unless overruled by xaxt = "n" and/or yaxt = "n" (which enables only one axis to be drawn).
#' @param yaxt  If axes is FALSE then neither axis is drawn and no box is placed around the plot area. If axes = TRUE the axes are drawn on the bottom and left of the figure unless overruled by xaxt = "n" and/or yaxt = "n" (which enables only one axis to be drawn).
#' @param tcl determine the length of the major tick marks (typically whole degrees) and minor.tcl the length of the minor ticks marks (associated with numbers of minutes) and in the fraction of a mex unit. Negative values put ticks outside the axis and positive inside. If minor.tcl = 0 the minor ticks are not drawn. If tcl = 0, then no tick marks are drawn (no matter what minor.tcl is set to).
#' @param labels logical or a character string governing the printing of tick labels: one of "degree" , "decimal", "inside", TRUE, or FALSE. If "degree" or "decimal" then the tick labels are written in the named format outside the plot area. If "inside" tick labels are written inside the plot area. If FALSE (or "FALSE") then no labels are written. If TRUE, then labels are outside in degree tick format. The position of tick labels in relation to the axis is overruled by the second component of the mgp argument.
#' @param cex character expansion for tick labels.
#' @param mgp graphics parameter, a numeric vector of length 3, which sets, for each side, the number of lines from the plot edge into the margin that: the axis title is written, the tick labels are written, and the axis position is drawn (see par and axis functions help in the R Graphics package). If mgp is missing (default), PICT draws the axes and positions the labels according to internal defaults (axes are drawn on plot area border, and labels are placed on the line so there is a gap of 0.5*cex lines between the line and the ends of the major tick marks. Setting mgp will overrule the inside/outside setting for the labels argument depending on whether the second component of mgp is less than 0. Setting mgp also changes the line where the axes titles are drawn, (mgp[1]) and also the line position where the axes are drawn, (mgp[3]).
#' @param las graphics parameter that determines the orientation of the tick labels. Restricted in PICT to only 0 or 1. If not set, the default value is las = 1, labels are are horizontal for both axes.
#' @param xlab character arguments which give the axes titles, the main title,
#' @param xlab character arguments which give the axes titles, the main title,
#' @param main, sub and the sub-title.
#' @param box.lty line type, width, and colour for the box around the plot.
#' @param box.lwd box.lty = 0 supresses the drawing of the box
#' @param box.col Box colour
#' @param border Extent of border around outside of plot. default=0
#' @param background.col   Either missing or the background colour to shade the whole plot area. If missing, there is no shading (transparent).#' @param ... Additional plotting parameters passed to some plotting commands within .PICT (For further details see par().) Examples: cex.main, col.main, font.main for character expansion, colour, and font of the main title and cex.lab for character expansion for the xlab and ylab titles.
#' @param grid Add 5 degree grid lines (default is false). See also pp.grid()
#' @details After setting up plot area, then if required, PICT fills the plot area with background.col, fills land areas using pp.polygon and draws coastlines using pp.lines. It next draws the axes using pp.labels.degree of pp.labels.decimal and a box is drawn around the plot area. If required, a title, subtitle and axis titles are added and the size, colour, and font can be controlled by cex.main, cex.sub, cex.lab, col.main, etc. which are passed via the ... argument.  The characteristics of the plot are saved in the list object .PICT in the global environment for later use by low level plotting functions that add features to the exsting plot.

#' @return Returns no value but the side effect is to draw the plot area.
#' @export
#'
"pp"<-
function(xlim=c(140, 225), ylim = c(10,-30), coastline = TRUE, lty = 1, lwd = 1, col = 1, fill.col = "ivory3", density = NA, angle = 45, projection = "merc", aspect = "ratio", axes = TRUE, xaxt, yaxt, tcl = -0.3, minor.tcl = 0, labels = "degree", cex = 1, mgp, las, xlab = NULL, ylab = NULL, main = NULL, sub = NULL, box.lty = 1, box.lwd = 1, box.col = 1, background.col, border = 0, grid=F ,...)
{

  # print the version of PICTPlot
  PICTplot.version()

  # sort out the latitude and longitude if inrelation to negative numbers
  xlim <-  xlim %% 360
  if (any(abs(ylim) > 75))
    warning("Because only the mercator or rectangular projections are used, there will be extreme distortions at the higher latitudes.", call. = FALSE)

  # order limits W to E, and S to N
  xlim <- sort(xlim)
  ylim <- sort(ylim)

  # projection, mercator or rectangular. Final version has to be in the 4 character form.
  projection <- c("merc", "rect")[pmatch(as.character(projection), c("mercator", "rectangular"), nomatch = 1)]
  # projection functions
  pn <- paste("pp", projection, "long", sep = ".")
  mlong <- get(pn)
  # inverse projection fn
  mlonginv <- get(paste(pn, 2, sep = ""))
  pn <- paste("pp", projection, "lat", sep = ".")
  mlat  <- get(pn)
  # inverse projection fn
  mlatinv  <- get(paste(pn, 2, sep = ""))

  # specified limits in usr units
  xmlim <- mlong(xlim)
  ymlim <- mlat(ylim)
  range <- list(x = xmlim, y = ymlim)   ####

  # use aspect to set up aspect ratio properties of plot
  asp  <- c(1, 1)
  pty = "m"
  valid <- c("ratio", "square", "maximal")
  aspect <- valid[pmatch(as.character(aspect), valid, nomatch = 1)]
  if (aspect == "square")
    pty <- "s"
  if(projection == "rect")
    asp <- c(cos(mean(ylim)*pi/180), 1)

  # set up plot aspect parameter (asp) value, asp = parasp
  parasp <- ifelse(aspect == "maximal", NA, asp[2]/asp[1])

   ## Start filling .PICT, the globally stored plot details for use by low level plot functions.
  .PICT <- list()
   # save original par parameters before changes
  .PICT$old.par      <- par(no.readonly = TRUE)
  .PICT$projection   <- projection
  .PICT$mlong        <- mlong         # function for transforming long to usr
  .PICT$mlat         <- mlat          # function for transforming lat to usr
  .PICT$mlonginv     <- mlonginv      # function for transforming usr to long
  .PICT$mlatinv      <- mlatinv       # function for transforming usr to lat
  .PICT$aspect       <- aspect        # for subplot
  .PICT$asp          <- asp           # for subplot and pp.pie
  .PICT$xlim         <- xlim          # for axes
  .PICT$ylim         <- ylim          # for axes
  .PICT$cex          <- cex           # for axes
  .PICT$tcl          <- tcl           # for axes
  .PICT$minor.tcl    <- minor.tcl     # for axes
  .PICT$lty          <- lty           # for axes

  # Set plot boundaries to have correct aspect and coincide with xlim, ylim - code hacked from map in maps package keeps plot area in centre of figure area
  xl <- mlong(xlim)
  yl <- mlat(ylim)
  d <- c(diff(xl), diff(yl))*asp
  p <- par("fin") - as.vector(matrix(rep(c(0,1,1,0),2), nrow = 2)%*%par("mai"))
  par(pin = p)
  p <- par("pin")
  p <- d*min(p/d)
  par(pin = p)

  # Add margins to plot limits
  border <- border
  d <- d * border + (p/min(p/d) - d)/2/asp
  usr <- c(xl, yl) + rep(c(-1,1),2)*rep(d, c(2,2))
  par(usr = usr)

  # add usr to .PICT for subplot
  .PICT$usr <- usr

  # plot area without axes or content. Must be done after setting pin
  plot(NA, NA, xlim = xl,  ylim = yl, type = "n", axes = FALSE, ann = FALSE, asp = parasp, pty = pty, xaxs = "i", yaxs = "i")

  ## Fill plot area with background colour if required, draw box later
  if(!missing(background.col) && !is.na(background.col)) {
    U <- par("usr")
    rect(U[1], U[3], U[2], U[4], col = background.col, border = NA)
  }

  ## Plot coast using correct projection
  # save plot details set so far to .PICT in global environment for use with
  # low level plot functions
  assign(".PICT", .PICT, envir = globalenv())

  do.polygon <-
    !(is.numeric(density) & density == 0) & # either shade or fill
    ((is.numeric(density) & density > 0) |  # shade & ~ fill
    (!is.null(fill.col) & ! is.na(fill.col) & # not fill & not fill
    ((is.numeric(fill.col) & fill.col > 0) |
     is.character(fill.col))))  # fill

  if (coastline) {
    if (do.polygon) {
      pp.polygon(pp.coast, density = density,
        col = fill.col, angle = angle, border = NA, ...)
    }
    if (coastline)
      pp.lines(pp.coast, lty = lty, lwd = lwd, col = col, ...)
  }

  ## Draw box if axes = TRUE (in case a polygon has erased some of it).
  box(lty = ifelse(axes, box.lty, 0), lwd = box.lwd, col = box.col)

  ## Axes, ticks, and tick labels
  # Are axes to be drawn (accounting for xaxt & yaxt)
  # axes has been character in past versions
  if(is.character(axes) && axes %in% c("T", "TRUE"))
    axes <- TRUE
  if(is.character(axes) && !axes %in% c("T", "TRUE"))
    axes <- FALSE
  # check for individual sides to be plotted
  noside1 <- !missing(xaxt) && xaxt =="n"     # not plot x-axis
  noside2 <- !missing(yaxt) && yaxt =="n"     # not plot y-axis
  # overule axes = TRUE if both xaxt and yaxt = "n"
  axes <- axes && !(noside1 & noside2)

  # Tick marks
  # are tick marks to be drawn
  tick <- tcl != 0

  # Tick labels
  # tick labels inside or outside?
  inside <- (!missing(mgp) && mgp[2] < 0) || labels == "inside"
  # are tick labels included?
  label.text <- TRUE
  if(is.character(labels)) {
    if(labels %in% c("F", "FALSE"))
      # will now place tick marks but not label them
      label.text <- FALSE
  } else if(is.logical(labels)) {
      label.text <- labels
  }
  # unless labels = "decimal" ticks will be placed according to "degree"
  valid <- c("degree", "decimal")
  labels <- valid[pmatch(as.character(labels), valid, nomatch = 1)]

  # add labels and inside to .PICT
  .PICT$labels <- labels
  .PICT$inside <- inside

  # Set mgp if not set by an argument (adding 0.5*cex lines between ticks and
  #  labels). Also calculating line of placement of axis labels (in mgp[1])
  if(missing(mgp))
    if(inside) {
      m2 <- -(max(tcl, 0) + 0.5*cex)
      mgp <- c(max(0, -tcl) + 1*cex, m2, 0)
    } else {
      m2 <-  max(-tcl, 0) + 0.5*cex
      mgp <- c(m2 + 2*cex , m2, 0)
    }

  # save mgp to .PICT to be available if required by pp.addaxis
  .PICT$mgp <- mgp

  # Set axes orientation using plotting parameter las. Only 0 or 1 permitted
  #  if las missing of > 1 set defaults
  if(missing(las))
    las <- 1
  if(!missing(las) && !las %in% 0:1) {
    las <- 1
    warning("las can only be 0 or 1. Setting las to 1 ", call. = FALSE, immediate. = TRUE)
  }
  # las must set after plot is done or it messes up the size of the plot area
  #  Can't be set inside a low level plotting function.
  par(las = las)

  # save to .PICT
  .PICT$las <- las

  # no need to draw axes if no ticks and no labels
  # axes <- axes && !(!tick && label.text)

 ## Save full set of plot details to the list .PICT in the global environment
  assign(".PICT", .PICT, envir = globalenv())

  # select function for drawing axes
  labfun <- get(paste("pp.labels.", labels, sep = ""))

  # Draw axis 1,  if required
  # set adj's to default
  hadj <- padj <- NA
  # saving tick positions to .PICT$xaxis.at
  if(!noside1 & axes) {
    # for axis 1, set padj
    if (!inside)
      padj <- 1   # justified
    #  subtract 1 from mgp[2] because of changed padj (justified downwards)
    #  for x axis labels R for adds 1 to mgp[2] and justifies upwards
    mgpa <- mgp
    mgpa[2] <- mgp[2] - 1
    # match signs of mgp1[1] to that of mgp1[2] (to avoid a warning)
    mgpa[1] <- mgpa[2]
    .PICT$xaxis.at <- labfun(1, xlim, cex, lty = 1, mgp = mgpa, tcl, minor.tcl, label.text, hadj, padj)
  } else
    .PICT$xaxis.at <- labfun(1, xlim, cex, lty = 0, mgp = mgp, tcl, minor.tcl, label.text = FALSE, hadj, padj)

  # Draw axis 2, if required
  # set adj's to default
  hadj <- padj <- NA
  # saving tick positions to .PICT$yaxis.at
  if(!noside2 & axes) {
    # for axis 2, set hadj if inside.
    if (inside)
      if(las == 1)
        hadj <- 0
      else
        padj <- 1
    # match signs of mgp1[1] to that of mgp1[2] (to avoid a warning)
    mgpa <- mgp
    mgpa[1] <- mgpa[2]
    .PICT$yaxis.at <- labfun(2, ylim, cex, lty = 1, mgp = mgpa, tcl, minor.tcl, label.text, hadj, padj)
  } else
    .PICT$yaxis.at <- labfun(2, ylim, cex, lty = 0, mgp = mgp, tcl, minor.tcl, label.text = FALSE, hadj, padj)

  # save plot details to global environment in the list .PICT
  assign(".PICT", .PICT, envir = globalenv())

 ## Add title or subtitle if required
  title(main = main, sub = sub, ...)

 ## Add axis names if required
  #using cex.lab if set otherwise cex.lab = 1
  if(!is.null(xlab) || !is.null(ylab)) {
    aa <- list(...)
    cl <- ifelse("cex.lab" %in% names(aa), aa$cex.lab, 1)
    title(xlab = xlab, line = mgp[1] + cl - 1.1, ...)
    # allow for labels perpendicular to axis one with a °S
    title(ylab = ylab, line = mgp[1] + 0.5*(las == 1 & !inside), ...)
  }

  # Add grid
  if(grid)
    pp.grid()

  invisible()
}

#' Add a subplot to a PICT plot
#'
"subplot"<-
  function(fun, x, y, size = c(1, 1), background.col = NA, pars)

## Description

#   Function to add a subplot to a map or plot created by another function.
#   Note that this function can be used to add subplots to any plot not just
#   one created by PICTPlot.


## Arguments

#   fun     a function to draw the subplot in the subplot area. The function
#           needs to specify the expansion factor and other parameters for the
#           subplot as well as giving the high level plotting command.

#   x, y    location, in usr coordinates, of an opposite pair corners of the
#           defining the plot area of the subplot. Any margins are external to
#           this area. If x, y are missing the function requests the pair of
#           points be marked using the locator(2) function. If size is
#           specified then x, y must specify a single point, which is the
#           BOTTOM LEFT corner of the plot area.

#   size    vector of length 2 giving the size of the edges of the plot area
#           for the subplot expressed as fractions of the width and height of
#           the parent plot. If size is not specified, then opposite corners
#           of the plot area need to be specified, either through the x, y
#           arguments or using the locator(2) function.

#   background.col   If NULL or NA, the plot area of the subplot is transparent
#           Otherwise, the background shading is the specified colour.

#   pars    a list giving loaction and other parameters to specify the position
#           and size of the subplot. If pars is specified x, y and size are
#           not required and any specification ignored.


## Examples

# PICT()
# inset <- function() {
#   par(mex = 0.8, cex = 0.8, mgp = c(2,0.8,0),tcl = -0.3) # set axes parameters
#   x <- seq(0,pi,length.out=201)
#   plot(x,sin(x),type="l")
# }
# subplot(inset)          # program will pause when locator function is invoked
# subplot(inset, background.col = "white") # plot area has a white, rather than
                                           # a transparent background


## Updates

#   23 Mar 2011   Fixed bugs so function now works. Added in the capability of
#                 shading the background of the subplot plot area. (It would be
#                 nice to have the option to shade the whole figure are, not
#                 just the plot area. This can't be done, currently, because
#                 the subplot margins are set so as to fill the whole out the
#                 original figure area.)

{

  # default values
  hadj <- 0.5
  vadj <- 0.5

  if(missing(fun))
    stop("missing argument \"fun\"")
  if(missing(pars)) {
# set graphical parameters
    opars <- par(no.readonly = TRUE)  # save old parameters
    par(err = -1)
    mfin <- par()$fin  # dimensions of figure, inches
#
#     mai doesn't deal with pty='s', etc
#     mai <- par()$mai  # bottom, left, top, right margins, in inches
    plti <- par("plt") * mfin[c(1, 1, 2, 2)]
    mai <- c(plti[3], plti[1], mfin[2] - plti[4], mfin[1] - plti[2]) #
#
    usr <- par()$usr
    # limits in user units : xmin,xmax,ymin,ymax
    # fix uin for R
#    uin <- par()$uin  # inches per user units , x then y.
    uin <- par()$pin/(usr[c(2,4)] - usr[c(1,3)])  # in. per user units, x then y

    if(missing(x)) {
      if(missing(size)) {
        cat("Using function \"locator(2)\" to place opposite corners of subplot area\n")
        x <- locator(2)
      } else {
        cat("Using function \"locator(1)\" to place lower left corner of subplot area\n")
        x <- locator(1)
      }
    }
    if(is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if(length(x) == 2 && length(y) == 2) {
    # then x,y represent corners of plot
      # reparameterize to lower left corner, size
      x <- sort(x)
      y <- sort(y)
      size[1] <- (x[2] - x[1]) * uin[1]
      size[2] <- (y[2] - y[1]) * uin[2]
      x <- x[1]
      y <- y[1]
      hadj <- 0
      vadj <- 0
    }
    if(length(x) != 1 || length(y) != 1) {
      stop("length of x and y must both be same: 1 or 2")
    }
  # convert x, y to inches from edges of plot, xi and yi
    xi <- mai[2] + (x - usr[1]) * uin[1]
    yi <- mai[1] + (y - usr[3]) * uin[2]
    hoff <- size[1] * hadj
    voff <- size[2] * vadj
    newmai <- c(yi - voff, xi - hoff, mfin[2] - yi - size[2] +
      voff, mfin[1] - xi - size[1] + hoff)
    newmex <- sqrt(max(size)/min(mfin))
    if(any(newmai < 0))
      stop("subplot out of bounds")
    par(pty = "m", mex = newmex, mai = newmai)
  } else
    opars <- par(pars) # set graphical parameters as specified in pars

  opars$new <- F

  # add in background colour if required
  if(!is.null(background.col) && !is.na(background.col)) {
    par(new = T)  # don't erase current plot
    image(1,1,matrix(1,1,1),col = background.col,axes=FALSE,ann=FALSE)
  }

  par(new = T)  # don't erase current plot
  # now actually do the subplot defined by fun
  eval(fun(), sys.parent(1))
  eval(fun(), sys.parent(1))
  invisible(par(opars))
}

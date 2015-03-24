#' Add axis grid lines to a PICT plot
#'
#' @description Adds grid lines to a PICT plot
#' @param at the frequency (in degrees) at which gridlines will be drawn. Defaults to 5 degrees
#' @param ...  other arguments to be passed to the R graphics package function lines
#' @details  Grid lines are added to the plot at the specified interval
#' @return Returns no value
#' @export
#'
"pp.grid" <-
    function(at = 5, col="gray", lty=1, ...)
{
  x<-seq(.PICT$xlim[1], .PICT$xlim[2], by=at)
  x<-x[x!=.PICT$xlim[1]]
  x<-x[x!=.PICT$xlim[2]]
  if(length(x)==0)
    invisible()

  y<-seq(.PICT$ylim[1], .PICT$ylim[2], by=at)
  y<-y[y!=.PICT$ylim[1]]
  y<-y[y!=.PICT$ylim[2]]
  if(length(y)==0)
    invisible()

  pp.axis(side=1,at=x,tck=1,labels=FALSE,col=col,lty=lty,...)
  pp.axis(side=2,at=y,tck=1,labels=FALSE,col=col,lty=lty,...)

  box()

  invisible()
}


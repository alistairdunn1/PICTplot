#' Add eez lines to a PICT plot
#'
#' @description Adds eez lines to a PICT plot
#' @param country Selects the counties for which to plot EEZ lines. NOT YET IMPLEMETED. Defaults to all countries.
#' @param ...  other arguments to be passed to the R graphics package function lines
#' @details  EEZ lines are added to the plot
#' @return Returns no value
#' @export
#'
"pp.eez"<-
function(country=NULL, lty = 1, lwd = 1, col = 1, ...)
{
  # TO DO: select country on the bases of country, else plot all EEZ lines
  pp.lines(pp.eez.lines, lty = lty, lwd = lwd, col = col, ...)
  invisible()
}

#' Add eez lines to a PICT plot
#'
#' @export
#'
"pp.eez"<-
function(country=NULL, lty = 1, lwd = 1, col = 1, ...)
{
  # to do: select country on the bases of counrty, else plot all EEZ lines
  pp.lines(pp.eez.lines, lty = lty, lwd = lwd, col = col, ...)
  invisible()
}

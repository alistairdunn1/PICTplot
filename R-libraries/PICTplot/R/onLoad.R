#' On load hook
#'
#' This is a load hook that is called by R when the package is loaded. This should not be exported
#'
.onLoad <- function(libname, pkgname)
{
  cat("\n")
  cat("PICTplot library ")
  cat(PICTplot.version())
  cat("\n")
}



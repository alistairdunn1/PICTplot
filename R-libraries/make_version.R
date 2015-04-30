# Get Version
VERSION <- paste("1.0","-", format(Sys.Date(),format(Sys.time(),"%d-%m-%Y")), "",sep="")

# Build DESCRIPTION file
filename<-"PICTplot/DESCRIPTION"
cat("Package: PICTplot\nTitle: Pacific Island Countries and Territories coastlines, boundaries, and bathymetry\nVersion: ",file=filename)
cat(VERSION,file=filename,append=TRUE)
cat("\nDate: ",file=filename,append=TRUE)
cat(Sys.Date(),file=filename,append=TRUE)
cat("\n",file=filename,append=TRUE)
cat("Author: Alistair Dunn, Laura Boyer\n",file=filename,append=TRUE)
cat("Description: Data and functions for plotting Pacific Island Countries and Territories coastlines, boundaries, and bathymetry.\n",file=filename,append=TRUE)
cat("Maintainer: Alistair Dunn <a.dunn@niwa.cri.nz>\n",file=filename,append=TRUE)
cat("Depends: R (>= 3.0.0)\n",file=filename,append=TRUE)
cat("License: GPL-3.\n",file=filename,append=TRUE)
cat("URL: http://www.niwa.co.nz, http://www.spc.int\n",file=filename,append=TRUE)
cat("Copyright: National Institute of Water & Atmospheric Research (NIWA), Secretariat of the Pacific Community (SPC).\n",file=filename,append=TRUE)
cat("LazyData: true\n",file=filename,append=TRUE)

# Create R function to return version number
filename<-"PICTplot/R/PICTplot.version.R"
cat("\"PICTplot.version\"<-\n",file=filename)
cat("function() {\n",file=filename,append=T)
cat(paste("return(\"Version v",VERSION,"\")\n",sep=""),file=filename,append=T)
cat("}\n",file=filename,append=T)

# Write a *.html file to report version number for the Wiki
cat(paste("Version v",VERSION,sep=""),file="PICTplot.html")

# Exit
q()


rem #===============================================================
rem # Alistair Dunn Mar 2015
rem #===============================================================

rem # Initialise, roxygenize, and make version
rem #===============================================================

R --vanilla < run-roxygen.R
R --vanilla < make_version.R

rem # Build libraries
rem #===============================================================

Rcmd build --force PICTplot
Rcmd INSTALL --build PICTplot

rem # Copy files to local NIWA server
rem #===============================================================

rem copy PICTplot_* \\niwa.local\groups\Wellington\NIWAFisheries\R\ /Y
rem copy PICTplot.html \\niwa.local\groups\Wellington\NIWAFisheries\R\ /Y
del PICTplot.html

rem R --vanilla < update_packages.R

rem # Install locally
rem #===============================================================

R CMD INSTALL PICTplot

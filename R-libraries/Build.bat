rem #===============================================================
rem # Alistair Dunn Mar 2015
rem #===============================================================

R --vanilla < run-roxygen.R
R --vanilla < make_version.R

Rcmd build --force PICTplot
Rcmd INSTALL --build PICTplot

rem copy PICTplot_* \\niwa.local\groups\Wellington\NIWAFisheries\R\ /Y
rem copy PICTplot.html \\niwa.local\groups\Wellington\NIWAFisheries\R\ /Y
del PICTplot.html

rem R --vanilla < update_packages.R

R CMD INSTALL PICTplot

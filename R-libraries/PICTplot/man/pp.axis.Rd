% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/pp.axis.R
\name{pp.axis}
\alias{pp.axis}
\title{Add an axis to a PICT plot}
\usage{
pp.axis(side, at = NULL, labels = TRUE, ...)
}
\arguments{
\item{side}{integer specifying which side of the plot the axis is drawn on.}

\item{at}{the points (in degrees) at which tickmarks are to be drawn. If NULL (default) then the ticks will placed at 'prettied' degree points and, if labels = TRUE, labels are attached to whole degrees.}

\item{labels}{either a character or expression vector of labels to be placed at the tickpoints, or a logical value specifying whether no labels are written or whether whole degree annotations are to be made at the appropriate tickmarks. If labels is not logical, at should also be supplied and of the same length. If FALSE no labels are written.}

\item{...}{other arguments to be passed to the R graphics package function axis including parameters passed by ... in the axis function. E.g. tick to determine if tickmarks and the axis are both drawn, tcl for tickmark length, col and tick.col for the colour of the axis and tickmarks, mgp to position the axes and the labels.}
}
\value{
A vector of numeric locations on the axis scale at which tick marks were drawn. Side effect is to add an axis to the existing plot.
}
\description{
Wrapper function for the R function axis to add an axis to a plot generated by PICT, allowing full freedom to format an axis and tick mark labels.
}


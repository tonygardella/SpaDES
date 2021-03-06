% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting-helpers.R
\docType{methods}
\name{.makeSpadesPlot}
\alias{.makeSpadesPlot}
\alias{.makeSpadesPlot,list,list-method}
\alias{.makeSpadesPlot,list,missing-method}
\alias{.makeSpadesPlot,missing,missing-method}
\title{Make a \code{.spadesPlot} class object}
\usage{
.makeSpadesPlot(plotObjects, plotArgs, whichSpadesPlottables, ...)

\S4method{.makeSpadesPlot}{list,list}(plotObjects, plotArgs,
  whichSpadesPlottables, ...)

\S4method{.makeSpadesPlot}{list,missing}(plotObjects, plotArgs,
  whichSpadesPlottables, ...)

\S4method{.makeSpadesPlot}{missing,missing}(plotObjects, plotArgs,
  whichSpadesPlottables, ...)
}
\arguments{
\item{plotObjects}{list. Any plot objects.}

\item{plotArgs}{list. Any arguments that the the grid package can accept for
the specific grob types, e.g., rasterGrob, polygonGrob, etc.}

\item{whichSpadesPlottables}{Logical indicating which objects in the
\code{Plot} call can be plotted by \code{Plot}.}

\item{...}{additional arguments. Currently nothing.}
}
\value{
A \code{\link{.spadesPlot}} object, which has 2 slots, one for the plot arrangement
(i.e., layout and dimensions) and onefor all of the \code{spadesGrobs}
(stored as a spadesGrobList of lists \code{.spadesGrob} objects).
}
\description{
Builds a \code{.spadesPlot} object from a list of objects.
}
\author{
Eliot McIntire
}


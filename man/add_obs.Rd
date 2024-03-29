% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_obs.R
\name{add_obs}
\alias{add_obs}
\alias{add_obs,data.frame,character-method}
\title{Adding Summary of Observations in Plotted Growth Curves}
\usage{
add_obs(x, method, ...)

\S4method{add_obs}{data.frame,character}(
  x,
  method = c("input", "piled", "relative"),
  factor,
  pch_args = list(),
  err_args = list(),
  scale_error = 1,
  ...
)
}
\arguments{
\item{x}{A data frame obtained by \code{\link{summary_stats}}.}

\item{method}{Method used by \code{\link[cropgrowth]{plot}} for plotting
growth curves.}

\item{...}{Further arguments passed among methods.}

\item{factor}{Variable displayed at the x-axis.}

\item{pch_args, err_args}{Lists of arguments passed to
\code{\link[=points]{points()}} and \code{\link[=segments]{segments()}},
respectively.}

\item{scale_error}{Value multiplied to the dispersion variable for the
display in error bars.}
}
\value{
Points and error bars overlaid to active plot device.
}
\description{
Overlaying tendency and dispersal of observations in plotted growth curves.

For \code{'method="relative"'}, no error bars will be displayed.
}
\examples{

## No example at the moment

}

\name{add_obs}
\docType{methods}
\alias{add_obs}
\alias{add_obs,data.frame,character-method}

\title{Adding Summary of Observations in Plotted Growth Curves}
\description{
Overlaying tendency and dispersal of observations in plotted growth curves.
}
\usage{
\S4method{add_obs}{data.frame,character}(x, method=c("input","piled","relative"), factor,
	pch_args=list(), err_args=list(), scale_error=1, ...)
}
\arguments{
\item{x}{A data frame obtained by \code{\link{summary_stats}}.}
\item{method}{Method used by \code{\link[cropgrowth]{plot}} for plotting growth
	curves.}
\item{factor}{Variable displayed at the x-axis.}
\item{pch_args,err_args}{Lists of arguments passed to
	\code{\link[=points]{points()}} and \code{\link[=segments]{segments()}},
	respectively.}
\item{scale_error}{Value multiplied to the dispersion variable for the display
	in error bars.}
\item{...}{Further arguments passed among methods.}
}
\details{
For \code{'method="relative"'}, no error bars will be displayed.
}
\value{
Points and error bars overlaid to active plot device.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(cropgrowth)

## No example at the moment
}
\keyword{methods}

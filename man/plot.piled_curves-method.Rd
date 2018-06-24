\name{plot.piled_curves-method}
\docType{methods}
\alias{plot}
\alias{plot,character,piled_curves-method}

\title{Plot Growth Curves}
\description{
Ad-hoc function to plot growth curves contained in
\code{\linkS4class{piled_curves}} objects.
}
\usage{
\S4method{plot}{character,piled_curves}(x, y, bottom,
	method=c("input","piled","relative"), col=c("black","grey"),
	fill=c("lightgreen","darkgreen"), lty=1, lwd=1, ...)
}
\arguments{
\item{x}{Character value indicating the time variable to be used as x-axis in
	the plot.}
\item{y}{Object of class \code{\linkS4class{piled_curves}} containing the values
	to be used in the plot.}
\item{bottom}{Custom bottom used for polygons (optional).}
\item{method}{Character value indicating slot of \code{'y'} to be used for the
	plot.}
\item{lty,lwd}{Arguments passed to \code{\link[graphics]{lines}}.}
\item{col,fill}{Colors used for \code{\link[graphics]{lines}} or
	\code{\link[graphics]{polygon}}, respectively.}
\item{...}{Further arguments passed to \code{\link[graphics]{plot}}.}
}
\details{
In the option \code{method="input"} only lines will be drawn, while in the other
cases both, lines and polygons are displayed.
The curves are drawn in the top-down direction.
Most of the graphic parameters will be recycled.

For polygons, if not provided, the argument \code{'bottom'} will be calculated
as minimum value in growth curves.
}
\value{
A plot.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(cropgrowth)

## Subset to treatment T3
maize_model <- subset(maize_model, treatment == "T3")

## Create a piled_curves object
maize_growth <- pile(x=maize_model, time=c("current_date","development_stage","tsum"),
	biomass=c("wst","wglv","wgld","yield"))

## Plot all single curves
par(mar=c(4,5,1,1))
plot("current_date", maize_growth, method="input", col=c("brown","darkgreen","darkgrey",
	"red"), lwd=3, xlab="Time", ylab=expression("Biomass in g m"^-2))

## Plot piled curves (top is total aerial biomass)
plot("current_date", maize_growth, method="piled", col=c("brown","darkgreen","darkgrey",
	"red"), fill=c("yellow","lightgreen","lightgrey","orange"),
	lwd=3, xlab="Time", ylab=expression("Biomass in g m"^-2))

## Plot partition curves (top is total aerial biomass as unit)
plot("current_date", maize_growth, method="relative", col=c("brown","darkgreen",
	"darkgrey", NA), fill=c("yellow","lightgreen","lightgrey","orange"),
	lwd=3, xlab="Time", ylab="Biomass proportion")
}
\keyword{methods}

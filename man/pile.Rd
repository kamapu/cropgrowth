\name{pile}
\docType{methods}
\alias{pile}
\alias{pile,data.frame-method}
\alias{pile,formula-method}

\title{Pile partitions of growth curves}
\description{
Generating objects of class \code{\linkS4class{piled_curves}}.
}
\usage{
\S4method{pile}{data.frame}(x, time, biomass, ...)

\S4method{pile}{formula}(x, data, ...)
}
\arguments{
\item{x}{A data frame or a formula.}
\item{time, biomass}{Character vectors indicating the names of time and biomass
	variables in the input data frame.}
\item{data}{A data frame including time and biomass variables.}
\item{...}{Further arguments passed among methods.}
}
\details{
In the case of crops, sorting of data frames should be done from
left to right according to the bottom-up position of organs in the plants.

In the method \code{'formula'}, the terms at the left side are the sorted
biomass variables, while at the right side are the time variables.
}
\value{
A S4 object of class \code{\linkS4class{piled_curves}}.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(cropgrowth)

## Subset to one treatment
maize_model <- subset(maize_model, treatment == "T3")

## Method for data frame
maize_growth <- pile(x=maize_model, time=c("current_date","development_stage","tsum"),
	biomass=c("wst","wglv","wgld","yield"))

## Method for formula
maize_growth <- pile(wst + wglv + wgld + yield ~ current_date + development_stage + tsum,
	data=maize_model)
}
\keyword{methods}

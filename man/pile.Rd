\name{pile}
\docType{methods}
\alias{pile}
\alias{pile,matrix-method}
\alias{pile,data.frame-method}

\title{Pile partitions of growth curves}
\description{
Generating objects of class \code{\linkS4class{piled_curves}}.
}
\usage{
\S4method{pile}{matrix}(x, time, ...)

\S4method{pile}{data.frame}(x, time, ...)
}
\arguments{
\item{x}{A data frame containing growth curves as columns and sorted from bottom
	to top (e.g. from lower to upper parts of organisms).}
\item{time}{Either a vector or a data frame containing the time variables.}
\item{...}{Further arguments passed among objects.}
}
\details{
In the case of crops, sorting of data frames (or matrices) should be done from
left to right according to the bottom-up position of organs in the plants.

In the method \code{'matrix'}, columns of the matrix have to be named (e.g. by
\code{\link{colnames}}.
}
\value{
A S4 object of class \code{\linkS4class{piled_curves}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(cropgrowth)
maize_model <- subset(maize_model, treatment == "T3")

maize_growth <- pile(maize_model[,c("wrt","wst","wglv","wgld","yield")],
	maize_model$current_date)
}
\keyword{methods}

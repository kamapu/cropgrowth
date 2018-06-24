\name{piled_curves}
\docType{class}
\alias{piled_curves}
\alias{piled_curves-class}

\title{Piled Growth Curves}
\description{
Objects containing growth curves representing partitions inside growing units
(organisms).
}
\section{Slots}{
\describe{
\item{\code{time}}{A data frame including time variables as columns.}
\item{\code{input}}{A matrix including the state (e.g. biomass) of every
	partition as columns.}
\item{\code{piled}}{The same state variables as in \code{'input'} but with
	accumulated values}
\item{\code{relative}}{The relative partition of the state variable.}
}
}
\details{
Objects can be created by \code{new("piled_curves")}, but usually they will
result from the function \emph{pile}.

The slot \code{'time'} can contain different formats or variables used as time
(e.g. date, day of the year, etc.).
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
showClass("piled_curves")
}
\keyword{classes}

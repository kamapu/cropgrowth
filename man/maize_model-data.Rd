\name{maize_model}
\docType{data}
\alias{maize_model}

\title{LINTUL 5 simulation results for maize in East Africa}
\description{
Results of simulation for biomass growth in maize experiments in East Africa.
}
\usage{
data(maize_model)
}
\section{Format}{
A data frame containing biomass growth (as g m^-2) from maize experiments
carried out in East Africa (Kwasunga, Tanzania) and simulated with the model
\bold{LINTUL 5}.
The content of the columns are further detailed:
\describe{
\item{\code{locality}}{Name of the experiment locality.}
\item{\code{crop_name}}{Name of the crop.}
\item{\code{treatment}}{Name of the treatment.}
\item{\code{current_date}}{Date of the simulation day (as \code{\link{Date}}).}
\item{\code{develpment_stage}}{Ordinal value for the stage of development.}
\item{\code{tsum}}{Temperature sum (in °C).}
\item{\code{yield}}{Biomass of storage organs.}
\item{\code{wglv}}{Biomass of green leaves.}
\item{\code{wgld}}{Biomass of dead leaves.}
\item{\code{wst}}{Biomass of stems (stover).}
\item{\code{wrt}}{Biomass of roots.}
\item{\code{lai}}{Leaf area index.}
\item{\code{smact}}{Volumetric soil water content.}
}
}
\source{
\href{https://www.ipe.uni-bonn.de/verbundprojekte/wetland/swea}{SWEA project}.
}
\examples{
library(cropgrowth)
data(maize_model)

## Checking the heads
head(maize_model)
tail(maize_model)
}
\keyword{data-sets}

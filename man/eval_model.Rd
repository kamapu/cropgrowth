\name{eval_model}
\alias{eval_model}

\title{Evaluating Models according to Residuals}
\description{
Evaluation of models will be done comparing observed with simulated data.
Simulated data should be of a higher resolution than observed, while the time
of sampling may not match exactly the simulation steps.
Additionally, multiple observations (repetitions) may be compared with single
simulations.
}
\usage{
eval_model(pred, obs, vars, match, group, FUN=list(), na.rm=FALSE, ...)
}
\arguments{
\item{pred,obs}{Data frames containing predicted and observed variables,
	respectively.}
\item{vars}{Character vector with the names of variables to be evaluated.}
\item{match,group}{Character values or vectors indicating the names of variables
	used to match observations with predictions and to group treatments (see
	also \code{\link{match_observations}}).}
\item{FUN}{List with functions used for the evaluation.}
\item{na.rm}{Logical value, whether NAs in predictions and observations have to
	be skipped or not.}
\item{...}{Further arguments passed to \code{\link{approx}} (see also
	\code{\link{match_observations}}).}
}
\details{
This function matches predictions and observations according
to the variable \code{'match'} within the samples belonging to the same group
(i.e. treatment).

No functions are included to contrast predictions with observations, thus
they have to be defined by the user.
Those functions have to include two arguments, namely \code{'x'} for predictions
and \code{'y'} for observations.
}
\value{
A data frame.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\references{
\bold{Janssen PHM, Heuberger PSC (1995).} Calibration of process-oriented
models. \emph{Ecological Modelling} 83: 55--66.
\url{https://doi.org/10.1016/0304-3800(95)00084-9}

\bold{Bennett ND et al. (2013).} Characterising performance of environmental
models. \emph{Environmental Modelling and Software} 40: 1--20.
\url{http://dx.doi.org/10.1016/j.envsoft.2012.09.011}
}
\examples{
library(cropgrowth)
## Make an example!
}

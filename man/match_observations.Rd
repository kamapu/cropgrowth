\name{match_observations}
\alias{match_observations}

\title{Match Observations with Predictions}
\description{
Interpolation of predicted values for comparison with observed ones.
}
\usage{
match_observations(pred, obs, vars, match, group, suffix="_pred", clean_ties=FALSE, ...)
}
\arguments{
\item{pred, obs}{Data frame containing predicted and observed variables,
	respectively.}
\item{vars}{Character vector with the name of variables to be compared.}
\item{match}{Character value indicating the name of the variable used to match
	observations with predictions. A vector of length 2 can be provided, where
	the first value is the name of the variable in the data frame 'pred' and the
	second is the name of the same variable in 'obs'.}
\item{group}{Character value indicating the name of the variable used for
	grouping predictions and observations (i.e. simulation runs or treatments).
	This argument can be also of length 2 as for 'match'.}
\item{suffix}{Character value used as suffix for predicted variables in output.}
\item{clean_ties}{Logical value, whether ties in the 'match' variable should be
	deleted from the data set with predictions or not.}
\item{...}{Further arguments passed to the function \code{\link{approx}}.}
}
\details{
This function is using \code{\link{approx}} for the interpolation of predicted
variables to observations.
For the interpolation, the variables indicated in \code{'match'} will be
considered as time or progress in the simulated dynamics (e.g. growth).

There are two assumptions that will not be tested by the function.
On the one side the predictions are sorted according to the variable+
\code{'match'}.
On the other side, the single values of the variable \code{'group'} represent
single runs in the simulation (i.e. single treatments in growth experiments).

For interpolation is carried out in a loop for single values in \code{'group'}.
At every run there is a check on occurrence of ties in the variable
\code{'match'} at the data frame \code{'pred'} and a check on observations done
within the simulated range.
If those tests fail, interpolated values will be \code{NA}s and a warning will
be displayed for the run.

Depending on the variable used for measuring the dynamic progress and depending
on its resolution, ties may not be avoidable.
In such cases the argument \code{'clean_ties'} can be set as
\code{'clean_ties=TRUE'}, which will delete all but the last of the tied values
in \code{'pred'}.
}
\value{
A copy of the data frame \code{'obs'} including predicted values for the
requested variables.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(cropgrowth)

## Insert example
}

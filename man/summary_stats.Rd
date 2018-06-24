\name{summary_stats}
\docType{methods}
\alias{summary_stats}
\alias{summary_stats,formula-method}
\alias{summary_stats,character-method}

\title{Summarizing Variables by their Tendency and Distribution}
\description{
Quick displays of tendency and distribution of variables recorded in repetitions
may be required for plotting dots with error bars.
The function \code{summary_stats()} retrieves a data frame with the tendency and
spread of values of a variable by one or more factors.
}
\usage{
\S4method{summary_stats}{formula}(x, trend=mean, spread=sd, data, trend_args=list(),
	spread_args=list())
				
\S4method{summary_stats}{character}(x, factors, trend=mean, spread=sd, data, trend_args=list(),
	spread_args=list())
}
\arguments{
\item{x}{Either a formula or a character vector with the name of variables.}
\item{factors}{Character vector indicating categorical variables for grouping
	the values.}
\item{trend,spread}{The trend and spread functions.}
\item{data}{A data frame containing the records.}
\item{trend_args,spread_args}{Lists of further arguments required by the trend
	and spread functions, respectively.}
}
\details{
Calculation of tendency and distribution parameters for variables grouped by a
combination of factors.
This function is applying \code{\link[=aggregate]{aggregate()}} for grouping
observations.

In the method \code{'formula'}, the terms are expressed as
\emph{variable ~ factors}.
}
\value{
A data frame with one column for the variables, each factor as a column and the
values of tendency and dispersion.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\link{aggregate}}.
}
\examples{
library(cropgrowth)
data(maize_model)

## The formula method
summary_stats(wrt + wst + wglv + wgld + yield ~ treatment, data=maize_model)
}

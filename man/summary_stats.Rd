\name{summary_stats}
\alias{summary_stats}

\title{Summarizing Variables by their Tendency and Distribution}

\description{
Quick displays of tendency and distribution of variables recorded in repetitions
may be required for plotting dots with error bars.
The function \code{summary_stats()} retrieves a data frame with the tendency and
spread of values of a variable by a factor (identity of repetition).
}
\usage{
summary_stats(variable, factor, trend=mean, spread=sd, data=NULL, trend_args=list(),
	spread_args=list())
}
\arguments{
\item{variable,factor}{Character value or symbol indicating a numeric variable
	to be summarized and a categorical variable for grouping the values.}
\item{trend,spread}{The trend and spread functions.}
\item{data}{A data frame containing the records.}
\item{trend_args,spread_args}{Lists of further arguments required by the trend
	and spread functions, respectively.}
}
\details{
Calculation of tendency and distribution parameters for variables grouped by a
factor (e.g. repetitions of records).
This function is applying \code{\link[=aggregate]{aggregate()}} for grouping
observations.
}
\value{
A data frame with three variables, namely factor, tendency and dispersion.
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

summary_stats(lai, treatment, data=maize_model)
}

\name{read_apsim}
\alias{read_apsim}

\title{Read multiple APSIM output files}
\description{
This function is optimized for reading multiple APSIM output files and storing
the data in a single data frame.
}
\usage{
read_apsim(file, path=".", pattern="out")
}
\arguments{
\item{file}{Character vector with the names to be read.}
\item{path}{A character value indicating the relative or absolute path to the
	folder containing the files.}
\item{pattern}{A character value indicating the extension of the files (passed
	to \code{\link{list.files}}).}
}
\details{
Details.
}
\value{
A data frame.
}
\examples{
## Examples will follow
}

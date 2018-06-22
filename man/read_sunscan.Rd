\name{read_sunscan}
\alias{read_sunscan}

\title{Reading SunScan Output Files}

\description{
Importing SunScan readings from text files (output files) into the session.
}
\usage{
read_sunscan(file, version=1, collapse=FALSE)
}
\arguments{
\item{file}{Character value with the name of the file, including extension.}
\item{version}{Version of the output file as integer.}
\item{collapse}{A logical value indicating whether the output may be collapsed
	to a data frame or kept as a list.}
}
\details{
Groups of readings are usually split in different tables within an output file,
which are the elements of the output list if \code{'collapse=FALSE'}.
At the moment only versions 1 and 2 are implemented in the function.

Data frames are separated in the lists "Raw" and "LAI" depending on the content
of the tables.
Raw data are measurements of PAR, while LAI includes estimations of the leaf
area index.
}
\value{
A list containing reading groups in separated data frames (\code{'Raw'} and
\code{'LAI'}).
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\seealso{
\code{\link{readLines}}.
}
\examples{
library(cropgrowth)

file_name <- file.path(path.package("cropgrowth"), "sunscan_v1.txt")
sunscan_v1 <- read_sunscan(file_name)

## Using collapse=TRUE
sunscan_v1 <- read_sunscan(file_name, collapse=TRUE)

## Second example (note different structure of the outputs)
file_name <- file.path(path.package("cropgrowth"), "sunscan_v2.txt")
sunscan_v2 <- read_sunscan(file_name, version=2, collapse=TRUE)
}

\name{read_apsim}
\alias{read_apsim}

\title{Read Multiple APSIM Output Files}
\description{
This function is optimized for reading multiple APSIM output files and storing
the data in a single data frame.
}
\usage{
read_apsim(file, path=".", pattern="out", na_strings, empty_cols=0)
}
\arguments{
\item{file}{Character vector with the names to be read.}
\item{path}{A character value indicating the relative or absolute path to the
	folder containing the files.}
\item{pattern}{A character value indicating the extension of the files (passed
	to \code{\link{list.files}}).}
\item{na_strings}{Character vector with values that have to be converted into
	NAs.}
\item{empty_cols}{Value used to fill new columns, if required.}
}
\details{
If \code{'file'} is missing, the folder indicated in \code{'path'} will be
scanned by \code{\link{list.files}} and all contained files will be loaded.
If some tables contains less columns, missing columns will be created and filled
with the value indicated in \code{'empty_cols'} (usually \code{'0'} or
\code{'NA'}).
The entry indicated as title (usually the name of the file without extension) is
inserted as column \code{'index'} for the respective simulations.
}
\value{
A data frame with daily records as rows and all variables as columns.
}
\source{
Visit the site of the \bold{APSIM} Initiative (\url{http://www.apsim.info}).
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(cropgrowth)

## APSIM example installed in package
rice <- read_apsim(path=path.package("cropgrowth"))
head(rice)
}

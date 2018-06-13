\name{read_apsim}
\alias{read_apsim}

\title{Read multiple APSIM output files}
\description{
This function is optimized for reading multiple APSIM output files and storing
the data in a single data frame.
}
\usage{
read_apsim(file, path=".", pattern="out", empty_cols=0)
}
\arguments{
\item{file}{Character vector with the names to be read.}
\item{path}{A character value indicating the relative or absolute path to the
	folder containing the files.}
\item{pattern}{A character value indicating the extension of the files (passed
	to \code{\link{list.files}}).}
\item{empty_cols}{Value used to fill new columns, if required.}
}
\details{
If \code{'file'} is missing, the folder indicated in \code{'path'} will be
scanned and all contained files will be loaded.
If some tables contains less columns, missing columns will be created and filled
with the value indicated in \code{'empty_cols'} (usually \code{'0'} or
\code{'NA'}).
}
\value{
A data frame.
}
\examples{
## Examples will follow
}

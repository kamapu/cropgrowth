#' @title Read Multiple APSIM Output Files
#' 
#' @description 
#' This function is optimized for reading multiple APSIM output files and
#' storing the data in a single data frame.
#' 
#' If \code{'file'} is missing, the folder indicated in \code{'path'} will be
#' scanned by \code{\link{list.files}} and all contained files will be loaded.
#' If some tables contains less columns, missing columns will be created and
#' filled with the value indicated in \code{'empty_cols'} (usually \code{'0'}
#' or \code{'NA'}). The entry indicated as title (usually the name of the file
#' without extension) is inserted as column \code{'index'} for the respective
#' simulations.
#' 
#' @param file Character vector with the names to be read.
#' @param path A character value indicating the relative or absolute path to
#' the folder containing the files.
#' @param pattern A character value indicating the extension of the files
#' (passed to \code{\link{list.files}}).
#' @param na_strings Character vector with values that have to be converted
#' into NAs.
#' @param empty_cols Value used to fill new columns, if required.
#' 
#' @return A data frame with daily records as rows and all variables as
#' columns.
#' 
#' @source Visit the site of the \bold{APSIM} Initiative
#' (\url{http://www.apsim.info}).
#' 
#' @examples
#' ## APSIM example installed in package
#' rice <- read_apsim(path=path.package("cropgrowth"))
#' head(rice)
#' 
#' @export 
read_apsim <- function(file, path=".", pattern="out", na_strings,
		empty_cols=0) {
	if(missing(file))
		file <- list.files(path=path, pattern=pattern)
	Out <- list()
	for(i in 1:length(file)) {
		apsim <- readLines(file.path(path, file[i]))
		apsim <- strsplit(apsim, "\\s+")
		title_pos <- sapply(apsim, function(x) "title" == tolower(x[1]))
		title_name <- apsim[title_pos][[1]][length(apsim[title_pos][[1]])]
		col_names <- apsim[which(title_pos) + 1][[1]]
		data_frame <- as.data.frame(do.call(rbind,
						apsim[(which(title_pos) + 3):length(apsim)]),
				stringsAsFactors=FALSE)
		colnames(data_frame) <- col_names
		colnames(data_frame)[1] <- "Title"
		data_frame$Title <- title_name
		data_frame$Date <- as.Date(data_frame$Date, "%d/%m/%Y")
		for(j in colnames(data_frame)[!colnames(data_frame) %in%
						c("Title","Date")]) {
			if(!missing(na_strings))
				data_frame[data_frame[,j] %in% na_strings, j] <- NA
			data_frame[,j] <- as.numeric(data_frame[,j])
		}
		Out[[i]] <- data_frame
	}
	complete_columns <- function(x, columns) {
		for(i in columns)
			if(!i %in% colnames(x))
				x[,i] <- empty_cols
		return(x)
	}
	if(length(Out) > 1) {
		col_names <- unique(do.call(c, lapply(Out, colnames)))
		Out <- lapply(Out, complete_columns, columns=col_names)
		Out <- do.call(rbind, Out)
	} else Out <- Out[[1]]
	return(Out)
}

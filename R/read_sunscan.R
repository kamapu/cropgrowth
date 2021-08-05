#' @title Read Sunscan v 1.x
#' 
#' @description
#' Reading files of version 1.x
#' 
#' @name Sunscan_Raw_v1
#' 
#' @keywords internal
Sunscan_Raw_v1 <- function(x) {
	Head <- gsub("^ *|(?<= ) | *$", "", x, perl=TRUE)
	Head <- strsplit(Head, " ", fixed=TRUE)
	Head <- Data <- Head[sapply(Head, length) > 0]
	Head <- Head[1:3]
	index.2 <- cumsum(grepl("Ext", Data))
	Data <- split(Data[index.2 > 0], index.2[index.2 > 0])
	# Extract section as table
	Section <- list()
	for(j in 1:length(Data)) {
		Head.2 <- Data.2 <- Data[[j]]
		Head.2 <- Head.2[1:3]##
		Data.2 <- Data.2[-c(1:3)]##
		index.3 <- seq(1, length(Data.2), by=5)
		for(k in index.3) {
			if(length(Data.2[[k]]) == 7) Data.2[[k]] <- c(Data.2[[k]], "")
			if(length(Data.2[[k]]) > 8) Data.2[[k]] <- c(Data.2[[k]][1:7],
						paste(Data.2[[k]][8:length(Data.2[[k]])], collapse=" "))
			Data.2[[k]] <- c(Data.2[[k]], Data.2[[k+1]], Data.2[[k+2]],
					Data.2[[k+3]], Data.2[[k+4]])
		}
		# Constructing table
		Data.2 <- Data.2[index.3]
		Data.2 <- t(sapply(Data.2, cbind))
		colnames(Data.2) <- c(Head.2[[3]], c(1:64))
		Data.2[,2] <- colnames(Data.2)[2]
		colnames(Data.2)[2:3] <- c("plot","repetition")
		Data.2 <- data.frame(Group=Head.2[[2]][2], ExtSensor=Head.2[[1]][3],
				Data.2, stringsAsFactors=FALSE, check.names=FALSE)
		for(k in colnames(Data.2)[!colnames(Data.2) %in% c("ExtSensor","Time",
						"plot", "repetition","Notes")]) Data.2[,k] <-
					as.numeric(Data.2[,k])
		Section[[j]] <- Data.2
	}
	if(length(Section) == 1) Section <- Section[[1]] else Section <-
				do.call(rbind, Section)
	Section <- data.frame(Title=gsub("Title :", "", paste(Head[[1]],
							collapse=" ")), Location=gsub("Location :", "",
					paste(Head[[2]], collapse=" ")), Date=Head[[3]], Section,
			stringsAsFactors=FALSE, check.names=FALSE)
	return(Section)
}

#' @title Read LAI v 1.x
#' 
#' @description
#' Reading files of version 1.x
#'  
#' @name Sunscan_LAI_v1
#' 
#' @keywords internal
Sunscan_LAI_v1 <- function(x) {
	Head <- gsub("^ *|(?<= ) | *$", "", x, perl=TRUE)
	Head <- strsplit(Head, " ", fixed=TRUE)
	Head <- Data <- Head[sapply(Head, length) > 0]
	Head <- Head[1:4]
	index.2 <- cumsum(grepl("Ext", Data))
	Data <- split(Data[index.2 > 0], index.2[index.2 > 0])
	# Extract section as table
	Section <- list()
	for(j in 1:length(Data)) {
		Head.2 <- Data.2 <- Data[[j]]
		Head.2 <- Head.2[1:4]
		Data.2 <- Data.2[-c(1:4)]
		# Constructing table
		for(k in 1:length(Data.2)) {
			if(length(Data.2[[k]]) == 9) Data.2[[k]] <- c(Data.2[[k]], "")
			if(length(Data.2[[k]]) > 10) Data.2[[k]] <- c(Data.2[[k]][1:9],
						paste(Data.2[[k]][10:length(Data.2[[k]])],
								collapse=" "))
		}
		Data.2 <- t(sapply(Data.2, cbind))
		colnames(Data.2) <- Head.2[[3]]
		Data.2[,2] <- colnames(Data.2)[2]
		colnames(Data.2)[2:3] <- c("plot","repetition")
		colnames(Data.2)[colnames(Data.2) == "Trans-"] <- "Transmitted"
		colnames(Data.2)[colnames(Data.2) == "Incid-"] <- "Incident"
		Data.2 <- data.frame(ExtSensor=Head.2[[1]][3], LeafAngle=Head.2[[1]][8],
				LeafAbsorption=Head.2[[1]][12], Group=Head.2[[2]][2], Data.2,
				stringsAsFactors=FALSE, check.names=FALSE)
		for(k in colnames(Data.2)[!colnames(Data.2) %in% c("ExtSensor","Group",
						"Time","plot","repetition","Notes")]) Data.2[,k] <-
					as.numeric(Data.2[,k])
		Section[[j]] <- Data.2
	}
	if(length(Section) == 1) Section <- Section[[1]] else Section <-
				do.call(rbind, Section)
	Section <- data.frame(Title=gsub("Title :", "", paste(Head[[1]],
							collapse=" ")),
			Location=gsub("Location :", "", paste(Head[[2]], collapse=" ")),
			Latitude=gsub("Latitude :", "", paste(Head[[3]][1:2],
							collapse=" ")),
			Longitude=gsub("Longitude :", "", paste(Head[[3]][3:4],
							collapse=" ")),
			Date=Head[[4]][1], TimeZone=Head[[4]][5], Section,
			stringsAsFactors=FALSE, check.names=FALSE)
	return(Section)
}

#' @title Read LAI v 2.x
#' 
#' @description
#' Reading files of version 2.x
#'  
#' @name Sunscan_Raw_v2
#' 
#' @keywords internal
Sunscan_Raw_v2 <- function(x) {
	Data <- strsplit(x, "\t", fixed=TRUE)
	Head <- Data[1:10]
	Data <- Data[sapply(Data, length) == 74][-1]
	Data <- t(sapply(Data, cbind))
	colnames(Data) <- Head[[10]]
	Data <- data.frame(Title=Head[[1]][2], Location=Head[[2]][2],
			Date=Head[[4]][1], ExtSensor=Head[[7]][2], Data,
			stringsAsFactors=FALSE, check.names=FALSE)
	Data <- Data[,!colnames(Data) %in% c("V8","V9")]
	for(k in colnames(Data)[!colnames(Data) %in% c("Title","Location","Date",
					"ExtSensor","Time","plot","repetition",
					"Notes")]) Data[,k] <- as.numeric(Data[,k])
	return(Data)
}

#' @title Reading SunScan Output Files
#' 
#' @description 
#' Importing SunScan readings from text files (output files) into the session.
#' 
#' Groups of readings are usually split in different tables within an output
#' file, which are the elements of the output list if \code{'collapse=FALSE'}.
#' At the moment only versions 1 and 2 are implemented in the function.
#' 
#' Data frames are separated in the lists "Raw" and "LAI" depending on the
#' content of the tables. Raw data are measurements of PAR, while LAI includes
#' estimations of the leaf area index.
#' 
#' @param file Character value with the name of the file, including extension.
#' @param version Version of the output file as integer.
#' @param collapse A logical value indicating whether the output may be
#' collapsed to a data frame or kept as a list.
#' 
#' @return A list containing reading groups in separated data frames (`'Raw'`
#'   and `'LAI'`).
#' 
#' @seealso [readLines()].
#' 
#' @examples
#' file_name <- file.path(path.package("cropgrowth"), "sunscan_v1.txt")
#' sunscan_v1 <- read_sunscan(file_name)
#' 
#' ## Using collapse=TRUE
#' sunscan_v1 <- read_sunscan(file_name, collapse=TRUE)
#' 
#' ## Second example (note different structure of the outputs)
#' file_name <- file.path(path.package("cropgrowth"), "sunscan_v2.txt")
#' sunscan_v2 <- read_sunscan(file_name, version=2, collapse=TRUE)
#' 
#' @export 
read_sunscan <- function(file, version=1, collapse=FALSE) {
	x <- readLines(file, warn=FALSE)
	index <- cumsum(grepl("Title", x))
	x <- split(x[index > 0], index[index > 0])
	Out <- list(Raw=list(), LAI=list())
	if(version == 1) {
		for(i in 1:length(x)) {
			if(grepl("Latitude", x[[i]][3])) {
				Out[["LAI"]][[length(Out[["LAI"]]) + 1]] <-
						Sunscan_LAI_v1(x[[i]])
			} else {
				Out[["Raw"]][[length(Out[["Raw"]]) + 1]] <-
						Sunscan_Raw_v1(x[[i]])
			}
		}
	}
	if(version == 2) {
		for(i in 1:length(x)) {
			Out[["Raw"]][[length(Out[["Raw"]]) + 1]] <- Sunscan_Raw_v2(x[[i]])
		}
	}
	if(collapse == TRUE) {
		Out[["Raw"]] <- do.call(rbind, Out[["Raw"]])
		Out[["LAI"]] <- do.call(rbind, Out[["LAI"]])
	}
	return(Out)
}

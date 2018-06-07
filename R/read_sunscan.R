# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

function(file, merge=FALSE) {
	x <- readLines(file)
	index <- cumsum(grepl("Title", x))
	x <- subset(x, index > 0)
	index <- subset(index, index > 0)
	x <- split(x, index)
	for(i in names(x)) {
		pre.tab <- strsplit(x[[i]], "\t", fixed=TRUE)
		data <- rep(0, times=length(pre.tab))
		for(j in 1:length(data)) data[j] <- length(pre.tab[[j]])
		data <- subset(pre.tab, data == max(data))
		data.2 <- matrix(ncol=length(data[[1]]), nrow=length(data)-1)
		for(j in 1:length(data)-1) data.2[j,] <- data[[j+1]]
		colnames(data.2) <- data[[1]]
		data.2 <- data.2[,which(colnames(data.2) != "")]
		n <- which(colnames(data.2) == "1")
		for(j in as.numeric(n):(dim(data.2)[2])) {
			colnames(data.2)[j] <- j-n+1
		}
		# To a character data frame
		x[[i]] <- as.list(as.data.frame(data.2, stringsAsFactors=FALSE))
		# Numbers into numeric variables
		for(j in names(x[[i]])) {
			if(!j %in% c("Time","plot","repetition","Notes")) {
				x[[i]][[j]] <- as.numeric(paste(x[[i]][[j]]))
			}
		}
		x[[i]] <- as.data.frame(x[[i]], stringsAsFactors=FALSE)
		OldNames <- names(x[[i]])
		NewNames <- c("Titel","Location","Date","Group")
		x[[i]][,"Titel"] <- c(pre.tab[[1]][2])
		x[[i]][,"Location"] <- c(pre.tab[[2]][2])
		x[[i]][,"Date"] <- c(pre.tab[[4]][1])
		x[[i]][,"Group"] <- c(pre.tab[[8]][2])
		x[[i]] <- x[[i]][,c(NewNames,OldNames)]
	}
	if(merge == TRUE) {
		x.2 <- as.list(x[[1]])
		for(j in 2:length(x)) {
			for(k in c(NewNames,OldNames)) {
				x.2[[k]] <- c(x.2[[k]],x[[j]][,k])
			}
		}
		x.2 <- as.data.frame(x.2, stringsAsFactors=FALSE)
		return(x.2)
	} else {return(x)}
}

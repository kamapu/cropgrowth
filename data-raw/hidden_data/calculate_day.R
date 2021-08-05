# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

function(Date, Start=NULL) {
	if(is.null(Start)) {
		Start <- format(Date, "%Y")
		Start <- strptime(paste(Start,"01.01",sep="."), format="%Y.%m.%d")
	} 
	Start <- as.numeric(as.Date(Start))
	DOY <- (as.numeric(as.Date(Date)) - Start) + 1
	return(DOY)
}



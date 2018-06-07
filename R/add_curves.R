# TODO:   Stapple grouth curves
# 
# Author: Miguel Alvarez
################################################################################

function(x, curves, order) {
	if(is.null(attributes(curves)$dimnames[[2]]))
		stop("colnames are mandatory: name columns in the input matricurves!")
	if(missing(order)) order <- colnames(curves)
	for(i in order) {
		if(i %in% colnames(curves) == FALSE)
			stop("argument 'order' should be contained in column names")
	}
	curves <- curves[,order]
	if(missing(x)) x <- 1:(dim(curves)[1])
	out <- new("AddedCurves")
	out@x <- x
	out@original <- curves
	out@absolute <- curves
	out@relative <- curves
	for(i in 2:length(order)) {
		out@absolute[,i] <- out@absolute[,i]+out@absolute[,i-1]
	}
	vector.sum <- rowSums(out@original)
	for(i in 1:length(order)) {
		out@relative[,i] <- out@absolute[,i]/vector.sum
	}
	return(out)
}

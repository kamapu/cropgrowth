# TODO:   Stapple grouth curves
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("pile",
		function(x, ...)
			standardGeneric("pile")
)

# Method for data.frame
setMethod("pile", signature(x="data.frame"),
		function(x, time, ...) {
			if(is.null(dim(time))) {
				if(length(time) != nrow(x))
					stop("Length of 'time' does not macht number of rows in 'x'.")
			}
			if(is.data.frame(time)) {
				if(nrow(time) != nrow(x))
					stop("Number of rows in 'time' does not macht number of rows in 'x'.")
			}
			out <- new("piled_curves")
			if(is.null(dim(time)))
				out@time <- data.frame(time=time, stringsAsFactors=FALSE) else
				out@time <- time
			out@input <- as.matrix(x)
			out@piled <- t(apply(as.matrix(x), 1, cumsum))
			out@relative <- t(apply(as.matrix(x), 1,
							function(x) cumsum(x)/sum(x)))
			return(out)
		}
)

# method for matrix
setMethod("pile", signature(x="matrix"),
		function(x, time, ...) {
			if(is.null(colnames(x)))
				stop("Column names of 'x' have to be named.")
			return(pile(as.data.frame(x), time, ...))
		}
)

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
		function(x, time, biomass, ...) {
			out <- new("piled_curves")
			out@time <- x[,time]
			out@input <- as.matrix(x[,biomass])
			out@piled <- t(apply(out@input, 1, cumsum))
			out@relative <- t(apply(out@input, 1, function(x) cumsum(x)/sum(x)))
			return(out)
		}
)

# Method for formula
setMethod("pile", signature(x="formula"),
		function(x, data, ...) {
			biomass <- strsplit(as.character(x)[2], "+", fixed=TRUE)[[1]]
			biomass <- gsub("\\s+", "", biomass)
			time <- attr(terms(x), "term.labels")
			return(pile(data, time, biomass))
		}
)

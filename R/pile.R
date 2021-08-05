#' @title Pile Partitions of Growth Curves
#' 
#' @description 
#' Generating objects of class  [piled_curves-class].
#' 
#' In the case of crops, sorting of data frames should be done from left to
#' right according to the bottom-up position of organs in the plants.
#' 
#' In the method `'formula'`, the terms at the left side are the sorted
#' biomass variables, while at the right side are the time variables.
#' 
#' @name pile
#' @rdname pile
#' 
#' @param x A data frame or a formula.
#' @param time,biomass Character vectors indicating the names of time and
#' biomass variables in the input data frame.
#' @param data A data frame including time and biomass variables.
#' @param ... Further arguments passed among methods.
#' 
#' @return A S4 object of class [piled_curves-class].
#' 
#' @examples
#' ## Subset to one treatment
#' maize_model <- subset(maize_model, treatment == "T3")
#' 
#' ## Method for data frame
#' maize_growth <- pile(x=maize_model, time=c("current_date","development_stage","tsum"),
#' 	biomass=c("wst","wglv","wgld","yield"))
#' 
#' ## Method for formula
#' maize_growth <- pile(wst + wglv + wgld + yield ~ current_date + development_stage + tsum,
#' 	data=maize_model)
#' 
#' @export 
setGeneric("pile",
		function(x, ...)
			standardGeneric("pile")
)

#' @rdname pile
#' @aliases pile,data.frame-method
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

#' @rdname pile
#' @aliases pile,formula-method
setMethod("pile", signature(x="formula"),
		function(x, data, ...) {
			biomass <- strsplit(as.character(x)[2], "+", fixed=TRUE)[[1]]
			biomass <- gsub("\\s+", "", biomass)
			time <- attr(terms(x), "term.labels")
			return(pile(data, time, biomass))
		}
)

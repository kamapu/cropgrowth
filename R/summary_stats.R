# TODO:   Calculation of trend and distribution at once
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("summary_stats",
		function(x, ...)
			standardGeneric("summary_stats")
)

# Method for class 'formula'
setMethod("summary_stats", signature(x="formula"),
		function(x, trend=mean, spread=sd, data, trend_args=list(),
				spread_args=list()) {
			factors <- paste(x)[3]
			variables <- strsplit(paste(x)[2], " + ", fixed=TRUE)[[1]]
			summ <-  list()
			for(i in variables) {
				summ[[i]] <- list()
				summ[[i]]$trend <- aggregate(as.formula(paste(i, "~", factors)),
						data, function(x) do.call(trend, c(list(x=x),
											trend_args)))
				summ[[i]]$spread <- aggregate(as.formula(paste(i, "~", factors)),
						data, function(x) do.call(spread, c(list(x=x),
											spread_args)))
				colnames(summ[[i]]$trend)[2] <- "tendency"
				summ[[i]]$trend$dispersion <- summ[[i]]$spread[,2]
				summ[[i]] <- data.frame(variable=i, summ[[i]]$trend,
						stringsAsFactors=FALSE)
			}
			summ <- do.call(rbind, summ)
			rownames(summ) <- NULL
			return(summ)
		}
)

# Method for class 'character'
setMethod("summary_stats", signature(x="character"),
		function(x, factors, trend=mean, spread=sd, data, trend_args=list(),
				spread_args=list()) {
			if(length(x) > 1) x <- paste(x, collapse= " + ")
			if(length(factors) > 1) factors <- paste(factors, collapse= " + ")
			
			x <- as.formula(paste(x, "~", factors))
			return(summary_stats(x, trend, spread, data, trend_args,
							spread_args))
		}
)

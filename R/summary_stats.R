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
			var_trend <- aggregate(x, data, function(x) do.call(trend,
								c(list(x=x), trend_args)))
			var_spread <- aggregate(x, data, function(x) do.call(spread,
								c(list(x=x), spread_args)))
			colnames(var_trend)[ncol(var_trend)] <- "tendency"
			var_trend$dispersion <- var_spread[,ncol(var_trend)]
			return(var_trend)
		}
)

# Method for class 'data.frame'
setMethod("summary_stats", signature(x="character"),
		function(x, factors, trend=mean, spread=sd, data, trend_args=list(),
				spread_args=list()) {
			x <- as.formula(paste(x, "~", paste(factors, collapse=" + ")))
			return(summary_stats(x, trend, spread, data, trend_args,
							spread_args))
		}
)

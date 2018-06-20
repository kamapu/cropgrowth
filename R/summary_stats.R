# TODO:   Calculation of trend and distribution at once
# 
# Author: Miguel Alvarez
################################################################################

summary_stats <- function(variable, factor, trend=mean, spread=sd,
		data=NULL, trend_args=list(), spread_args=list()) {
	variable <- substitute(variable)
	variable <- eval(variable, data, parent.frame())
	factor <- substitute(factor)
	factor <- eval(factor, data, parent.frame())
	var_trend <- aggregate(variable ~ factor, data, function(x) do.call(trend,
						c(list(x=x), trend_args)))
	var_spread <- aggregate(variable ~ factor, data, function(x) do.call(spread,
						c(list(x=x), spread_args)))
	colnames(var_trend)[2] <- "tendency"
	var_trend$dispersion <- var_spread[,2]
	return(var_trend)
}

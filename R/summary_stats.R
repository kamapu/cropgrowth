#' @title Summarizing Variables by their Tendency and Distribution
#' 
#' @description 
#' Quick displays of tendency and distribution of variables recorded in
#' repetitions may be required for plotting dots with error bars. The function
#' \code{summary_stats()} retrieves a data frame with the tendency and spread
#' of values of a variable by one or more factors.
#' 
#' Calculation of tendency and distribution parameters for variables grouped by
#' a combination of factors. This function is applying
#' \code{\link[=aggregate]{aggregate()}} for grouping observations.
#' 
#' In the method \code{'formula'}, the terms are expressed as \emph{variable ~
#' factors}.
#' 
#' @name summary_stats
#' @rdname summary_stats
#' @aliases summary_stats summary_stats,formula-method
#' summary_stats,character-method
#' 
#' @param x Either a formula or a character vector with the name of variables.
#' @param factors Character vector indicating categorical variables for
#'   grouping the values.
#' @param trend,spread The trend and spread functions.
#' @param data A data frame containing the records.
#' @param trend_args,spread_args Lists of further arguments required by the
#'   trend and spread functions, respectively.
#' @param ... Further arguments passed among methods.
#' 
#' @return A data frame with one column for the variables, each factor as a
#' column and the values of tendency and dispersion.
#' 
#' @seealso [aggregate()].
#' 
#' @examples
#' data(maize_model)
#' 
#' ## The formula method
#' summary_stats(wrt + wst + wglv + wgld + yield ~ treatment, data=maize_model)
#' 
#' @export 
setGeneric("summary_stats",
		function(x, ...)
			standardGeneric("summary_stats")
)

#' @rdname summary_stats
#' @aliases summary_stats,formula-method
setMethod("summary_stats", signature(x = "formula"),
		function(x, trend = mean, spread = sd, data, trend_args = list(),
				spread_args = list()) {
			factors <- paste(x)[3]
			variables <- strsplit(paste(x)[2], " + ", fixed = TRUE)[[1]]
			summ <-  list()
			for(i in variables) {
				summ[[i]] <- list()
				summ[[i]]$trend <- aggregate(as.formula(paste(i, "~", factors)),
						data, function(x) do.call(trend, c(list(x = x),
											trend_args)))
				summ[[i]]$spread <- aggregate(as.formula(paste(i, "~",
										factors)),
						data, function(x) do.call(spread, c(list(x = x),
											spread_args)))
				colnames(summ[[i]]$trend)[ncol(summ[[i]]$trend)] <- "tendency"
				summ[[i]]$trend$dispersion <- summ[[i]]$spread[,
						ncol(summ[[i]]$spread)]
				summ[[i]] <- data.frame(variable = i, summ[[i]]$trend,
						stringsAsFactors = FALSE)
			}
			summ <- do.call(rbind, summ)
			rownames(summ) <- NULL
			return(summ)
		}
)

#' @rdname summary_stats
#' @aliases summary_stats,character-method
setMethod("summary_stats", signature(x = "character"),
		function(x, factors, trend = mean, spread = sd, data, trend_args = list(),
				spread_args = list()) {
			if(length(x) > 1) x <- paste(x, collapse =  " + ")
			if(length(factors) > 1) factors <- paste(factors, collapse =  " + ")
			
			x <- as.formula(paste(x, "~", factors))
			return(summary_stats(x, trend, spread, data, trend_args,
							spread_args))
		}
)

#' @title Adding Summary of Observations in Plotted Growth Curves
#' 
#' @description 
#' Overlaying tendency and dispersal of observations in plotted growth curves.
#' 
#' For \code{'method="relative"'}, no error bars will be displayed.
#' 
#' @name add_obs
#' @rdname add_obs
#'
#' @param x A data frame obtained by \code{\link{summary_stats}}.
#' @param method Method used by \code{\link[cropgrowth]{plot}} for plotting
#' growth curves.
#' @param factor Variable displayed at the x-axis.
#' @param pch_args,err_args Lists of arguments passed to
#' \code{\link[=points]{points()}} and \code{\link[=segments]{segments()}},
#' respectively.
#' @param scale_error Value multiplied to the dispersion variable for the
#' display in error bars.
#' @param ... Further arguments passed among methods.
#' 
#' @return Points and error bars overlaid to active plot device.
#' 
#' @examples
#' 
#' ## No example at the moment
#' 
#' @export 
setGeneric("add_obs",
		function(x, method, ...)
			standardGeneric("add_obs")
)

#' @rdname add_obs
#' @aliases add_obs,data.frame,character-method
setMethod("add_obs", signature(x="data.frame", method="character"),
		function(x, method=c("input","piled","relative"), factor,
				pch_args=list(), err_args=list(), scale_error=1, ...) {
			if(length(factor) > 1) {
				warning("Only the first value of 'factor' will be used.")
				factor <- factor[1]
			}
			x <- split(x, x$variable)[unique(x$variable)]
			for(i in names(pch_args)) pch_args[[i]] <- rep_len(pch_args[[i]],
						length(x))
			for(i in names(err_args)) err_args[[i]] <- rep_len(err_args[[i]],
						length(x))
			# conversion of data to piled
			if(method[1] == "piled" | method[1] == "relative")
				for(i in 2:length(x))
					x[[i]]$tendency <- (x[[i]]$tendency + x[[i - 1]]$tendency)
			# conversion of data to relative
			if(method[1] == "relative")
				for(i in 1:length(x))
					x[[i]]$tendency <- (x[[i]]$tendency/x[[length(x)]]$tendency)
			# The plot
			for(i in rev(1:length(x))) {
				with(x[[i]], {
							if(method[1] != "relative")
								do.call(segments,
										c(list(x0=get(factor),
												y0=(tendency -
													dispersion*scale_error),
												y1=(tendency +
													dispersion*scale_error)),
												lapply(err_args, "[", i)))
							do.call(points,
									c(list(x=get(factor), y=tendency),
											lapply(pch_args, "[", i)))
						}
				)
			}
		}
)

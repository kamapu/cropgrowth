# TODO:   Add observations to piled curves
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("add_obs",
		function(x, method, ...)
			standardGeneric("add_obs")
)

# Method for data.frame
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
								do.call(segments, c(list(x0=get(factor),
														y0=(tendency - dispersion*scale_error),
														y1=(tendency + dispersion*scale_error)),
												lapply(err_args, "[", i)))
							do.call(points, c(list(x=get(factor), y=tendency),
											lapply(pch_args, "[", i)))
						}
				)
			}
		}
)

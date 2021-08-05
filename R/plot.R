#' @title Plot Growth Curves
#' 
#' @description 
#' Ad-hoc function to plot growth curves contained in [piled_curves-class]
#' objects.
#' 
#' In the option `'method = "input"'` only lines will be drawn, while in the
#' other cases both, lines and polygons are displayed. The curves are drawn in
#' the top-down direction. Most of the graphic parameters will be recycled.
#' 
#' For polygons, if not provided, the argument `'bottom'` will be
#' calculated as minimum value in growth curves.
#' 
#' @name plot
#' @rdname plot
#' 
#' @param x Character value indicating the time variable to be used as x-axis
#' in the plot.
#' @param y Object of class \code{\linkS4class{piled_curves}} containing the
#' values to be used in the plot.
#' @param bottom Custom bottom used for polygons (optional).
#' @param method Character value indicating slot of \code{'y'} to be used for
#' the plot.
#' @param lty,lwd Arguments passed to \code{\link[graphics]{lines}}.
#' @param col,fill Colors used for \code{\link[graphics]{lines}} or
#' \code{\link[graphics]{polygon}}, respectively.
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#' 
#' @return A plot.
#' 
#' @examples
#' ## Subset to treatment T3
#' maize_model <- subset(maize_model, treatment == "T3")
#' 
#' ## Create a piled_curves object
#' maize_growth <- pile(x=maize_model, time=c("current_date","development_stage","tsum"),
#' 	biomass=c("wst","wglv","wgld","yield"))
#' 
#' ## Plot all single curves
#' par(mar=c(4,5,1,1))
#' plot("current_date", maize_growth, method="input", col=c("brown","darkgreen","darkgrey",
#' 	"red"), lwd=3, xlab="Time", ylab=expression("Biomass in g m"^-2))
#' 
#' ## Plot piled curves (top is total aerial biomass)
#' plot("current_date", maize_growth, method="piled", col=c("brown","darkgreen","darkgrey",
#' 	"red"), fill=c("yellow","lightgreen","lightgrey","orange"),
#' 	lwd=3, xlab="Time", ylab=expression("Biomass in g m"^-2))
#' 
#' ## Plot partition curves (top is total aerial biomass as unit)
#' plot("current_date", maize_growth, method="relative", col=c("brown","darkgreen",
#' 	"darkgrey", NA), fill=c("yellow","lightgreen","lightgrey","orange"),
#' 	lwd=3, xlab="Time", ylab="Biomass proportion")
#' 
#' @exportMethod plot
if (!isGeneric("plot"))
	setGeneric("plot", function(x, y, ...) standardGeneric("plot")) 

#' @rdname plot
#' @aliases plot,character,piled_curves-method
setMethod("plot", signature(x="character", y="piled_curves"),
		function(x, y, bottom, method=c("input","piled","relative"),
				col=c("black","grey"), fill=c("lightgreen","darkgreen"),
				lty=1, lwd=1, ...) {
			x2 <- slot(y, "time")[,x]
			y2 <- slot(y, method[1])
			n <- dim(y2)[2] # How many curves
			plot(x2, rep_len(range(y2), length(x2)), type="n", ...)
			# some plotting parameters
			col <- rep_len(col, n)
			lty <- rep_len(lty, n)
			lwd <- rep_len(lwd, n)
			fill <- rep_len(fill, n)
			if(method[1] == "input") {
				for(i in n:1)
					lines(x2, y2[,i], col=col[i], lty=lty[i], lwd=lwd[i])
			} else {
				if(missing(bottom)) bottom <- min(y2)
				for(i in n:1) {
					polygon(
							c(x2[1], x2, x2[length(x2)]),
							c(bottom, y2[,i], bottom),
							col=fill[i], border=NA)
					lines(x2, y2[,i], col=col[i], lty=lty[i], lwd=lwd[i])
				}	
			}
		}
)

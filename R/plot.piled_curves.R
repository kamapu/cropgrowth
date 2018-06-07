# TODO:   Plot added curves
# 
# Author: Miguel Alvarez
################################################################################

if (!isGeneric("plot"))
	setGeneric("plot", function(x, y, ...) standardGeneric("plot")) 

# Method for piled_curves objects
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

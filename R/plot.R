# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

function(par.trans, par.total, Time, eladp, lat_dec, long_dec) {
	## require(RAtmosphere)
	zenith <- SZA(Time, Lat=lat_dec, Lon=long_dec)*pi/180
	K <- sqrt(eladp^2 + tan(2*zenith))/(1.74 + 0.45*eladp + 0.1223*eladp^2 -
				0.013*eladp^3 + 0.000509*eladp)
	if(!is.vector(par.trans)) par.trans <- rowMeans(par.trans)
	if(!is.vector(par.total)) par.total <- rowMeans(par.total)
	LAI <- -log(par.trans/par.total)/K
	return(LAI)
}
> plot.AddedCurves
function(x, bottom=NULL,
		method=c("original","absolute","relative"), col=c("black","grey"),
		border=NULL, lty=1, lwd=1, ...) {
	method <- pmatch(method, c("original","absolute","relative"))[1]
	x2 <- slot(x,"x")
	if(method == 1) y2 <- slot(x,"original")
	if(method == 2) y2 <- slot(x,"absolute")
	if(method == 3) y2 <- slot(x,"relative")
	n <- dim(y2)[2] # How many y.
	plot(x2, rep_len(range(y2), length(x2)), type="n", ...)
	# some plotting parameters
	col <- rep_len(col, n)
	if(is.null(border)) border <- col else border <- rep_len(border, n)
	lty <- rep_len(lty, n)
	lwd <- rep_len(lwd, n)
	if(is.null(bottom)) bottom=min(y2)
	if(method == 1) drawLines(x2, y2, col=col, lty=lty, lwd=lwd)
	if(method != 1) drawPolygons(x2, y2, bottom, border=border, col=col,
				lty=lty, lwd=lwd)
}


# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

function(x, time.name, format, vars, stat, ...) {
	x <- x[,c(time.name, vars)]
	x$time.num <- as.numeric(as.Date(x[,time.name], format=format))
	x <- split(x, x$time.num)
	out <- matrix(nrow=length(x), ncol=length(vars))
	rownames(out) <- names(x)
	colnames(out) <- vars
	name.fun <- as.character(substitute(stat))
	FUN <- match.fun(name.fun)
	for(i in names(x)) {
		for(j in vars) {
			if(name.fun == "length") {
				out[i,j] <- FUN(x[[i]][,j])-sum(is.na(x[[i]][,j]))} else {
				out[i,j] <- FUN(x[[i]][,j], ...)
			}
		}
	}
	Date <- format(as.Date(as.numeric(rownames(out)), origin = "1970-01-01"),
			format = format)
	out <- data.frame(Date, out)
	colnames(out) <- c(time.name, vars)
	out
}

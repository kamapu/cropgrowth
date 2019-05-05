# TODO:   Application of functions to calculate the residuals
# 
# Author: Miguel Alvarez
################################################################################

eval_model <- function(pred, obs, vars, match, group, FUN=list(), na.rm=FALSE,
		...) {
	obs <- match_observations(pred, obs, vars, match, group, suffix="_pred",
			...)
	# Calculation of parameters
	obs <- split(obs, unique(obs[,group]))
	vars_fit <- list()
	for(i in names(obs)) {
		vars_fit[[i]] <- list()
		for(j in vars) {
			vars_fit[[i]][[j]] <- list()
			for(k in names(FUN)) {
				# NA check only applied for observations
				if(na.rm) {
					vars_fit[[i]][[j]][[k]] <-
							with(obs[[i]][!is.na(obs[[i]][,j]),],
							do.call(k, list(x=get(paste0(j, "_pred")),
													y=get(j))))
				} else {
					vars_fit[[i]][[j]][[k]] <- with(obs[[i]],
							do.call(k, list(x=get(paste0(j, "_pred")),
											y=get(j))))
				}
			}
			vars_fit[[i]][[j]] <- data.frame(group=j, var=i, do.call(cbind,
							vars_fit[[i]][[j]]), stringsAsFactors=FALSE)
		}
		vars_fit[[i]] <- do.call(rbind, vars_fit[[i]])
	}
	vars_fit <- do.call(rbind, vars_fit)
	rownames(vars_fit) <- NULL
	return(vars_fit)
}

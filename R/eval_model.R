# TODO:   Application of functions to calculate the residuals
# 
# Author: Miguel Alvarez
################################################################################

eval_model <- function(vars, match, group, pred, obs, FUN=list(), na.rm=FALSE) {
	# Matching groups
	group_values <- intersect(unique(pred[,group]), unique(obs[,group]))
	for(i in c("pred","obs")) {
		assign(i, get(i)[get(i)[,group] %in% group_values,])
		assign(i, split(get(i), get(i)[, group]))
	}
	# Assignment of matches
	for(i in paste(group_values)) {
		match_values <- unique(obs[[i]][,match])
		select_matches <- sapply(as.list(match_values), function(x, y) {
					which.min(abs(y - x))
				}, y=pred[[i]][,match])
		pred[[i]] <- pred[[i]][select_matches,]
		pred[[i]][,match] <- match_values
	}
	# Observations and predictions per variable per group
	vars_diff <- list()
	for(i in vars) {
		vars_diff[[i]] <- list()
		for(j in paste(group_values)) {
			obs_df=obs[[j]][,c(match,i)]
			pred_df=pred[[j]][,c(match,i)]
			colnames(obs_df)[2] <- "observed"
			colnames(pred_df)[2] <- "predicted"
			vars_diff[[i]][[j]] <- merge(obs_df, pred_df, sort=FALSE)
			vars_diff[[i]][[j]]$group <- j
			if(na.rm)
				vars_diff[[i]][[j]] <- vars_diff[[i]][[j]][
						!is.na(vars_diff[[i]][[j]]$observed) &
								!is.na(vars_diff[[i]][[j]]$predicted),]
		}
		## vars_diff[[i]] <- do.call(rbind, vars_diff[[i]])
	}
	# Calculation of parameters
	vars_fit <- list()
	for(i in vars) {
		vars_fit[[i]] <- list()
		for(j in paste(group_values)) {
			vars_fit[[i]][[j]] <- list()
			for(k in names(FUN))
				vars_fit[[i]][[j]][[k]] <- with(vars_diff[[i]][[j]],
						do.call(k, list(x=predicted, y=observed)))
			vars_fit[[i]][[j]] <- data.frame(group=j, var=i, do.call(cbind,
							vars_fit[[i]][[j]]), stringsAsFactors=FALSE)
		}
		vars_fit[[i]] <- do.call(rbind, vars_fit[[i]])
	}
	vars_fit <- do.call(rbind, vars_fit)
	rownames(vars_fit) <- NULL
	return(vars_fit)
}

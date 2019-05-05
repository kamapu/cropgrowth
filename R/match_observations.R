# TODO:   Matching observed with predicted data
# 
# Author: Miguel Alvarez
################################################################################

match_observations <- function(pred, obs, vars, match, group, suffix="_pred",
		clean_ties=FALSE, ...) {
	# Tests for variable 'match'
	if(length(match) != 2) match <- rep_len(match, 2)
	if(!match[1] %in% colnames(pred))
		stop("Variable 'match' missed in 'pred'")
	if(!match[2] %in% colnames(obs))
		stop("Variable 'match' missed in 'obs'")
	# Tests for variable 'group'
	if(length(group) != 2) group <- rep_len(group, 2)
	if(!group[1] %in% colnames(pred))
		stop("Variable 'group' missed in 'pred'")
	if(!group[2] %in% colnames(obs))
		stop("Variable 'group' missed in 'obs'")
	if(!all(obs[,group[2]] %in% pred[,group[1]]))
		stop(paste0("Some values of'", group[2],"' in 'obs' are missed at '",
						group[1], "' in 'pred'"))
	# Tests for variable 'vars'
	if(!all(vars %in% colnames(obs)))
		stop("Some values of 'vars' are missed as variables in 'obs'")
	if(!all(vars %in% colnames(pred)))
		stop("Some values of 'vars' are missed as variables in 'pred'")
	# Splitting inputs
	pred <- split(pred[pred[,group[1]] %in% obs[,group[2]], c(match[1], vars)],
			pred[,group[1]])
	obs <- split(obs, obs[,group[2]])
	# Interpolation in a loop
	for(i in names(pred)) {
		# Clean ties
		if(clean_ties)
			pred[[i]] <- pred[[i]][rev(!duplicated(rev(pred[[i]][,match[1]]))),]
		# Check ties
		if(any(duplicated(pred[[i]][,match[1]]))) {
			for(j in vars)
				obs[[i]][,paste0(j, suffix)] <- NA
			warning(paste0("Ties detected in 'pred' for group '", i, "'"))
			next
		}
		# Check intervals
		R1 <- range(obs[[i]][,match[2]])
		R2 <- range(pred[[i]][,match[1]])
		if(!all(R1[1] > R2[1], R1[2] < R2[2])) {
			for(j in vars)
				obs[[i]][,paste0(j, suffix)] <- NA
			warning("Values of 'match' in 'obs' are out of range in 'pred'")
			next
		}
		# Interpolate
		for(j in vars)
			obs[[i]][,paste0(j, suffix)] <- with(pred[[i]],
					approx(get(match[1]), get(j), obs[[i]][,match[2]], ...)$y)
	}
	return(do.call(rbind, obs))
}

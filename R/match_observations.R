#' @title Match Observations with Predictions
#' 
#' @description 
#' Interpolation of predicted values for comparison with observed ones.
#' 
#' This function is using [approx()] for the interpolation of
#' predicted variables to observations. For the interpolation, the variables
#' indicated in `'match'` will be considered as time or progress in the
#' simulated dynamics (e.g. growth).
#' 
#' There are two assumptions that will not be tested by the function. On the
#' one side the predictions are sorted according to the variable `'match'`.
#' On the other side, the single values of the variable `'group'` represent
#' single runs in the simulation (i.e. single treatments in growth experiments).
#' 
#' For interpolation is carried out in a loop for single values in
#' \code{'group'}. At every run there is a check on occurrence of ties in the
#' variable \code{'match'} at the data frame \code{'pred'} and a check on
#' observations done within the simulated range. If those tests fail,
#' interpolated values will be \code{NA}s and a warning will be displayed for
#' the run.
#' 
#' Depending on the variable used for measuring the dynamic progress and
#' depending on its resolution, ties may not be avoidable. In such cases the
#' argument \code{'clean_ties'} can be set as \code{'clean_ties=TRUE'}, which
#' will delete all but the last of the tied values in \code{'pred'}.
#' 
#' @name match_observations
#' 
#' @param pred,obs Data frame containing predicted and observed variables,
#' respectively.
#' @param vars Character vector with the name of variables to be compared.
#' @param match Character value indicating the name of the variable used to
#' match observations with predictions. A vector of length 2 can be provided,
#' where the first value is the name of the variable in the data frame 'pred'
#' and the second is the name of the same variable in 'obs'.
#' @param group Character value indicating the name of the variable used for
#' grouping predictions and observations (i.e. simulation runs or treatments).
#' This argument can be also of length 2 as for 'match'.
#' @param suffix Character value used as suffix for predicted variables in
#' output.
#' @param clean_ties Logical value, whether ties in the 'match' variable should
#' be deleted from the data set with predictions or not.
#' @param ... Further arguments passed to the function \code{\link{approx}}.
#' 
#' @return A copy of the data frame \code{'obs'} including predicted values for
#' the requested variables.
#' 
#' @examples
#' ## No example at the moment
#' 
#' @export 
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

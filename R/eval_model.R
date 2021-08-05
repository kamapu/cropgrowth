#' @title Evaluating Models according to Residuals
#' 
#' @description 
#' Evaluation of models will be done comparing observed with simulated data.
#' Simulated data should be of a higher resolution than observed, while the
#' time of sampling may not match exactly the simulation steps. Additionally,
#' multiple observations (repetitions) may be compared with single simulations.
#' 
#' This function matches predictions and observations according to the variable
#' \code{'match'} within the samples belonging to the same group (i.e.
#' treatment).
#' 
#' No functions are included to contrast predictions with observations, thus
#' they have to be defined by the user. Those functions have to include two
#' arguments, namely \code{'x'} for predictions and \code{'y'} for
#' observations.
#' 
#' @param pred,obs Data frames containing predicted and observed variables,
#' respectively.
#' @param vars Character vector with the names of variables to be evaluated.
#' @param match,group Character values or vectors indicating the names of
#' variables used to match observations with predictions and to group
#' treatments (see also \code{\link{match_observations}}).
#' @param FUN List with functions used for the evaluation.
#' @param na.rm Logical value, whether NAs in predictions and observations have
#' to be skipped or not.
#' @param ... Further arguments passed to \code{\link{approx}} (see also
#' \code{\link{match_observations}}).
#' 
#' @return A data frame.
#' 
#' @references \bold{Janssen PHM, Heuberger PSC (1995).} Calibration of
#' process-oriented models. \emph{Ecological Modelling} 83: 55--66.
#' \url{https://doi.org/10.1016/0304-3800(95)00084-9}
#' 
#' \bold{Bennett ND et al. (2013).} Characterising performance of environmental
#' models. \emph{Environmental Modelling and Software} 40: 1--20.
#' \url{http://dx.doi.org/10.1016/j.envsoft.2012.09.011}
#' 
#' @examples
#' ## No example at the moment
#' 
#' @export 
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
			vars_fit[[i]][[j]] <- data.frame(group=i, var=j, do.call(cbind,
							vars_fit[[i]][[j]]), stringsAsFactors=FALSE)
		}
		vars_fit[[i]] <- do.call(rbind, vars_fit[[i]])
	}
	vars_fit <- do.call(rbind, vars_fit)
	rownames(vars_fit) <- NULL
	return(vars_fit)
}

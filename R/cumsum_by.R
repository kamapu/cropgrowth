#' @title Cumulative sums for database lists
#' 
#' @description 
#' Observations needing the calculation of a cumulative sum, for instance in
#' germination experiments, will require to be done by treatment, combining
#' identifiers (e.g. treatment and repetition).
#' 
#' In the formula method you set in the left term the numeric variable which
#' has to be acummulated. Note that you can insert in the left term multiple
#' variables separating them by a plus symbol (`'+'`).
#' On the right term you set the sorting variable (usually time, Date or
#' integer) as the first term followed by identifiers.
#' 
#' This function is checking the uniqueness of the temporal variable per
#' treatment.
#' For ties the function will retrieve an error message.
#' 
#' @param x A data frame containing or a formula, depending on the method. See
#'   the description above to design the formula.
#' @param data A data frame required in the formula method.
#' @param ids A character vector with the names of the variables used to
#'   identify every experimental object (usually the combination of treatment
#'   and repetition).
#' @param vars A character vector with the names of the variables used to
#'   calculate the cumulative sum.
#' @param by A character value used as temporal variable.
#' @param suffix A character value added to the name of the variable in the
#'   output data frame.
#' @param ... Further arguments passed among methods.
#' 
#' @return
#' The input data frame with the cumulative sums as additional variables.
#' Note that the sorting of rows may change in the process.
#' 
#' @name cumsum_by
#' @rdname cumsum_by
#' 
#' @export
cumsum_by <- function(x, ...) {
	UseMethod("cumsum_by", x)
}

#' @rdname cumsum_by
#' @aliases cumsum_by.data.frame
#' @export
cumsum_by.data.frame <- function(x, ids, vars, by, suffix = "_cumul", ...) {
	# sort table
	x <- x[order(x[ , by]), ]
	# check identities
	if(any(duplicated(do.call(paste0, x[ , c(ids, by)]))))
		stop(paste("The combination of 'ids' and 'by' is not unique in 'x'.",
						"Did you forget an identifier?"))
	# creates the IDs
	IDs <- do.call(paste0, x[ , ids, drop = FALSE])
	# calculate cumsum by group
	OUT <- list()
	for(i in unique(IDs)) {
		OUT[[i]] <- x[IDs == i, ]
		for(j in vars)
			OUT[[i]][ , paste0(j, suffix)] <- cumsum(OUT[[i]][ , j])
	}
	OUT <- do.call(rbind, OUT)
	invisible(OUT)
}

#' @rdname cumsum_by
#' @aliases cumsum_by.formula
#' @export 
cumsum_by.formula <- function(x, data, ...) {
	vars <- strsplit(as.character(x)[2], "+", fixed=TRUE)[[1]]
	vars <- gsub("\\s+", "", vars)
	ids <- attr(terms(x), "term.labels")
	by <- ids[1]
	ids <- ids[-1]
	cumsum_by(x = data, ids = ids, vars = vars, by = by, ...)
}

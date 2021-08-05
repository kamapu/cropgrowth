#' @title Piled Growth Curves
#' 
#' @description 
#' Objects containing growth curves representing partitions inside growing
#' units (organisms).
#' 
#' Objects can be created by \code{new("piled_curves")}, but usually they will
#' result from the function \emph{pile}.
#' 
#' The slot \code{'time'} can contain different formats or variables used as
#' time (e.g. date, day of the year, etc.).
#' 
#' @name piled_curves-class
#' @aliases piled_curves
#' 
#' @section Slots:
#' \describe{
#'   \item{time}{A data frame including time variables as columns.}
#'   \item{input}{A matrix including the state (e.g. biomass) of every partition
#'     as columns.}
#'   \item{piled}{The same state variables as in `'input'` but with accumulated
#'     values}
#'   \item{relative}{The relative partition of the state variable.}
#' }
#' 
#' @examples
#' showClass("piled_curves")
#' 
#' @exportClass piled_curves
setClass("piled_curves",
		# Definition of slots
		slots=c(
				time="data.frame",
				input="matrix",
				piled="matrix",
				relative="matrix"
		),
		# Prototype
		prototype=list(
				time=data.frame(),
				input=matrix(),
				piled=matrix(),
				relative=matrix()
		),
		# Validity procedures
		validity=function(object) {
			if(dim(object@input)[1] != nrow(object@time))
				return("Non matching dimension between slots 'time' and 'input'.")
			if(dim(object@piled)[1] != nrow(object@time))
				return("Non matching dimension between slots 'time' and 'piled'.")
			if(dim(object@relative)[1] != nrow(object@time))
				return("Non matching dimension between slots 'time' and 'relative'.")
			if(length(object@input) > 1 & !is.numeric(object@input))
				return("Slot 'input' have to be numeric.")
			if(length(object@piled) > 1 & !is.numeric(object@piled))
				return("Slot 'piled' have to be numeric.")
			if(length(object@relative) > 1 & !is.numeric(object@relative))
				return("Slot 'relative' have to be numeric.")
		}
)

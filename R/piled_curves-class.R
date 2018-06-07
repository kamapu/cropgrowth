# TODO:   Definition of class added curves
# 
# Author: Miguel Alvarez
################################################################################

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

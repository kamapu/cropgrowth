# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

function(pred, obs, na.rm=FALSE) {
	# First check classes and dimensions
	if(class(pred) != class(obs)) stop("pred and obs should be the same class")
	if(is.vector(pred) & is.vector(obs)) {
		pred <- matrix(pred, ncol=1)
		colnames(pred) <- "response"
		obs <- matrix(obs, ncol=1)
		colnames(obs) <- "response"
	}
	if(!all(dim(pred) == dim(obs)))
		stop("pred and obs should have the same dimensions")
	if(!all(colnames(pred) %in% colnames(obs)))
		stop("mismatchings between colnames of pred and obs")
	# Eliminate NAs
	if(na.rm == TRUE) use="complete.obs" else use="everything"
	# now calculate
	out <- matrix(nrow=dim(pred)[2], ncol=7)
	rownames(out) <- colnames(pred)
	colnames(out) <- c("MSE","r","MC","SC","RC","NRMSE","Pvalue")
	for(i in colnames(pred)) {
		out[i,"MSE"] <- MSE <- mean((pred[,i]-obs[,i])^2, na.rm=na.rm)
		out[i,"r"] <- r <- cor(pred[,i], obs[,i], method="pearson", use=use)
		out[i,"MC"] <- (mean(pred[,i], na.rm=na.rm)-
					mean(obs[,i], na.rm=na.rm))^2/MSE
		out[i,"SC"] <- (sd(pred[,i], na.rm=na.rm)-
					r*sd(obs[,i], na.rm=na.rm))^2/MSE
		out[i,"RC"] <- (1-r^2)*sd(obs[,i], na.rm=na.rm)^2/MSE
		out[i,"NRMSE"] <- sqrt(MSE)/mean(obs[,i], na.rm=na.rm)
		out[i,"Pvalue"] <- t.test(obs[,i], pred[,i], paired=TRUE,
				alternative="two.sided")$p.value
	}
	out <- as.data.frame(out)
	out$Significance <- "ns"
	out$Significance[out$Pvalue < 0.05] <- "*"
	out$Significance[out$Pvalue < 0.01] <- "**"
	return(out)
}

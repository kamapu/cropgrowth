# TODO:   Script to produce package step-by-step
# 
# Author: Miguel Alvarez
################################################################################

library(SWEApack)
library(RAtmosphere)
library(devtools)
install_github("kamapu/cropgrowth")
library(cropgrowth)

data(maize_model)
maize_model <- subset(maize_model, treatment == "T3")


setwd("M:/WorkspaceR/cropgrowth")

## data(maize_model)
## colnames(maize_model)[1:5] <- c("locality","crop_name","treatment",
##         "current_date","development_stage")
## colnames(maize_model) <- tolower(colnames(maize_model))
## for(i in c(1:4))
##     maize_model[,i] <- paste(maize_model[,i])
## maize_model$current_date <- as.Date(maize_model$current_date, "%d.%m.%Y")
## save(maize_model, file="M:/WorkspaceR/cropgrowth/data/maize_model.rda")






# TODO:   Message displayed on start (modified from vegdata)
# 
# Author: Miguel Alvarez
################################################################################

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is cropgrowth ",
            utils::packageDescription("cropgrowth", field="Version"),
            appendLF=TRUE)
}

#### Install and load required libraries ####

.requiredLibraries <- c(
  'dplyr',
  'magrittr',
  'quantmod',
  'binhf',
  'h2o',
  'ggplot2'
)

####################################################################
## All libraries will be installed if needed

.requireLib <- function(package) {
  if (!require(package, character.only=T)) {
    install.packages(package)
    return(require(package, character.only=T))
  }
  
  return(TRUE)
}

lapply(.requiredLibraries, .requireLib)
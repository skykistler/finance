### One time special install for blotter and quantstrat ###

# require(devtools)
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")

#### Install and load required libraries ####

.requiredLibraries <- c(
  'prophet',
  'dplyr',
  'magrittr',
  'foreach',
  'quantmod',
  'binhf',
  'h2o',
  'ggplot2',
  'plotly',
  'TTR',
  'PerformanceAnalytics',
  'FinancialInstrument',
  'tseries',
  'roll',
  'pracma',
  'blotter',
  'quantstrat',
  'RCurl',
  'rjson'
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
#### Install and load required libraries ####

.requiredLibraries <- c(
  'dplyr',
  'magrittr',
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
  'pracma'
)

install.packages("packages/blotter_0.9.1741.zip", repos="NULL", type="source")
install.packages("packages/quantstrat_0.9.1739.zip", repos="NULL", type="source")

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
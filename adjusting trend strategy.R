source('libraries.R')

if (!exists('GSPC'))
  getSymbols('^GSPC')

plotEntries <- function (symbol = GSPC, recent = 10, semirecent = 50, general=200, subset='::', column = 4) {
  prices <- symbol[,column]
  
  EMA.RECENT      <- EMA(prices, n=recent)
  EMA.SEMIRECENT  <- EMA(prices, n=semirecent)
  EMA.GENERAL     <- EMA(prices, n=general)
  
  Fast.Diff <- EMA.RECENT - EMA.SEMIRECENT
  Slow.Diff <- EMA.SEMIRECENT - EMA.GENERAL
  
  Long_Trades <- ifelse(
    Slow.Diff > 0 &
    Fast.Diff > 0 &
    shift(v = as.numeric(Fast.Diff), places = 1, dir = "right") < 0,
    prices, NA
  )
  
  Short_Trades <- ifelse(
    Slow.Diff < 0 &
    Fast.Diff < 0 &
    shift(v = as.numeric(Fast.Diff), places = 1, dir = "right") > 0,
    prices, NA
  )
  
  plot(prices[subset], main=deparse(substitute(symbol)))
  points(Long_Trades, col='blue', cex=1.5, pch=18)
  points(Short_Trades, col='red', cex=1.5, pch=18) 
}



plotEntries(GSPC)

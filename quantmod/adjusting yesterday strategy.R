source('libraries.R')

if (!exists('GSPC'))
  getSymbols('^GSPC')

yesterday <- function (prices) {
  shift(v = as.numeric(prices), places = 1, dir = "right") %>% return()
}

plotEntriesLag <- function (symbol = GSPC, recent = 10, semirecent = 50, general=200, subset='2015') {
  prices <- symbol[,4]
  
  EMA.RECENT      <- EMA(prices, n=recent)
  EMA.SEMIRECENT  <- EMA(prices, n=semirecent)
  EMA.GENERAL     <- EMA(prices, n=general)
  
  Fast.Diff <- EMA.RECENT - EMA.SEMIRECENT
  Slow.Diff <- EMA.SEMIRECENT - EMA.GENERAL
  
  Long_Trades <- ifelse(
    Slow.Diff > yesterday(Slow.Diff) &
    Fast.Diff > 0 &
    yesterday(Fast.Diff) < 0,
    prices, NA
  )
  
  Short_Trades <- ifelse(
    Slow.Diff < yesterday(Slow.Diff) &
    Fast.Diff < 0 &
    yesterday(Fast.Diff) > 0,
    prices, NA
  )
  
  plot(symbol[subset], main=deparse(substitute(symbol)))
  points(Long_Trades, col='blue', cex=1.5, pch=18)
  points(Short_Trades, col='red', cex=1.5, pch=18) 
}


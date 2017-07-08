source('libraries.R')

if (!exists('GSPC'))
  getSymbols('^GSPC')

plotMA <- function (symbol=GSPC, recent=10, semirecent=50, general=200, subset='::', column = 4) {
  prices <- symbol[,column]
  
  EMA.RECENT      <- EMA(prices, n=recent)
  EMA.SEMIRECENT  <- EMA(prices, n=semirecent)
  EMA.GENERAL     <- EMA(prices, n=general)
  
  ta.semirecent_ema <- paste('addEMA(', semirecent, ', col="blue");', sep='')
  ta.general_ema    <- paste('addEMA(', general   , ', col="red");', sep='')
  
  ta.chars <- paste(ta.semirecent_ema, ta.general_ema)
  
  chartSeries(prices[subset], 
              theme = "white",
              name  = deparse(substitute(symbol)),
              TA    = ta.chars
  )
  
  fast.ema.diff.label <- paste(recent, '-', semirecent,  ' EMA', sep='')
  slow.ema.diff.label <- paste(semirecent, '-', general, ' EMA', sep='')
  
  addTA(EMA.RECENT - EMA.SEMIRECENT, col='blue', type='h', legend=fast.ema.diff.label) %>% print
  addTA(EMA.SEMIRECENT - EMA.GENERAL, col='red', type='h', legend=slow.ema.diff.label) %>% print
}

plotMA(GSPC)

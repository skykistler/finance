source('libraries.R')

#### Predict and plot predictions for a given symbol ####
predictProphet <- function(symbol, from.date='2014-01-01', to.date=Sys.Date(), future.days=365, png.filename=NULL) {
  
  #### Pull ticker data from yahoo ####
  symbol.data <- getSymbols(symbol, src='yahoo', from=from.date, to=to.date, auto.assign=F)
  
  #### Normalize ticker data for data frame ####
  price  <- symbol.data[,paste0(symbol,'.Adjusted')]
  volume <- symbol.data[,paste0(symbol,'.Volume')]
  
  symbol.daily <- data.frame(price, volume, symbol.data %>% as.data.frame() %>% rownames)
  colnames(symbol.daily) <- c('price', 'volume', 'date')
  
  
  # symbol.daily %<>% rbind(
  #   data.frame(
  #     price=244,
  #     volume=4000000,
  #     date='2017-10-27'
  #   )
  # )
  
  symbol.daily$date %<>% as.Date()
  
  #### Engineer features ####
  symbol.daily %<>%
    mutate(
      # This is not trained on, see line 'characteristics <- ...'
      percent.change = (price / lag(price)) - 1,
      
      # Quick descriptions of price
      price.lag      = lag(price),
      price.lag.2    = lag(price, n=2),
      price.lag.3    = lag(price, n=3),
      price.lag.4    = lag(price, n=4),
      price.lag.5    = lag(price, n=5),
      
      # Volume is pretty noisy ;O
      volume.lag     = lag(volume),
      
      # These are base moving stats, using yesterday's price
      percent.change.lag = price.lag / (lag(price,2)) - 1,
      ema.short          = EMA(price.lag, n=3),
      ema.short.diff     = price.lag - ema.short,
      ema.medium         = EMA(price.lag, n=10),
      ema.medium.diff    = price.lag - ema.medium,
      ema.long           = EMA(price.lag, n=100),
      ema.long.diff      = price.lag - ema.long,
      evwma.medium       = EVWMA(price.lag, volume.lag, n=10),
      evwma.medium.diff  = volume.lag - evwma.medium,
      rsi.medium         = RSI(price.lag, n=12),
      
      # These are the above moving stats from 1 day ago
      ema.short.lag.1    = lag(ema.short.diff, n=1),
      ema.medium.lag.1   = lag(ema.medium.diff, n=1),
      evwma.medium.lag.1 = lag(evwma.medium.diff, n=1),
      rsi.medium.lag.1   = lag(rsi.medium, n=1),
      
      # These are the above moving stats from 2 days ago
      ema.short.lag.2    = lag(ema.short.diff, n=2),
      ema.medium.lag.2   = lag(ema.medium.diff, n=2),
      evwma.medium.lag.2 = lag(evwma.medium.diff, n=2),
      rsi.medium.lag.2   = lag(rsi.medium, n=2),
      
      # These are the above moving stats from 3 days ago
      ema.short.lag.3    = lag(ema.short.diff, n=3),
      ema.medium.lag.3   = lag(ema.medium.diff, n=3),
      evwma.medium.lag.3 = lag(evwma.medium.diff, n=3),
      rsi.medium.lag.3   = lag(rsi.medium, n=3),
      
      # These are the above moving stats from 4 days ago
      ema.short.lag.4    = lag(ema.short.diff, n=4),
      ema.medium.lag.4   = lag(ema.medium.diff, n=4),
      evwma.medium.lag.4 = lag(evwma.medium.diff, n=4),
      rsi.medium.lag.4   = lag(rsi.medium, n=4),
      
      # These are the above moving stats from 5 days ago
      ema.short.lag.5    = lag(ema.short.diff, n=5),
      ema.medium.lag.5   = lag(ema.medium.diff, n=5),
      evwma.medium.lag.5 = lag(evwma.medium.diff, n=5),
      rsi.medium.lag.5   = lag(rsi.medium, n=5)
    )
  
  characteristics <- colnames(symbol.daily)[-(4:1)]
  
  #### Prepare data for prophet ####
  symbol.daily %<>% filter(!is.na(evwma.medium.lag.5))
  
  symbol.prophet.df <- symbol.daily[,characteristics]
  symbol.prophet.df %<>%
    mutate(
      y  = symbol.daily$price,
      ds = symbol.daily$date
    )
  
  #### Run prophet ####p
  symbol.prophet <- prophet(symbol.prophet.df, daily.seasonality=TRUE, yearly.seasonality=TRUE)
  
  #### Make and plot predictions ####
  future <- make_future_dataframe(symbol.prophet, periods = future.days)
  forecast <- predict(symbol.prophet, future)
  
  if (!is.null(png.filename))
    png(filename=png.filename, width=1920, height=1080)
  
  plot(symbol.prophet, forecast) %>% print
  
  if (!is.null(png.filename))
    dev.off()
  
  forecast %>% 
    select(
      date = ds,
      price = yhat
    ) %>%
    View
}

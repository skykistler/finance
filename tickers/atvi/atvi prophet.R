source('libraries.R')

pred.date <- '2017-10-18'

getSymbols('ATVI', src='yahoo', from="2010-05-27", to=pred.date)

#####################################################

ATVI.daily <- data.frame(ATVI$ATVI.Adjusted, ATVI$ATVI.Volume, ATVI %>% as.data.frame() %>% rownames)
colnames(ATVI.daily) <- c('price', 'volume', 'date')

ATVI.daily$date %<>% as.Date()


ATVI.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date=pred.date)
)

ATVI.daily %<>%
  mutate(
    # This is not trained on, see line 'characteristics <- ...'
    percent.change= price / (lag(price) - 1) - 1,
    
    # Quick descriptions of price
    price.lag     = lag(price),
    price.lag.2   = lag(price, n=2),
    price.lag.3   = lag(price, n=3),
    price.lag.4   = lag(price, n=4),
    price.lag.5   = lag(price, n=5),
    
    # Volume is pretty noisy ;O
    volume.lag    = lag(volume),
    
    # These are base moving stats, using yesterday's price
    percent.change.lag = price.lag / (lag(price,2)) - 1,
    ema.20        = EMA(price.lag, n=3),
    ema.20.diff   = price.lag - ema.20,
    ema.50        = EMA(price.lag, n=10),
    ema.50.diff   = price.lag - ema.50,
    ema.100       = EMA(price.lag, n=100),
    ema.100.diff  = price.lag - ema.100,
    evwma.50      = EVWMA(price.lag, volume.lag, n=10),
    evwma.50.diff = volume.lag - evwma.50,
    
    # These are the above moving stats from 1 day ago
    ema.20.lag.1   = lag(ema.20.diff, n=1),
    ema.50.lag.1   = lag(ema.50.diff, n=1),
    evwma.50.lag.1 = lag(evwma.50.diff, n=1),
    
    # These are the above moving stats from 2 days ago
    ema.20.lag.2   = lag(ema.20.diff, n=2),
    ema.50.lag.2   = lag(ema.50.diff, n=2),
    evwma.50.lag.2 = lag(evwma.50.diff, n=2),
    
    # These are the above moving stats from 3 days ago
    ema.20.lag.3   = lag(ema.20.diff, n=3),
    ema.50.lag.3   = lag(ema.50.diff, n=3),
    evwma.50.lag.3 = lag(evwma.50.diff, n=3),
    
    # These are the above moving stats from 4 days ago
    ema.20.lag.4   = lag(ema.20.diff, n=4),
    ema.50.lag.4   = lag(ema.50.diff, n=4),
    evwma.50.lag.4 = lag(evwma.50.diff, n=4),
    
    # These are the above moving stats from 5 days ago
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(ATVI.daily)[-(4:1)]

ATVI.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

ATVI.prophet.df <- ATVI.daily[,characteristics]
ATVI.prophet.df %<>%
  mutate(
    y = ATVI.daily$price,
    ds = ATVI.daily$date
  )

ATVI.prophet <- prophet(ATVI.prophet.df, daily.seasonality=TRUE, yearly.seasonality=TRUE)

future <- make_future_dataframe(ATVI.prophet, periods = 365)

forecast <- predict(ATVI.prophet, future)
plot(ATVI.prophet, forecast) %>% print

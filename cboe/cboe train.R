source('libraries.R')

h2o.init(nthreads=4)

# getSymbols('CBOE', src='yahoo', from="2000-02-29", to="2017-07-14")

#####################################################

CBOE.daily <- data.frame(CBOE$CBOE.Adjusted, CBOE$CBOE.Volume, CBOE %>% as.data.frame() %>% rownames)
colnames(CBOE.daily) <- c('price', 'volume', 'date')
CBOE.daily$date %<>% as.Date()

# CBOE.daily %<>% rbind(
#   data.frame(price=92.14, volume=6e5, date='2017-07-14')
# )

CBOE.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-14')
)


CBOE.daily %<>%
  mutate(
    # These are not trained on, see line 'characteristics <- ...'
    volume.lag    = lag(volume),
    price.lag     = lag(price),
    percent.change= price / (price.lag - 1) - 1,
    
    # These are base moving stats, using yesterday's price
    percent.change.lag = price.lag / (lag(price,2)) - 1,
    ema.20        = EMA(price.lag, n=20),
    ema.20.diff   = price.lag - ema.20,
    ema.50        = EMA(price.lag),
    ema.50.diff   = price.lag - ema.50,
    evwma.50      = EVWMA(price.lag, volume.lag, n=50),
    evwma.50.diff = volume.lag - evwma.50,
    
    # These are the above moving stats from 2 days ago
    ema.20.lag.2   = lag(ema.20.diff, n=2),
    ema.50.lag.2   = lag(ema.50.diff, n=2),
    evwma.50.lag.2 = lag(evwma.50.diff, n=2),
    
    # These are the above moving stats from 5 days ago
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(CBOE.daily)[-(3:1)]

CBOE.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(CBOE.daily) - 20)

CBOE.daily.train <- CBOE.daily[training.subset, ]
CBOE.daily.test  <- CBOE.daily[-training.subset, ]

#####################################################

CBOE.daily.train.h2o <- as.h2o(CBOE.daily.train)
CBOE.daily.test.h2o  <- as.h2o(CBOE.daily.test)

CBOE.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = CBOE.daily.train.h2o,
    model_id = 'CBOE.daily.dnn',
    nfolds=20,
    activation = "RectifierWithDropout",
    hidden=c(200, 100, 20, 5, 50, 150),
    epochs=15,
    seed=1
  )

CBOE.daily.test$prediction <- h2o.predict(
  h2o.getModel('CBOE.daily.dnn'),
  CBOE.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


offset <- CBOE.daily.test[1,]$price / CBOE.daily.test[1,]$prediction

ggplot(CBOE.daily.test, aes(x=CBOE.daily.test$date)) +                    
  geom_line(aes(y=CBOE.daily.test$prediction * offset), colour="red", size=1) +  
  geom_line(aes(y=CBOE.daily.test$price), colour="green", size=1) +
  scale_x_date(
    minor_breaks = seq(
      from = min(CBOE.daily.test$date), 
      to   = max(CBOE.daily.test$date),
      by   = "1 day"
    ),
    date_breaks = "5 days"
  ) +
  scale_y_sqrt() %>% 
  print




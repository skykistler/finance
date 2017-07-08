source('libraries.R')

h2o.init(nthreads=2)

getSymbols('SPY', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

SPY.daily <- data.frame(SPY$SPY.Adjusted, SPY$SPY.Volume, SPY %>% as.data.frame() %>% rownames)
colnames(SPY.daily) <- c('price', 'volume', 'date')

SPY.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

SPY.daily %<>%
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
    
    # These are the above moving stats from 5 days ago
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(SPY.daily)[-(6:1)]

SPY.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(SPY.daily) - 2400)

SPY.daily.train <- SPY.daily[training.subset, ]
SPY.daily.test  <- SPY.daily[-training.subset, ]

#####################################################

SPY.daily.train.h2o <- as.h2o(SPY.daily.train)
SPY.daily.test.h2o  <- as.h2o(SPY.daily.test)

SPY.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = SPY.daily.train.h2o,
    validation_frame = SPY.daily.test.h2o,
    model_id = 'SPY.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

SPY.daily.test$prediction <- h2o.predict(
  h2o.getModel('SPY.daily.dnn'),
  SPY.daily.test.h2o
) %>% as.vector('numeric')


ggplot(SPY.daily.test, aes(x=SPY.daily.test$date%>%as.Date)) +                    
  geom_line(aes(y=SPY.daily.test$prediction), colour="red") +  
  geom_line(aes(y=SPY.daily.test$price), colour="green") +
  scale_x_date() +
  scale_y_sqrt()























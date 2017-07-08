source('libraries.R')

h2o.init(nthreads=2)

getSymbols('TSLA', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

TSLA.daily <- data.frame(TSLA$TSLA.Adjusted, TSLA$TSLA.Volume, TSLA %>% as.data.frame() %>% rownames)
colnames(TSLA.daily) <- c('price', 'volume', 'date')

TSLA.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

TSLA.daily %<>%
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

characteristics <- colnames(TSLA.daily)[-(6:1)]

TSLA.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(TSLA.daily) - 1000)

TSLA.daily.train <- TSLA.daily[training.subset, ]
TSLA.daily.test  <- TSLA.daily[-training.subset, ]

#####################################################

TSLA.daily.train.h2o <- as.h2o(TSLA.daily.train)
TSLA.daily.test.h2o  <- as.h2o(TSLA.daily.test)

TSLA.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = TSLA.daily.train.h2o,
    validation_frame = TSLA.daily.test.h2o,
    model_id = 'TSLA.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

TSLA.daily.test$prediction <- h2o.predict(
  h2o.getModel('TSLA.daily.dnn'),
  TSLA.daily.test.h2o
) %>% as.vector('numeric')


ggplot(TSLA.daily.test, aes(x=TSLA.daily.test$date%>%as.Date)) +                    
  geom_line(aes(y=TSLA.daily.test$prediction), colour="red") +  
  geom_line(aes(y=TSLA.daily.test$price), colour="green") +
  scale_x_date() +
  scale_y_sqrt()























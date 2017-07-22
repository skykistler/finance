source('libraries.R')

h2o.init(nthreads=2)

getSymbols('GOOG', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

GOOG.daily <- data.frame(GOOG$GOOG.Adjusted, GOOG$GOOG.Volume, GOOG %>% as.data.frame() %>% rownames)
colnames(GOOG.daily) <- c('price', 'volume', 'date')

GOOG.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

GOOG.daily %<>%
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

characteristics <- colnames(GOOG.daily)[-(6:1)]

GOOG.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(GOOG.daily) - 1400)

GOOG.daily.train <- GOOG.daily[training.subset, ]
GOOG.daily.test  <- GOOG.daily[-training.subset, ]

#####################################################

GOOG.daily.train.h2o <- as.h2o(GOOG.daily.train)
GOOG.daily.test.h2o  <- as.h2o(GOOG.daily.test)

GOOG.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = GOOG.daily.train.h2o,
    validation_frame = GOOG.daily.test.h2o,
    model_id = 'GOOG.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

GOOG.daily.test$prediction <- h2o.predict(
  h2o.getModel('GOOG.daily.dnn'),
  GOOG.daily.test.h2o
) %>% as.vector('numeric')


ggplot(GOOG.daily.test, aes(x=GOOG.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=GOOG.daily.test$prediction), colour="red") +  
  geom_line(aes(y=GOOG.daily.test$price), colour="green") +
  scale_y_sqrt()























source('libraries.R')

h2o.init(nthreads=2)

getSymbols('AMD', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

AMD.daily <- data.frame(AMD$AMD.Adjusted, AMD$AMD.Volume, AMD %>% as.data.frame() %>% rownames)
colnames(AMD.daily) <- c('price', 'volume', 'date')

AMD.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

AMD.daily %<>%
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

characteristics <- colnames(AMD.daily)[-(5:1)]

AMD.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(AMD.daily) - 2400)

AMD.daily.train <- AMD.daily[training.subset, ]
AMD.daily.test  <- AMD.daily[-training.subset, ]

#####################################################

AMD.daily.train.h2o <- as.h2o(AMD.daily.train)
AMD.daily.test.h2o  <- as.h2o(AMD.daily.test)

AMD.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = AMD.daily.train.h2o,
    validation_frame = AMD.daily.test.h2o,
    model_id = 'AMD.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

AMD.daily.test$prediction <- h2o.predict(
  h2o.getModel('AMD.daily.dnn'),
  AMD.daily.test.h2o
) %>% as.vector('numeric')


ggplot(AMD.daily.test, aes(x=AMD.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=AMD.daily.test$prediction), colour="red") +  
  geom_line(aes(y=AMD.daily.test$price), colour="green") +
  scale_y_sqrt()























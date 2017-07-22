source('libraries.R')

h2o.init(nthreads=2)

getSymbols('UAL', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

UAL.daily <- data.frame(UAL$UAL.Adjusted, UAL$UAL.Volume, UAL %>% as.data.frame() %>% rownames)
colnames(UAL.daily) <- c('price', 'volume', 'date')

UAL.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

UAL.daily %<>%
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

characteristics <- colnames(UAL.daily)[-(6:1)]

UAL.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(UAL.daily) - 2100)

UAL.daily.train <- UAL.daily[training.subset, ]
UAL.daily.test  <- UAL.daily[-training.subset, ]

#####################################################

UAL.daily.train.h2o <- as.h2o(UAL.daily.train)
UAL.daily.test.h2o  <- as.h2o(UAL.daily.test)

UAL.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = UAL.daily.train.h2o,
    validation_frame = UAL.daily.test.h2o,
    model_id = 'UAL.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

UAL.daily.test$prediction <- h2o.predict(
  h2o.getModel('UAL.daily.dnn'),
  UAL.daily.test.h2o
) %>% as.vector('numeric')


ggplot(UAL.daily.test, aes(x=UAL.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=UAL.daily.test$prediction), colour="red") +  
  geom_line(aes(y=UAL.daily.test$price), colour="green") +
  scale_y_sqrt()























source('libraries.R')

h2o.init(nthreads=4)

getSymbols('MU', src='yahoo', from="2004-01-01", to="2017-07-11")

#####################################################

MU.daily <- data.frame(MU$MU.Adjusted, MU$MU.Volume, MU %>% as.data.frame() %>% rownames)
colnames(MU.daily) <- c('price', 'volume', 'date')


MU.daily %<>% rbind(
  data.frame(price=31.08, volume=2.1e7, date='2017-07-11')
)

 
# MU.daily %<>% rbind(
#   data.frame(price=30.47, volume=3.0e7, date='2017-07-12')
# )


MU.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-12')
)

MU.daily %<>%
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

characteristics <- colnames(MU.daily)[-(6:1)]

MU.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(MU.daily) - 10)

MU.daily.train <- MU.daily[training.subset, ]
MU.daily.test  <- MU.daily[-training.subset, ]

#####################################################

MU.daily.train.h2o <- as.h2o(MU.daily.train)
MU.daily.test.h2o  <- as.h2o(MU.daily.test)

MU.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = MU.daily.train.h2o,
    validation_frame = MU.daily.test.h2o,
    model_id = 'MU.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

MU.daily.test$prediction <- h2o.predict(
  h2o.getModel('MU.daily.dnn'),
  MU.daily.test.h2o
  ) %>% as.vector('numeric')


ggplot(MU.daily.test, aes(x=MU.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=MU.daily.test$prediction), colour="red") +  
  geom_line(aes(y=MU.daily.test$price), colour="green") +
  scale_y_sqrt() %>% 
  print























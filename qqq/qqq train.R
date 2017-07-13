source('libraries.R')

h2o.init(nthreads=4)

getSymbols('QQQ', src='yahoo', from="2004-01-01", to="2017-07-12")

#####################################################

QQQ.daily <- data.frame(QQQ$QQQ.Adjusted, QQQ$QQQ.Volume, QQQ %>% as.data.frame() %>% rownames)
colnames(QQQ.daily) <- c('price', 'volume', 'date')

QQQ.daily$date %<>% as.Date()


# QQQ.daily %<>% rbind(
#   data.frame(price=138.6, volume=2.48e7, date='2017-07-10')
# )


# QQQ.daily %<>% rbind(
#   data.frame(price=30.47, volume=3.0e7, date='2017-07-12')
# )


QQQ.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-12')
)

QQQ.daily %<>%
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

characteristics <- colnames(QQQ.daily)[-(4:1)]

QQQ.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(QQQ.daily) - 30)

QQQ.daily.train <- QQQ.daily[training.subset, ]
QQQ.daily.test  <- QQQ.daily[-training.subset, ]

#####################################################

QQQ.daily.train.h2o <- as.h2o(QQQ.daily.train)
QQQ.daily.test.h2o  <- as.h2o(QQQ.daily.test)

QQQ.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = QQQ.daily.train.h2o,
    validation_frame = QQQ.daily.test.h2o,
    model_id = 'QQQ.daily.dnn',
    nfolds=7,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

QQQ.daily.test$prediction <- h2o.predict(
  h2o.getModel('QQQ.daily.dnn'),
  QQQ.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


offset <- QQQ.daily.test[1,]$price / QQQ.daily.test[1,]$prediction

ggplot(QQQ.daily.test, aes(x=QQQ.daily.test$date)) +                    
  geom_line(aes(y=QQQ.daily.test$prediction * offset), colour="red", size=1) +  
  geom_line(aes(y=QQQ.daily.test$price), colour="green", size=1) +
  scale_x_date(
    minor_breaks = seq(
      from = min(QQQ.daily.test$date), 
      to   = max(QQQ.daily.test$date),
      by   = "1 day"
    ),
    date_breaks = "5 days"
  ) +
  scale_y_sqrt() %>% 
  print























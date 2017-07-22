source('libraries.R')

h2o.init(nthreads=4)

getSymbols('CHK', src='yahoo', from="2016-02-29", to="2017-07-14")

#####################################################

CHK.daily <- data.frame(CHK$CHK.Adjusted, CHK$CHK.Volume, CHK %>% as.data.frame() %>% rownames)
colnames(CHK.daily) <- c('price', 'volume', 'date')
CHK.daily$date %<>% as.Date()


CHK.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-14')
)


CHK.daily %<>%
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

characteristics <- colnames(CHK.daily)[-(3:1)]

CHK.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(CHK.daily) - 200)

CHK.daily.train <- CHK.daily[training.subset, ]
CHK.daily.test  <- CHK.daily[-training.subset, ]

#####################################################

CHK.daily.train.h2o <- as.h2o(CHK.daily.train)
CHK.daily.test.h2o  <- as.h2o(CHK.daily.test)

CHK.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = CHK.daily.train.h2o,
    model_id = 'CHK.daily.dnn',
    nfolds=20,
    activation = "RectifierWithDropout",
    hidden=c(200, 100, 20, 5, 50, 150),
    epochs=15,
    seed=1
  )

CHK.daily.test$prediction <- h2o.predict(
  h2o.getModel('CHK.daily.dnn'),
  CHK.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


offset <- CHK.daily.test[1,]$price / CHK.daily.test[1,]$prediction

ggplot(CHK.daily.test, aes(x=CHK.daily.test$date)) +                    
  geom_line(aes(y=CHK.daily.test$prediction * offset), colour="red", size=1) +  
  geom_line(aes(y=CHK.daily.test$price), colour="green", size=1) +
  scale_x_date(
    minor_breaks = seq(
      from = min(CHK.daily.test$date), 
      to   = max(CHK.daily.test$date),
      by   = "1 day"
    ),
    date_breaks = "5 days"
  ) +
  scale_y_sqrt() %>% 
  print























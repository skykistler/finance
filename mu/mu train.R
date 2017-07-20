source('libraries.R')

h2o.init(nthreads=4)

pred.date <- '2017-07-20'

getSymbols('MU', src='yahoo', from="2004-01-01", to=pred.date)

#####################################################

MU.daily <- data.frame(MU$MU.Adjusted, MU$MU.Volume, MU %>% as.data.frame() %>% rownames)
colnames(MU.daily) <- c('price', 'volume', 'date')

MU.daily$date %<>% as.Date()


MU.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date=pred.date)
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
    
    # These are the above moving stats from 2 days ago
    ema.20.lag.2   = lag(ema.20.diff, n=2),
    ema.50.lag.2   = lag(ema.50.diff, n=2),
    evwma.50.lag.2 = lag(evwma.50.diff, n=2),
    
    # These are the above moving stats from 5 days ago
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(MU.daily)[-(4:1)]

MU.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(MU.daily) - 40)

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
    model_id = 'MU.daily.dnn',
    nfolds=7,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

MU.daily.test$prediction <- h2o.predict(
  h2o.getModel('MU.daily.dnn'),
  MU.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


offset <- MU.daily.test[1,]$price / MU.daily.test[1,]$prediction

ggplot(MU.daily.test, aes(x=MU.daily.test$date)) +                    
  geom_line(aes(y=MU.daily.test$prediction * offset), colour="red", size=1) +  
  geom_line(aes(y=MU.daily.test$price), colour="green", size=1) +
  scale_x_date(
    minor_breaks = seq(
      from = min(MU.daily.test$date), 
      to   = max(MU.daily.test$date),
      by   = "1 day"
    ),
    date_breaks = "5 days"
  ) +
  scale_y_sqrt() %>% 
  print





print(paste('True downs (both went down):', MU.daily.test %>% filter(prediction < lag(prediction), price < price.lag) %>% nrow)) 
print(paste('True ups (both went up):', MU.daily.test %>% filter(prediction > lag(prediction), price > price.lag) %>% nrow)) 

print(paste('False downs (actually went up):', MU.daily.test %>% filter(prediction < lag(prediction), price > price.lag) %>% nrow)) 
print(paste('False ups (actually went down):', MU.daily.test %>% filter(prediction > lag(prediction), price < price.lag) %>% nrow)) 













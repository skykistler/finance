source('libraries.R')

h2o.init(nthreads=4)

pred.date <- '2017-07-21'

getSymbols('OZRK', src='yahoo', from="2004-01-01", to=pred.date)

#####################################################

OZRK.daily <- data.frame(OZRK$OZRK.Adjusted, OZRK$OZRK.Volume, OZRK %>% as.data.frame() %>% rownames)
colnames(OZRK.daily) <- c('price', 'volume', 'date')

OZRK.daily$date %<>% as.Date()


OZRK.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date=pred.date)
)

OZRK.daily %<>%
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
    
    # These are the above moving stats from 1 day ago
    ema.20.lag.1   = lag(ema.20.diff, n=1),
    ema.50.lag.1   = lag(ema.50.diff, n=1),
    evwma.50.lag.1 = lag(evwma.50.diff, n=1),
    
    # These are the above moving stats from 2 days ago
    ema.20.lag.2   = lag(ema.20.diff, n=2),
    ema.50.lag.2   = lag(ema.50.diff, n=2),
    evwma.50.lag.2 = lag(evwma.50.diff, n=2),
    
    # These are the above moving stats from 3 days ago
    ema.20.lag.3   = lag(ema.20.diff, n=3),
    ema.50.lag.3   = lag(ema.50.diff, n=3),
    evwma.50.lag.3 = lag(evwma.50.diff, n=3),
    
    # These are the above moving stats from 4 days ago
    ema.20.lag.4   = lag(ema.20.diff, n=4),
    ema.50.lag.4   = lag(ema.50.diff, n=4),
    evwma.50.lag.4 = lag(evwma.50.diff, n=4),
    
    # These are the above moving stats from 5 days ago
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(OZRK.daily)[-(4:1)]

OZRK.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(OZRK.daily) - 20)

OZRK.daily.train <- OZRK.daily[training.subset, ]
OZRK.daily.test  <- OZRK.daily[-training.subset, ]

#####################################################

OZRK.daily.train.h2o <- as.h2o(OZRK.daily.train)
OZRK.daily.test.h2o  <- as.h2o(OZRK.daily.test)

OZRK.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = OZRK.daily.train.h2o,
    model_id = 'OZRK.daily.dnn',
    nfolds=7,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

OZRK.daily.test$prediction <- h2o.predict(
  h2o.getModel('OZRK.daily.dnn'),
  OZRK.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


offset <- OZRK.daily.test[1,]$price / OZRK.daily.test[1,]$prediction

ggplot(OZRK.daily.test, aes(x=OZRK.daily.test$date)) +                    
  geom_line(aes(y=OZRK.daily.test$prediction * offset), colour="red", size=1) +  
  geom_line(aes(y=OZRK.daily.test$price), colour="green", size=1) +
  scale_x_date(
    minor_breaks = seq(
      from = min(OZRK.daily.test$date), 
      to   = max(OZRK.daily.test$date),
      by   = "1 day"
    ),
    date_breaks = "5 days"
  ) +
  scale_y_sqrt() %>% 
  print




printPerformance <- function () {
  print(paste('True downs (both went down):', OZRK.daily.test %>% filter(prediction < lag(prediction), price < price.lag) %>% nrow)) 
  print(paste('True ups (both went up):', OZRK.daily.test %>% filter(prediction > lag(prediction), price > price.lag) %>% nrow)) 
  
  print(paste('False downs (actually went up):', OZRK.daily.test %>% filter(prediction < lag(prediction), price > price.lag) %>% nrow)) 
  print(paste('False ups (actually went down):', OZRK.daily.test %>% filter(prediction > lag(prediction), price < price.lag) %>% nrow)) 
}

printPerformance()









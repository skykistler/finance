source('libraries.R')

h2o.init(nthreads=4)

pred.date <- '2017-09-01'

getSymbols('BABA', src='yahoo', from="2013-05-27", to=pred.date)

chartSeries(BABA, type="matchsticks", theme='black', subset='::', TA='
            addEMA(20, col="blue");
            addEMA(50, col="yellow");
            addEVWMA(50, col="pink");
            addEMA(200, col="red");')


#####################################################

BABA.daily <- data.frame(BABA$BABA.Adjusted, BABA$BABA.Volume, BABA %>% as.data.frame() %>% rownames)
colnames(BABA.daily) <- c('price', 'volume', 'date')

BABA.daily$date %<>% as.Date()


BABA.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date=pred.date)
)

BABA.daily %<>%
  mutate(
    # This is not trained on, see line 'characteristics <- ...'
    percent.change= price / (lag(price) - 1) - 1,
    
    # Quick descriptions of price
    price.lag     = lag(price),
    price.lag.2   = lag(price, n=2),
    price.lag.3   = lag(price, n=3),
    price.lag.4   = lag(price, n=4),
    price.lag.5   = lag(price, n=5),
    
    # Volume is pretty noisy ;O
    volume.lag    = lag(volume),
    
    # These are base moving stats, using yesterday's price
    percent.change.lag = price.lag / (lag(price,2)) - 1,
    ema.20        = EMA(price.lag, n=3),
    ema.20.diff   = price.lag - ema.20,
    ema.50        = EMA(price.lag, n=10),
    ema.50.diff   = price.lag - ema.50,
    ema.100       = EMA(price.lag, n=100),
    ema.100.diff  = price.lag - ema.100,
    evwma.50      = EVWMA(price.lag, volume.lag, n=10),
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

characteristics <- colnames(BABA.daily)[-(4:1)]

BABA.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(BABA.daily) - 9n0)

BABA.daily.train <- BABA.daily[training.subset, ]
BABA.daily.test  <- BABA.daily[-training.subset, ]

#####################################################

BABA.daily.train.h2o <- as.h2o(BABA.daily.train)
BABA.daily.test.h2o  <- as.h2o(BABA.daily.test[,characteristics])

BABA.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = BABA.daily.train.h2o,
    model_id = 'BABA.daily.dnn',
    nfolds=7,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200,200,200),
    epochs=14,
    seed=-1
  )

BABA.daily.test$prediction <- h2o.predict(
  h2o.getModel('BABA.daily.dnn'),
  BABA.daily.test.h2o
) %>% as.vector('numeric')



##################################################################### 


plotPredictions <- function() {
  offset <- 1#BABA.daily.test[1,]$price / BABA.daily.test[1,]$prediction
  
  ggplot(BABA.daily.test, aes(x=BABA.daily.test$date)) +                    
    geom_line(aes(y=BABA.daily.test$prediction * offset), colour="red", size=1) +  
    geom_line(aes(y=BABA.daily.test$price), colour="green", size=1) +
    scale_x_date(
      minor_breaks = seq(
        from = min(BABA.daily.test$date), 
        to   = max(BABA.daily.test$date),
        by   = "1 day"
      ),
      date_breaks = "5 days"
    ) +
    scale_y_sqrt() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly()
}

plotPredictions()

printPerformance <- function () {
  print(paste('True downs (both went down):', BABA.daily.test %>% filter(prediction < lag(prediction), price < price.lag) %>% nrow)) 
  print(paste('True ups (both went up):', BABA.daily.test %>% filter(prediction > lag(prediction), price > price.lag) %>% nrow)) 
  
  print(paste('False downs (actually went up):', BABA.daily.test %>% filter(prediction < lag(prediction), price > price.lag) %>% nrow)) 
  print(paste('False ups (actually went down):', BABA.daily.test %>% filter(prediction > lag(prediction), price < price.lag) %>% nrow)) 
  
  BABA.daily.test %>% mutate(prediction.lag = lag(prediction)) %>% select(date, prediction.lag, prediction, price.lag, price) %>% View
}

printPerformance()









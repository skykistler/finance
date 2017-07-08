source('libraries.R')

h2o.init(nthreads=2)

getSymbols('NVDA', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

NVDA.daily <- data.frame(NVDA$NVDA.Adjusted, NVDA$NVDA.Volume, NVDA %>% as.data.frame() %>% rownames)
colnames(NVDA.daily) <- c('price', 'volume', 'date')

NVDA.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

NVDA.daily %<>%
  mutate(
    volume.lag    = lag(volume),
    price.lag     = lag(price),
    ema.20        = EMA(price.lag, n=20),
    ema.20.diff   = price.lag - ema.20,
    ema.50        = EMA(price.lag),
    ema.50.diff   = price.lag - ema.50,
    evwma.50      = EVWMA(price.lag, volume.lag, n=50),
    evwma.50.diff = volume.lag - evwma.50,
    
    ema.20.lag.5   = lag(ema.20.diff, n=5),
    ema.50.lag.5   = lag(ema.50.diff, n=5),
    evwma.50.lag.5 = lag(evwma.50.diff, n=5)
  )

characteristics <- colnames(NVDA.daily)[-(5:1)]

NVDA.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(NVDA.daily) - 200)

NVDA.daily.train <- NVDA.daily[training.subset, ]
NVDA.daily.test  <- NVDA.daily[-training.subset, ]

#####################################################

NVDA.daily.train.h2o <- as.h2o(NVDA.daily.train)
NVDA.daily.test.h2o  <- as.h2o(NVDA.daily.test)

NVDA.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = NVDA.daily.train.h2o,
    validation_frame = NVDA.daily.test.h2o,
    model_id = 'NVDA.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

NVDA.daily.test$prediction <- h2o.predict(
  h2o.getModel('NVDA.daily.dnn'),
  NVDA.daily.test.h2o
) %>% as.vector('numeric')


ggplot(NVDA.daily.test, aes(x=NVDA.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=NVDA.daily.test$prediction + 1.7), colour="red") +  
  geom_line(aes(y=NVDA.daily.test$price), colour="green") +
  scale_y_sqrt()























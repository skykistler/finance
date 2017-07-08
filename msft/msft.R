source('libraries.R')

h2o.init(nthreads=2)

getSymbols('MSFT', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

MSFT.daily <- data.frame(MSFT$MSFT.Adjusted, MSFT$MSFT.Volume, MSFT %>% as.data.frame() %>% rownames)
colnames(MSFT.daily) <- c('price', 'volume', 'date')

MSFT.daily %<>% rbind(
  data.frame(price=NA, volume=NA, date='2017-07-10')
)

MSFT.daily %<>%
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

characteristics <- colnames(MSFT.daily)[-(5:1)]

MSFT.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(MSFT.daily) - 200)

MSFT.daily.train <- MSFT.daily[training.subset, ]
MSFT.daily.test  <- MSFT.daily[-training.subset, ]

#####################################################

MSFT.daily.train.h2o <- as.h2o(MSFT.daily.train)
MSFT.daily.test.h2o  <- as.h2o(MSFT.daily.test)

MSFT.daily.dnn <-
  h2o.deeplearning(
    x=characteristics,
    y='price',
    training_frame = MSFT.daily.train.h2o,
    validation_frame = MSFT.daily.test.h2o,
    model_id = 'MSFT.daily.dnn',
    nfolds=3,
    activation = "RectifierWithDropout",
    hidden=c(200,200,200),
    epochs=15,
    seed=-1
  )

MSFT.daily.test$prediction <- h2o.predict(
  h2o.getModel('MSFT.daily.dnn'),
  MSFT.daily.test.h2o
) %>% as.vector('numeric')


ggplot(MSFT.daily.test, aes(x=MSFT.daily.test$date%>%as.numeric)) +                    
  geom_line(aes(y=MSFT.daily.test$prediction + 1.7), colour="red") +  
  geom_line(aes(y=MSFT.daily.test$price), colour="green") +
  scale_y_sqrt()























source('libraries.R')

h2o.init(nthreads=2)

getSymbols('MU', src='yahoo', from="2004-01-01", to="2017-07-08")

#####################################################

MU.daily <- data.frame(MU$MU.Adjusted, MU$MU.Volume)
colnames(MU.daily) <- c('price', 'volume')

MU.daily %<>%
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

characteristics <- colnames(MU.daily)[-(2:1)]

MU.daily %<>% filter(!is.na(evwma.50.lag.5))

#####################################################

training.subset <- 1:(nrow(MU.daily) - 50)

MU.daily.train <- MU.daily[training.subset, ]
MU.daily.test  <- MU.daily[-training.subset, ]

#####################################################

MU.daily.train.h2o <- as.h2o(MU.daily.train)
MU.daily.test.h2o  <- as.h2o(MU.daily.test)

# MU.gbm <-
#   h2o.gbm(
#     'price', 
#     characteristics,
#     MU.daily.train.h2o,
#     'MU.gbm',
#     ntrees=50,
#     max_depth=5,
#     min_rows=2,
#     learn_rate=0.2,
#     learn_rate_annealing = .99,
#     score_tree_interval = 5,
#     stopping_tolerance = .001,
#     stopping_rounds=5
#   )

predictions <- h2o.predict(
  h2o.getModel('MU.daily.dnn'), 
  MU.daily.test.h2o
  )

MU.daily.test$prediction <- predictions$predict 
MU.daily.test$prediction %>% length

# plot(MU.daily.test$price, MU.daily.test$prediction)














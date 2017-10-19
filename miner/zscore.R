source('libraries.R')
h2o.init(nthreads=4)

# Configure dates and size of portfolio
initial.portfolio.date <- '2003-06-30'
start.date             <- '2003-07-01'
end.date               <- '2017-08-18'
ticker                 <- 'MU'

sell.threshold <- .01
stop.threshold <- -.005

validation.days <- 20 * 5 # 20 weeks-ish
test.days       <- 32 * 5 # 32 weeks-ish


# Apparently timezone needs to be UTC
Sys.setenv(TZ='UTC')

# Get prices
getSymbols(ticker, from=start.date, to=end.date, index.class='POSIXct', adjust=T, auto.assign = T)
symbol <- get(ticker)
colnames(symbol) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# train classifier to recognize "negative", "neutral", "positive" (within n-days) 
# using x-days of zcore

zscore.fun <- function(x,n) {roll_scale(x, width=n)}
zscore     <- zscore.fun(diff(log(symbol$Close), lag=1), n=24)

symbol %<>%
  as.data.frame() %>%
  mutate(
    price    = Close,
    drawdown = Low,
    
    price.future.1 = lead(price, 1),
    price.future.2 = lead(price, 2),
    price.future.3 = lead(price, 3),
    
    cent.future.1  = price.future.1 / price - 1,
    cent.future.2  = price.future.2 / price - 1,
    cent.future.3  = price.future.3 / price - 1,
    
    state   = ifelse(cent.future.1 > sell.threshold | cent.future.2 > sell.threshold | cent.future.3 > sell.threshold, "positive", "negative"),
    state   = factor(state),
    
    zscore  = zscore %>% as.numeric
  )

symbol.zscores <- foreach(i=1:100, .combine=cbind) %do%
  lag(symbol[,'zscore'], n=i)

symbol %<>% cbind(symbol.zscores)

# Define characteristics
characteristics <- colnames(symbol)[-(16:1)]

# Remove NA's
symbol %<>% filter(!is.na(result.100))

# Split data
training.subset <- 1:(nrow(symbol) - (validation.days + test.days))
symbol.train    <- symbol[ training.subset, ]
symbol.nontrain <- symbol[-training.subset, ]

validation.subset <- 1:(nrow(symbol.nontrain) - (test.days))
symbol.validation <- symbol.nontrain[ validation.subset, ]
symbol.test       <- symbol.nontrain[-validation.subset, ]

symbol.train.h2o      <- as.h2o(symbol.train)
symbol.validation.h2o <- as.h2o(symbol.validation)
symbol.test.h2o       <- as.h2o(symbol.test)

# Train nn
# symbol.daily.dnn <-
#   h2o.deeplearning(
#     model_id = 'symbol.daily.dnn',
#     training_frame = symbol.train.h2o,
#     validation_frame = symbol.validation.h2o,
#     x=characteristics,
#     y='state',
#     nfolds=12,
#     activation = "RectifierWithDropout",
#     balance_classes = T,
#     hidden=c(100, 100, 100, 100),
#     epochs=14,
#     seed=-1
#   )

symbol.daily.gbm <-
  h2o.gbm(
    model_id = 'symbol.daily.gbm',
    training_frame = symbol.train.h2o,
    validation_frame = symbol.validation.h2o,
    x = characteristics,
    y = 'state',
    nfolds = 20,
    score_tree_interval = 1,
    balance_classes = T,
    ntrees = 10,
    max_depth = 18,
    learn_rate = .02,
    learn_rate_annealing = .99,
    min_rows = 20
  )

# Make predictions
symbol.test$prediction <- h2o.predict(
  h2o.getModel('symbol.daily.gbm'),
  symbol.test.h2o
)$positive %>% as.vector('numeric')

View(symbol.test %>% select (price,state,prediction))

#####################################################

printPerformance <- function (threshold = .541) {
  print(paste('True downs (both went down):', symbol.test %>% filter(state == 'negative', prediction < threshold) %>% nrow))
  print(paste('False downs (actually went up):', symbol.test %>% filter(state == 'positive', prediction < threshold) %>% nrow))

  print(paste('True ups (both went up):',     symbol.test %>% filter(state == 'positive', prediction >= threshold) %>% nrow))
  print(paste('False ups (actually went down):', symbol.test %>% filter(state == 'negative', prediction >= threshold) %>% nrow))
}

printPerformance()





source('libraries.R')


if (!exists('QQQ'))
  QQQ <- getSymbols('QQQ', auto.assign=F)


plot(QQQ$QQQ.Close)

period <- 100 
price_vector <- QQQ$QQQ.Close

moving_average <- sapply(
  (period + 1):length(price_vector), 
  function(index) {
    mean(price_vector[index:(index-period)]) %>% return
})

QQQ$QQQ.Close.SMA <- c(rep(NA, period), moving_average)

lines(QQQ$QQQ.Close.SMA, type='l', col='red', lwd=3)

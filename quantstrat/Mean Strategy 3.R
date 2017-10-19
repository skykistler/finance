source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
# initial.portfolio.date <- '1980-04-12'
# start.date             <- '1980-04-13'
end.date               <- '2017-10-06'
initial.equity         <- 10000
order.qty              <- 300
ticker                 <- 'MU'

buy.threshold  <- -1.5
sell.threshold <-  1.5

# Good orders, need to be tweaked:
# 350 OLN ~ 20%
# 35 IBB ~ 20%
# 1600 NOK ~ 38% but risky
# 150 HES ~ 50% @ -1.2 -> 1.5, 65.5% win rate
# 350 MU ~ 26.9%


# Apparently timezone needs to be UTC
Sys.setenv(TZ='UTC')

# Get prices
getSymbols(ticker, from=start.date, to=end.date, index.class='POSIXct', adjust=T, auto.assign = T)
symbol <- get(ticker)
colnames(symbol) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# Set currency and desired instrument
currency(primary_id='USD')
stock(primary_id=ticker, currency='USD', multiplier=1)

######################################################
# Strategy

# Need a stationary time-series without long term memory

# Augmented Dickey-Fuller Test
# p-value > 0.05 = not stationary
adf.test(Cl(symbol))

# Kwiatkowski-Phillips-Schmidt-Shin Test
# p-value < 0.05 = not stationary
kpss.test(Cl(symbol))

# Hurst Exponent
# H = 0.5 (Random Walk)
# 0.5 < H <= 1 (Trending)
# 0 <= H < 0.5 (Mean Reverting)
hurstexp(Cl(symbol))


# Differentiated Time Series
diffx <- diff(log(Cl(symbol)), lag=1)
diffx <- diffx[complete.cases(diffx)]
adf.test(diffx)
kpss.test(diffx)
hurstexp(diffx)

# Z-Score Function and Calculation
zscore.fun <- function(x,n) {roll_scale(x, width=n)}
zscore <- zscore.fun(diff(log(symbol$Close), lag=1), n=24)

quantiles <- (zscore %>% as.data.frame %>% filter(!is.na(zscore)))$Low %>% quantile
# buy.threshold  <- quantiles[[2]]
# sell.threshold <- quantiles[[4]] + quantiles[[3]]

plot(zscore) %>% print
abline(h=sell.threshold, col=2)
abline(h=buy.threshold, col=3)

# Strategy init
mean3.strat <- "MeanStrat3"
rm.strat(mean3.strat)
strategy(name=mean3.strat, store=T)

# Add indicators
mean3.strat %<>% add.indicator(name="zscore.fun", arguments=list(x=quote(diff(log(Cl(mktdata)), lag=1)), n=24), label='zscore')

# Add signals
mean3.strat %<>% add.signal(name='sigThreshold', arguments=list(threshold=buy.threshold,  column="zscore", relationship="lt"), label="BuySignal")
mean3.strat %<>% add.signal(name='sigThreshold', arguments=list(threshold=sell.threshold, column="zscore", relationship="gt"), label="SellSignal")

# Add Enter rule (with stop loss)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=order.qty, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=T)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=T)

# Add Exit rule
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

mean3.portf <- "MeanPort3"
rm.strat(mean3.portf)

initPortf(name = mean3.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean3.strat, portfolios = mean3.portf, initDate = initial.portfolio.date, initEq = initial.equity)
initOrders(portfolio = mean3.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean3.strat, portfolios = mean3.portf)

updatePortf(Portfolio=mean3.portf)
updateAcct(name=mean3.strat)
updateEndEq(Account=mean3.strat)

######################################################
# Evaluate

# Stats
mean3.stats <- t(tradeStats(Portfolios=mean3.portf))
View(mean3.stats)
mean3.perstats <- perTradeStats(Portfolio=mean3.portf)
View(mean3.perstats)

print(paste("Number of trades:", nrow(mean3.perstats)), quote=F)
print(paste("Average holding days:", mean(mean3.perstats$duration / 86400) %>% as.numeric), quote=F)

losses <- mean3.perstats %>% filter (Net.Trading.PL < 0) %>% nrow
wins   <- mean3.perstats %>% filter (Net.Trading.PL > 0) %>% nrow
print(paste("Win rate:", wins / (losses + wins) %>% as.numeric), quote=F)

# Order book
mean3.book <- getOrderBook(portfolio=mean3.portf)
# print(mean3.book)

# Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=mean3.portf, Symbol=ticker, theme=chart.theme)
add_BBands(n=20,sd=2)

# Equity curve
mean3.acct <- getAccount(Account=mean3.strat)
mean3.equity <- mean3.acct$summary$End.Eq
plot(mean3.equity, main="Mean3 Strategy Equity Curve") %>% print
source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
# initial.portfolio.date <- '2000-06-30'
# start.date             <- '2000-07-01'
end.date               <- '2017-07-24'
initial.equity         <- 10000
ticker                 <- 'MU'

# Apparently timezone needs to be UTC
Sys.setenv(TZ='UTC')

# Get prices
symbol <- getSymbols(ticker, from=start.date, to=end.date, index.class='POSIXct', adjust=T, auto.assign = F)
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
zscore <- zscore.fun(diff(log(Cl(symbol)), lag=1), n=24)
plot(zscore)
abline(h=1.5, col=2)
abline(h=-1.5, col=3)

# Strategy init
mean3.strat.name <- "MeanStrat3"
rm.strat(mean3.strat.name)
mean3.strat <- strategy(name=mean3.strat.name)

# Add indicators
mean3.strat %<>% add.indicator(name="zscore.fun", arguments=list(x=quote(diff(log(Cl(mktdata)), lag=1)), n=24), label='zscore')

# Add signals
mean3.strat %<>% add.signal(name='sigCrossover', arguments=list(threshold=-1.5, column="zscore", relationship="lt"), label="BuySignal")
mean3.strat %<>% add.signal(name='sigCrossover', arguments=list(threshold= 1.5, column="zscore", relationship="gt"), label="BuySignal")

# Add Enter rule (with stop loss)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=10, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
mean3.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

mean3.portf <- "MeanPort3"
rm.strat(mean3.portf)

initPortf(name = mean3.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean3.strat, portfolios = mean3.portf, initDate = initial.portfolio.date, initEquity = initial.equity)
initOrders(portfolio = mean3.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean3.strat, portfolios = mean3.portf)

updatePortf(Portfolio=mean3.portf)
updateAcct(name=mean3.strat)
updateEndEq(Account=mean3.strat)


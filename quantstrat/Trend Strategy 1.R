source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
end.date               <- '2017-07-24'
initial.equity         <- 10000
ticker                 <- 'MU'

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

# EDA
lineChart(symbol)
addSMA(n=5, col='red')
addSMA(n=20,col='blue')

# Strategy init
trend1.strat.name <- "TrendStrat1"
rm.strat(trend1.strat.name)
trend1.strat <- strategy(name=trend1.strat.name)

# Add indicators
trend1.strat %<>% add.indicator(name="SMA", arguments=list(x=quote(Cl(mktdata)), n=5), label="Fast SMA")
trend1.strat %<>% add.indicator(name="SMA", arguments=list(x=quote(Cl(mktdata)), n=20), label="Slow SMA")

# Add signals
trend1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("FastSMA", "SlowSMA"), relationship="gt"), label="BuySignal")
trend1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("FastSMA", "SlowSMA"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
trend1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=10, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
trend1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
trend1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
trend1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

trend1.portf <- "TrendPort1"
rm.strat(trend1.portf)

initPortf(name = trend1.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = trend1.strat, portfolios = trend1.portf, initDate = initial.portfolio.date, initEquity = initial.equity)
initOrders(portfolio = trend1.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = trend1.strat, portfolios = trend1.portf)

updatePortf(Portfolio=trend1.portf)
updateAcct(name=trend1.strat)
updateEndEq(Account=trend1.strat)


######################################################
# Evaluate






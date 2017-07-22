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
symbol <- getSymbols(ticker, from=start.date, to=end.date, index.class='POSIXct', adjust=T, auto.assign = F)
colnames(symbol) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# Set currency and desired instrument
currency(primary_id='USD')
stock(primary_id=ticker, currency='USD', multiplier=1)

######################################################
# Strategy

# EDA
barChart(symbol)
addMACD(fast=12,slow=26,signal=9)


# Strategy init
trend2.strat.name <- "TrendStrat2"
rm.strat(trend2.strat.name)
trend2.strat <- strategy(name=trend2.strat.name)

# Add indicators
trend2.strat %<>% add.indicator(name="MACD", arguments=list(x=quote(Cl(mktdata)), nFast=12,nSlow=26,nSig=9), label="MACD")

# Add signals
trend2.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("macd", "signal"), relationship="gt"), label="BuySignal")
trend2.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("macd", "signal"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=10, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)


######################################################
# Portfolio

trend2.portf <- "TrendPort2"
rm.strat(trend2.portf)

initPortf(name = trend2.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = trend2.strat, portfolios = trend2.portf, initDate = initial.portfolio.date, initEquity = initial.equity)
initOrders(portfolio = trend2.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = trend2.strat, portfolios = trend2.portf)

updatePortf(Portfolio=trend2.portf)
updateAcct(name=trend2.strat)
updateEndEq(Account=trend2.strat)


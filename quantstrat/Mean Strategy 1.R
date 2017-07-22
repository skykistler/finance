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
lineChart(symbol)
addBBands(n=20,sd=2)


# Strategy init
mean1.strat.name <- "MeanStrat1"
rm.strat(mean1.strat.name)
mean1.strat <- strategy(name=mean1.strat.name)

# Add indicators
mean1.strat %<>% add.indicator(name="BBands", arguments=list(HLC=quote(HLC(mktdata)), n=20,sd=2), label="BBands")

# Add signals
mean1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("Close", "dn"), relationship="lt"), label="BuySignal")
mean1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("Close", "up"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=10, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

mean1.portf <- "MeanPort1"
rm.strat(mean1.portf)

initPortf(name = mean1.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean1.strat, portfolios = mean1.portf, initDate = initial.portfolio.date, initEquity = initial.equity)
initOrders(portfolio = mean1.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean1.strat, portfolios = mean1.portf)

updatePortf(Portfolio=mean1.portf)
updateAcct(name=mean1.strat)
updateEndEq(Account=mean1.strat)

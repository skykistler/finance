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
addRSI(n=14)


# Strategy init
mean2.strat.name <- "MeanStrat2"
rm.strat(mean2.strat.name)
mean2.strat <- strategy(name=mean2.strat.name)

# Add indicators
mean2.strat %<>% add.indicator(name="RSI", arguments=list(price=quote(getPrice(mktdata)), n=14, maType='EMA'), label="BBands")
 
# Add signals
mean2.strat %<>% add.signal(name='sigCrossover', arguments=list(threshold=30, column="RSI", relationship="lt"), label="BuySignal")
mean2.strat %<>% add.signal(name='sigCrossover', arguments=list(threshold=70, column="RSI", relationship="gt"), label="BuySignal")

# Add Enter rule (with stop loss)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=10, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)


######################################################
# Portfolio

mean2.portf <- "MeanPort2"
rm.strat(mean2.portf)

initPortf(name = mean2.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean2.strat, portfolios = mean2.portf, initDate = initial.portfolio.date, initEquity = initial.equity)
initOrders(portfolio = mean2.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean2.strat, portfolios = mean2.portf)

updatePortf(Portfolio=mean2.portf)
updateAcct(name=mean2.strat)
updateEndEq(Account=mean2.strat)

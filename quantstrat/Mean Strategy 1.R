source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
end.date               <- '2017-07-24'
initial.equity         <- 10000
order.qty              <- 320
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
addBBands(n=20,sd=2)


# Strategy init
mean1.strat <- "MeanStrat1"
rm.strat(mean1.strat)
strategy(name=mean1.strat, store=T)

# Add indicators
mean1.strat %<>% add.indicator(name="BBands", arguments=list(HLC=quote(HLC(mktdata)), n=20,sd=2), label="BBands")

# Add signals
mean1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("Close", "dn"), relationship="lt"), label="BuySignal")
mean1.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("Close", "up"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=order.qty, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
mean1.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

mean1.portf <- "MeanPort1"
rm.strat(mean1.portf)

initPortf(name = mean1.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean1.strat, portfolios = mean1.portf, initDate = initial.portfolio.date, initEq = initial.equity)
initOrders(portfolio = mean1.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean1.strat, portfolios = mean1.portf)

updatePortf(Portfolio=mean1.portf)
updateAcct(name=mean1.strat)
updateEndEq(Account=mean1.strat)

######################################################
# Evaluate

# Stats
mean1.stats <- t(tradeStats(Portfolios=mean1.portf))
View(mean1.stats)
mean1.perstats <- perTradeStats(Portfolio=mean1.portf)
View(mean1.perstats)

# Order book
mean1.book <- getOrderBook(portfolio=mean1.portf)
# print(mean1.book)

# Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=mean1.portf, Symbol=ticker, theme=chart.theme)
add_BBands(n=20,sd=2)

# Equity curve
mean1.acct <- getAccount(Account=mean1.strat)
mean1.equity <- mean1.acct$summary$End.Eq
plot(mean1.equity, main="Mean1 Strategy Equity Curve") %>% print

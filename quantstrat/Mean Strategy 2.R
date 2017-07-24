source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
end.date               <- '2017-07-24'
initial.equity         <- 10000
order.qty              <- 100
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
barChart(symbol)
addRSI(n=14)


# Strategy init
mean2.strat <- "MeanStrat2"
rm.strat(mean2.strat)
strategy(name=mean2.strat, store=T)

# Add indicators
mean2.strat %<>% add.indicator(name="RSI", arguments=list(price=quote(getPrice(mktdata)), n=14, maType='EMA'), label="RSI")
 
# Add signals
mean2.strat %<>% add.signal(name='sigThreshold', arguments=list(threshold=30, column="RSI", relationship="lt"), label="BuySignal")
mean2.strat %<>% add.signal(name='sigThreshold', arguments=list(threshold=70, column="RSI", relationship="gt"), label="SellSignal")

# Add Enter rule (with stop loss)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=order.qty, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
mean2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)


######################################################
# Portfolio

mean2.portf <- "MeanPort2"
rm.strat(mean2.portf)

initPortf(name = mean2.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = mean2.strat, portfolios = mean2.portf, initDate = initial.portfolio.date, initEq = initial.equity)
initOrders(portfolio = mean2.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = mean2.strat, portfolios = mean2.portf)

updatePortf(Portfolio=mean2.portf)
updateAcct(name=mean2.strat)
updateEndEq(Account=mean2.strat)

######################################################
# Evaluate

# Stats
mean2.stats <- t(tradeStats(Portfolios=mean2.portf))
View(mean2.stats)
mean2.perstats <- perTradeStats(Portfolio=mean2.portf)
# View(mean2.perstats)

# Order book
mean2.book <- getOrderBook(portfolio=mean2.portf)
# print(mean2.book)

# Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=mean2.portf, Symbol=ticker, theme=chart.theme)
add_RSI(n=14)

# Equity curve
mean2.acct <- getAccount(Account=mean2.strat)
mean2.equity <- mean2.acct$summary$End.Eq
plot(mean2.equity, main="Mean2 Strategy Equity Curve") %>% print
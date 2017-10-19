source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2015-06-30'
start.date             <- '2015-07-01'
end.date               <- '2017-08-05'
initial.equity         <- 10000
order.qty              <- 350
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
addSMA(n=5, col='red')
addSMA(n=20,col='blue')
addSMA(n=100,col='yellow')
addSMA(n=200,col='gray')
addSMA(n=500,col='white')

# Strategy init
trend1.strat <- "TrendStrat1"
rm.strat(trend1.strat)
strategy(name=trend1.strat, store=T)

# Add indicators
trend1.strat %>% add.indicator(name="SMA", arguments=list(x=quote(Cl(mktdata)), n=5), label="FastSMA")
trend1.strat %>% add.indicator(name="SMA", arguments=list(x=quote(Cl(mktdata)), n=20), label="SlowSMA")

# Add signals
trend1.strat %>% add.signal(name='sigCrossover', arguments=list(columns=c("FastSMA", "SlowSMA"), relationship="gt"), label="BuySignal")
trend1.strat %>% add.signal(name='sigCrossover', arguments=list(columns=c("FastSMA", "SlowSMA"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
trend1.strat %>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=order.qty, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
trend1.strat %>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
trend1.strat %>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
trend1.strat %>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)

######################################################
# Portfolio

trend1.portf <- "TrendPort1"
rm.strat(trend1.portf)
initPortf(name = trend1.portf, symbols = ticker, initDate = initial.portfolio.date)

trend1.strat %>% initAcct(portfolios = trend1.portf, initDate = initial.portfolio.date, initEq = initial.equity)

initOrders(portfolio = trend1.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = trend1.strat, portfolios = trend1.portf)

updatePortf(Portfolio=trend1.portf)
updateAcct(name=trend1.strat)
updateEndEq(Account=trend1.strat)

######################################################
# Evaluate

# Stats
trend1.stats <- t(tradeStats(Portfolios=trend1.portf))
View(trend1.stats)
trend1.perstats <- perTradeStats(Portfolio=trend1.portf)
# View(trend1.perstats)

# Order book
trend1.book <- getOrderBook(portfolio=trend1.portf)
# print(trend1.book)

# Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=trend1.portf, Symbol=ticker, theme=chart.theme)
add_SMA(n=5)
add_SMA(n=20,col='darkblue')

# Equity curve
trend1.acct <- getAccount(Account=trend1.strat)
trend1.equity <- trend1.acct$summary$End.Eq
plot(trend1.equity, main="Trend1 Strategy Equity Curve") %>% print

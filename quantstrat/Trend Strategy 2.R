source('libraries.R')

# Configure dates and size of portfolio
initial.portfolio.date <- '2016-06-30'
start.date             <- '2016-07-01'
end.date               <- '2017-07-24'
initial.equity         <- 10000
order.qty              <- 'all'
ticker                 <- 'TSLA'

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
addMACD(fast=12,slow=26,signal=9)


# Strategy init
trend2.strat <- "TrendStrat2"
rm.strat(trend2.strat)
strategy(name=trend2.strat, store=T)

# Add indicators
trend2.strat %<>% add.indicator(name="MACD", arguments=list(x=quote(Cl(mktdata)), nFast=12,nSlow=26,nSig=9), label="MACD")

# Add signals
trend2.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("macd", "signal"), relationship="gt"), label="BuySignal")
trend2.strat %<>% add.signal(name='sigCrossover', arguments=list(columns=c("macd", "signal"), relationship="lt"), label="SellSignal")

# Add Enter rule (with stop loss)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty=order.qty, ordertype='market', orderside='long'), type='enter', label='EnterRule', enabled=T)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoplimit', threshold=0.05, orderside='long'), type='chain', label='StopLoss', parent='EnterRule', enabled=F)
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="BuySignal", sigval=T, orderqty='all', ordertype='stoptrailing', threshold=0.07, orderside='long'), type='chain', label='TrailingStop', parent='EnterRule', enabled=F)

# Add Exit rule
trend2.strat %<>% add.rule(name='ruleSignal', arguments=list(sigcol="SellSignal", sigval=T, orderqty='all', ordertype='market', orderside='long', TxnFees=0), type='exit', label='ExitRule', enabled=T)


######################################################
# Portfolio

trend2.portf <- "TrendPort2"
rm.strat(trend2.portf)

initPortf(name = trend2.portf, symbols = ticker, initDate = initial.portfolio.date)
initAcct(name = trend2.strat, portfolios = trend2.portf, initDate = initial.portfolio.date, initEq = initial.equity)
initOrders(portfolio = trend2.portf, initDate = initial.portfolio.date)

######################################################
# Execute

applyStrategy(strategy = trend2.strat, portfolios = trend2.portf)

updatePortf(Portfolio=trend2.portf)
updateAcct(name=trend2.strat)
updateEndEq(Account=trend2.strat)

######################################################
# Evaluate

# Stats
trend2.stats <- t(tradeStats(Portfolios=trend2.portf))
View(trend2.stats)
trend2.perstats <- perTradeStats(Portfolio=trend2.portf)
# View(trend2.perstats)

# Order book
trend2.book <- getOrderBook(portfolio=trend2.portf)
# print(trend2.book)

# Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio=trend2.portf, Symbol=ticker, theme=chart.theme)
add_MACD(fast=12,slow=26,signal=9, maType='EMA')

# Equity curve
trend2.acct <- getAccount(Account=trend2.strat)
trend2.equity <- trend2.acct$summary$End.Eq
plot(trend2.equity, main="Trend2 Strategy Equity Curve") %>% print

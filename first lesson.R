source('libraries.R')

#######################

basket_symbols <- c('^GSPC', 'OZRK', 'YELP', 'AMZN', 'AAPL', 'UAL')

getSymbols(basket_symbols)


basket <- data.frame(merge(YELP, AMZN, AAPL))


lineChart(AAPL, line.type='h', theme='white', TA=NULL)
barChart(GSPC, bar.type='hlc', TA=NULL)
candleChart(GSPC, subset='2008-06::2009-06')
candleChart(GSPC, subset='2015-06::2015-12')

chartSeries(GSPC, theme='white')
addSMA(n=20)
addROC(n=200)


chartSeries(OZRK, theme="white", TA="addVo();addBBands();addCCI()", subset='2016-01::2017-07')


GSPC.EMA.20  <- EMA(GSPC$GSPC.Close, n=20)
GSPC.EMA.100 <- EMA(GSPC$GSPC.Close, n=100)

chartSeries(GSPC, theme=chartTheme("white"), up.col='black', dn.col='black')
addTA(GSPC.EMA.20, on=1, col="red")
addTA(GSPC.EMA.100, on=1, col="blue")

addTA(GSPC.EMA.20 - GSPC.EMA.100, col='blue', type='h', legend='20-100 MA')

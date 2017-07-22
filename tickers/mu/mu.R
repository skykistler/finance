source('libraries.R')

getSymbols('MU', src='yahoo', from="2004-01-01")
chartSeries(MU, type="matchsticks", theme='black', subset='::', TA='
            addEMA(20, col="blue");
            addEMA(50, col="yellow");
            addEVWMA(50, col="pink");
            addEMA(200, col="red");')

EMA(MU$MU.Close, n=5) %>% View

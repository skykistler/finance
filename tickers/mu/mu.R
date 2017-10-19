source('libraries.R')

getSymbols('MU', src='yahoo', from="2014-01-01")
chartSeries(MU, type="matchsticks", theme='black', subset='::', TA='
            addEMA(20, col="blue");
            addEMA(50, col="yellow");
            addEVWMA(50, col="pink");
            addEMA(100, col="red");
            addRSI();
            addMACD();')

EMA(MU$MU.Close, n=20) %>% View

source('libraries.R')

# Built-in indicators
??quantmod::add

if(!exists('GSPC'))
  getSymbols('^GSPC')

chartSeries(GSPC,
            theme='white',
            TA="
              addSMA(50, col='black');
              addSMA(200, col='blue');
              addADX(n=14, maType='EMA', wilder=T)
            ",
            # subset='::'
            subset='2013::2015-07'
)

VWAP.Slow <- VWAP(price = GSPC$GSPC.Close, volume = GSPC$GSPC.Volume, n=100)
VWAP.Fast <- VWAP(price = GSPC$GSPC.Close, volume = GSPC$GSPC.Volume, n=20)
VWAP.Diff <- VWAP.Fast - VWAP.Slow

chartSeries(GSPC['::2015-07'], 
            theme='white',
            TA="
              addVo();
              addTA(VWAP.Slow, on=1, col='red');
              addTA(VWAP.Fast, on=1, col='blue');
              addTA(VWAP.Diff,col='blue');
            "
)


VWAP.Slow <- VWAP(price = IBEX$IBEX.Close, volume = IBEX$IBEX.Close, n=100)
VWAP.Fast <- VWAP(price = IBEX$IBEX.Close, volume = IBEX$IBEX.Close, n=20)
VWAP.Diff <- VWAP.Fast - VWAP.Slow

ADX.20 <- ADX(IBEX, n=14)

Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
  VWAP.Diff  > 100,
  IBEX$IBEX.Close, NA
)

Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
  VWAP.Diff  < -100,
  IBEX$IBEX.Close, NA
)

chartSeries(IBEX, 
            theme='white',
            TA="
              addVo();
              addTA(VWAP.Slow, on=1, col='red');
              addTA(VWAP.Fast, on=1, col='blue');
              addTA(VWAP.Diff,col='blue');
")


plot(IBEX)
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18) 









source('libraries.R')

EMA.50 <- EMA(IBEX$IBEX.Close, n=50)
EMA.200 <- EMA(IBEX$IBEX.Close, n=200)

Slow.Diff <- EMA.50 - EMA.200
CCI.IND <- CCI(HLC=IBEX[,c("IBEX.High", "IBEX.Low", "IBEX.Close")], n=100)
CV.IND <- chaikinVolatility(HL=IBEX[,c("IBEX.High", "IBEX.Low")], n=100)

Long_Trades <- ifelse(
    shift(v=as.numeric(CCI.IND), places=3, dir="right") > CCI.IND &
    CCI.IND < 100 &
    CV.IND < 0 &
    Slow.Diff > 0
  ,IBEX$IBEX.Close
  ,NA
)

Short_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=3, dir="right") > CCI.IND &
    CCI.IND > -100 &
    CV.IND < 0 &
    Slow.Diff < 0
  ,IBEX$IBEX.Close
  ,NA
)

plot(IBEX)
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)
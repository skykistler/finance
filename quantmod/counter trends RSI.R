source('libraries.R')

EMA.50 <- EMA(OZRK$OZRK.Adjusted, n=50)
EMA.200 <- EMA(OZRK$OZRK.Adjusted, n=200)
Slow.Diff <- EMA.50 - EMA.200


RSI.Fast <- RSI(price=OZRK$OZRK.Adjusted, n=10)
RSI.Slow <- RSI(price=OZRK$OZRK.Adjusted, n=30)
RSI.Diff <- RSI.Fast - RSI.Slow

CV.IND <- chaikinVolatility(HL=OZRK, n=100)

Long_Trades <- ifelse(
    shift(v=as.numeric(RSI.Diff), places=2, dir="right") < 0 &
    shift(v=as.numeric(RSI.Diff), places=1, dir="right") > 0 &
    RSI.Diff < 0 &
    CV.IND < -0.1 &
    Slow.Diff > 0
  ,OZRK$OZRK.Close
  ,NA
)

Short_Trades <- ifelse(
  shift(v=as.numeric(RSI.Diff), places=2, dir="right") > 0 &
    shift(v=as.numeric(RSI.Diff), places=1, dir="right") < 0 &
    RSI.Diff > 0 &
    CV.IND < -0.1 &
    Slow.Diff < 0
  ,OZRK$OZRK.Close
  ,NA
)

plot(OZRK)
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)
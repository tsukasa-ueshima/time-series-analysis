###　沖本本の問題演習　###

setwd("~/Desktop/計量時系列分析/Data")
par(family= "HiraKakuProN-W3") #日本語文字化け対策 

# 1.3
t = 150
mu = numeric(t)
e = rnorm(t, mean=0, sd=1)
y = mu + e
plot(1:t, y, type="l", xlab="Time")

# 1.5
library(readxl)
economicdata <- read_excel("economicdata.xls")
economicdata <- economicdata[,-1] #日付を削除
## (1)
ts_economicdata <- ts(economicdata, frequency=12, start=c(1975,1))
plot(ts_economicdata, main= "economicdata")
## (2)
diff_log_economicdata <- diff(log(ts_economicdata))
## (3)
plot(diff_log_economicdata[,2], xlab="", ylab= "", main="TOPIX")
plot(diff_log_economicdata[,3], xlab="", ylab= "", main="実効為替レート")
plot(diff_log_economicdata[,4], xlab="", ylab= "", main="鉱工業生産指数")
## (4)
acf(diff_log_economicdata[,3], lag.max=20)
## (5)
acf(diff_log_economicdata[,2], lag.max=20)
acf(diff_log_economicdata[,4], lag.max=20)

# 2.6
arma <- read_excel("arma.xls")
## (1)
par(mfrow=c(2,2))
acf(arma$y1, lag.max=20, main= "標本自己相関")
acf(arma$y1, lag.max=20, type="p", main= "標本偏自己相関")
## (2)
### 自己相関は減衰傾向
### 偏自己相関は4(=3+1)次以降でほとんど0 or 減衰
### 候補は AR(3), ARMA(1,1), ARMA(1,2), ARMA(2,1), ARMA(2,2)
## (3)
library(forecast)
ar3 <- arima(arma$y1, order = c(3,0,0))
arma11 <- arima(arma$y1, order = c(1,0,1))
arma12 <- arima(arma$y1, order = c(1,0,2))
arma21 <- arima(arma$y1, order = c(2,0,1))
arma22 <- arima(arma$y1, order = c(2,0,2))
ar3ic = c(AIC(ar3), BIC(ar3))
arma11ic = c(AIC(arma11), BIC(arma11))
arma12ic = c(AIC(arma12), BIC(arma12))
arma21ic = c(AIC(arma21), BIC(arma21))
arma22ic = c(AIC(arma22), BIC(arma22))
ic = data.frame("AR3" = ar3ic, 
                "ARMA11" = arma11ic,
                "ARMA12" = arma12ic,
                "ARMA21" = arma21ic,
                "ARMA22" = arma22ic)
rownames(ic) <- c("AIC", "SIC")                              
ic
### これなら一発でモデル選択できる
auto.arima(arma$y1,max.p=2,max.q=2,stepwise=T,trace=T)
## (4)
Box.test(arma12$res, type="Ljung")

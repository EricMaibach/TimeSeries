source('https://nmimoto.github.io/R/TS-00.txt')

D <- read.csv("https://nmimoto.github.io/datasets/car.csv", header=T)
D

D0 <- ts(D[,2], start=c(1960,1), freq=1)

options(repr.plot.width=30, repr.plot.height=12)
plot(D0, type='o')

Fit0 <- auto.arima(D0, stepwise = FALSE, approximation = FALSE)
Fit0

Randomness.tests(Fit0$residuals)

Fit00 <- auto.arima(D0, stepwise = FALSE, approximation = FALSE, max.p = 15, max.q = 15)
Fit00

Fit01 <- Arima(D0, order=c(15,1,15), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, order=c(13,1,15), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, order=c(13,1,14), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, order=c(13,1,12), include.drift = TRUE)
Fit01

Randomness.tests(Fit01$residuals)

Fit01 <- Arima(D0, order=c(12,1,0), include.drift = TRUE)
Fit01

Randomness.tests(Fit01$residuals)

forecast1 <- forecast(Fit01, 12)
forecast1

plot(forecast1)

Y <- D0
window.size <- 80
Arima.order <- c(12,1,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = NULL
xreg = FALSE
seasonal = c(0,0,0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit01 <- Arima(D0, order=c(17,1,0), method = "CSS", include.drift = TRUE)
Fit01

Randomness.tests(Fit01$residuals)

forecast1 <- forecast(Fit01, 12)
forecast1

plot(forecast1)

Y <- D0
window.size <- 80
Arima.order <- c(17,1,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = NULL
xreg = FALSE
seasonal = c(0,0,0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit01 <- Arima(D0, order=c(17,1,0), include.drift = TRUE, method = "CSS", fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,0,NA,NA,NA))
Fit01

Randomness.tests(Fit01$residuals)

Fit02 <- auto.arima(D0, d=0, D=0, xreg=time(D0), stepwise=FALSE, approximation=FALSE, max.p = 17, max.q = 17)
Fit02

Randomness.tests(Fit02$residuals)

Fit02 <- Arima(D0, order=c(12,0,0), include.drift=FALSE,  xreg=time(D0))
Fit02

Randomness.tests(Fit02$residuals)

h = 12
forecast2 <- forecast(Fit02, xreg=last(time(D0))+(1:h)/frequency(D0))
forecast2

Y <- D0
window.size <- 80
Arima.order <- c(12,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = TRUE
seasonal = c(0, 0, 0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit02 <- Arima(D0, order=c(17,0,17), method="CSS", include.drift=FALSE,  xreg=time(D0))
Fit02

Fit02 <- Arima(D0, order=c(17,0,16), include.drift=FALSE,  xreg=time(D0))
Fit02

Fit02 <- Arima(D0, order=c(17,0,14), method = "CSS", include.drift=FALSE,  xreg=time(D0))
Fit02

Fit02 <- Arima(D0, order=c(17,0,0), method = "CSS", include.drift=FALSE,  xreg=time(D0))
Fit02

Randomness.tests(Fit02$residuals)

h = 12
forecast2 <- forecast(Fit02, xreg=last(time(D0))+(1:h)/frequency(D0))
forecast2

Y <- D0
window.size <- 80
Arima.order <- c(12,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = TRUE
seasonal = c(0, 0, 0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit02 <- Arima(D0, order=c(17,0,0), method = "CSS", include.drift=FALSE,  xreg=time(D0), fixed = c(NA,NA,0,0,0,NA,0,0,0,NA,NA,NA,0,0,0,NA,NA,NA,NA))
Fit02

Randomness.tests(Fit02$residuals)

D1 <- ts(D[,2], start=c(1960,1), freq=12)

options(repr.plot.width=30, repr.plot.height=12)
plot(D1, type='o')

acf(D1)

pacf(D1)

Stationarity.tests(D1)

Y <- D1
Mav1 <- aggregate(c(Y), list(month=cycle(Y)),mean)$x
Y.MtlyAv <- ts(Mav1[cycle(Y)], start=start(Y), freq=frequency(Y))
Y.DS <- Y-Y.MtlyAv

plot(Y, type="o")
lines(Y.MtlyAv, col="blue")

D1.DS <- Y.DS
plot(D1.DS, type="o", main="De-seasonalized Car Data")

Reg1 <- lm(D1.DS ~ time(D1.DS))
summary(Reg1)

plot(Reg1$resid, type="o")
abline(Reg1, col="red")

Stationarity.tests(Reg1$resid)

acf(Reg1$resid)

pacf(Reg1$resid)

Fit11 <- auto.arima(D1.DS, d=0, D=0, xreg=time(D1.DS), stepwise=FALSE, approximation=FALSE)
Fit11

Randomness.tests(Fit11$resid)

h <- 12
Y <- D1.DS
Fit00 <- Fit11

Y.pred = forecast(Fit00, h, xreg=last(time(Y))+(1:h)/frequency(Y))
Y.pred

plot(Y.pred)

Y <- D1
Mav <- Mav1
Y.h <- Y.pred

Yhat = ts(Y.h$mean + Mav1, start=last(time(Y))+(1)/frequency(Y), freq=frequency(Y))
Yhat.CIu = ts(Y.h$lower[,2] + Mav1, start=last(time(Y))+(1)/frequency(Y), freq=frequency(Y))
Yhat.CIl = ts(Y.h$upper[,2]+ Mav1, start=last(time(Y))+(1)/frequency(Y), freq=frequency(Y))

ts.plot(Y,Yhat)
lines(Yhat, type="l", col="blue", lwd=2)
lines(Yhat.CIu, type="l", col="gray30", lty=1)
lines(Yhat.CIl, type="l", col="gray30", lty=1)

str(Yhat.CIu)

str(Yhat.CIl)

Fit2 <- auto.arima(D1, stepwise = FALSE, approximation = FALSE)
Fit2

Randomness.tests(Fit2$residuals)

forecast1 <- forecast(Fit2, 12)
forecast1

Y <- D1
window.size <- 80
Arima.order <- c(2,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = NULL
xreg = FALSE
seasonal = c(0, 1, 2)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

The sma2 term included in the model does not seems to be significant.  Try removing it, but first make sure taking a seasonal difference is appropriate

Fit21 <- Arima(D1, order=c(0,0,0), seasonal=c(0,1,1))
Fit21

Fit21 <- Arima(D1, order=c(0,0,0), seasonal=c(0,1,1), include.drift = TRUE)
Fit21

Arima(D1, order=c(0,0,12), seasonal=c(0,1,0))

Arima(D1, order=c(0,0,12), seasonal=c(0,1,0), include.drift=TRUE)

Fit22 <- Arima(D1, order=c(2,0,0), seasonal=c(0,1,1), include.drift = TRUE)
Fit22

Randomness.tests(Fit22$residuals)

Fit23 <- Arima(D1, order=c(11,0,11), seasonal=c(0,1,1), include.drift = TRUE)
Fit23

Fit24 <- Arima(D1, order=c(11,0,10), seasonal=c(0,1,1), include.drift = TRUE)
Fit24

Randomness.tests(Fit24$residuals)

forecast1 <- forecast(Fit24, 12)
forecast1

Y <- D1
window.size <- 80
Arima.order <- c(11,0,10)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = NULL
xreg = FALSE
seasonal = c(0, 1, 1)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

D11 <- ts(D[,2], start=c(1960,1), freq=11)

Fit25 <- auto.arima(D11, stepwise = FALSE, approximation = FALSE)
Fit25

Randomness.tests(Fit25$residuals)

plot(D1, type='o')

Stationarity.tests(D1)

plot(diff(D1), type='o')

Stationarity.tests(diff(D1))

plot(diff(D1, 12), type='o')

Stationarity.tests(diff(D1, 12))

Fit3 <- auto.arima(D1, d=1, D=0, stepwise = FALSE, approximation = FALSE)
Fit3

Randomness.tests(Fit3$residuals)

forecast1 <- forecast(Fit3, 12)
forecast1

Y <- D1
window.size <- 80
Arima.order <- c(2,1,1)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = FALSE
seasonal = c(1,0,0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

The model doe not appear to be performing well.

Fit24 <- Arima(D1, order=c(11,1,10), seasonal=c(1,0,0), include.drift = FALSE)
Fit24

Randomness.tests(Fit24$residuals)

forecast1 <- forecast(Fit24, 12)
forecast1

Y <- D1
window.size <- 80
Arima.order <- c(11,1,10)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = FALSE
seasonal = c(1,0,0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit4 <- auto.arima(D1, d=0, D=0, xreg=time(D1), stepwise=FALSE, approximation=FALSE)
Fit4

Randomness.tests(Fit4$resid)

h = 12
forecast2 <- forecast(Fit4, xreg=last(time(D1))+(1:h)/frequency(D1))
forecast2

Y <- D1
window.size <- 80
Arima.order <- c(0,0,2)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = TRUE
seasonal = c(1, 0, 0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

Fit4 <- Arima(D1, order=c(17,0,0), seasonal=c(1,0,0), include.drift = FALSE, xreg=time(D1))
Fit4

Randomness.tests(Fit4$resid)

h = 12
forecast2 <- forecast(Fit4, xreg=last(time(D1))+(1:h)/frequency(D1))
forecast2

Y <- D1
window.size <- 80
Arima.order <- c(17,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = NULL
xreg = TRUE
seasonal = c(1, 0, 0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

We have a lot of AR terms that are not significant.  What if we force them to 0.

Fit4 <- Arima(D1, order=c(17,0,0), seasonal=c(1,0,0), include.drift = FALSE, xreg=time(D1), fixed = c(NA,0,0,0,0,NA,NA,0,0,0,NA,NA,0,0,0,0,NA,NA,NA,NA))
Fit4

Randomness.tests(Fit4$resid)

FitE1 <- Arima(D1, order=c(11,0,10), seasonal=c(0,1,1), include.drift = TRUE)
FitE1

forecast1 <- forecast(FitE1, 12)
forecast1

plot(forecast1)

Y <- D1
window.size <- 80
Arima.order <- c(11,0,10)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = FALSE
xreg = FALSE
seasonal = c(0,1,1)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

FitE2 <- Arima(D1, order=c(2,0,0), seasonal=c(0,1,2), include.drift = TRUE)
FitE2

forecast2 <- forecast(FitE2, 12)
forecast2

plot(forecast2)

Y <- D1
window.size <- 80
Arima.order <- c(2,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = FALSE
xreg = FALSE
seasonal = c(0,1,2)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)



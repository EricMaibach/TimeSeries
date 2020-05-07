source('https://nmimoto.github.io/R/TS-00.txt')

D <- read.csv("https://nmimoto.github.io/datasets/car.csv", header=T)
D

D0 <- ts(D[,2], start=c(1960,1), freq=1)

options(repr.plot.width=30, repr.plot.height=12)
plot(D0, type='o')

Fit0 <- auto.arima(D0, stepwise = FALSE, approximation = FALSE)
Fit0

Randomness.tests(Fit0$residuals)

Fit0 <- auto.arima(D0, lambda = 0, stepwise = FALSE, approximation = FALSE)
Fit0

Randomness.tests(Fit0$residuals)

Fit00 <- auto.arima(D0, stepwise = FALSE, lambda=0, approximation = FALSE, max.p = 15, max.q = 15)
Fit00

Fit01 <- Arima(D0, lambda=0, order=c(15,1,15), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(13,1,15), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(13,1,13), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(12,1,12), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(12,1,11), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(12,1,9), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(12,1,7), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,7), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,6), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,5), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,4), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,3), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,2), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,1), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(11,1,0), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(12,1,0), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(13,1,0), include.drift = TRUE)
Fit01

Fit01 <- Arima(D0, lambda=0, order=c(13,1,0), include.drift = TRUE)
Fit01

Randomness.tests(Fit01$residuals)

Fit01 <- Arima(D0, lambda=0, order=c(17,1,0), include.drift = TRUE)
Fit01

Randomness.tests(Fit01$residuals)

Fit02 <- auto.arima(D0, d=0, D=0, lambda=0, xreg=time(D0), stepwise=FALSE, approximation=FALSE, max.p = 17, max.q = 17)
Fit02

Randomness.tests(Fit02$residuals)

Fit02 <- Arima(D0, lambda=0, order=c(17,0,0), include.drift=FALSE,  xreg=time(D0))
Fit02

Randomness.tests(Fit02$residuals)

D1 <- ts(D[,2], start=c(1960,1), freq=12)

Fit2 <- auto.arima(D1, lambda=0, stepwise = FALSE, approximation = FALSE)
Fit2

Randomness.tests(Fit2$residuals)

Fit21 <- Arima(D1, lambda = 0, order=c(0,0,0), seasonal=c(0,1,1))
Fit21

Fit21 <- Arima(D1, lambda = 0, order=c(0,0,0), seasonal=c(0,1,1), include.drift = TRUE)
Fit21

Arima(D1, lambda=0, order=c(0,0,12), seasonal=c(0,1,0))

Arima(D1, lambda=0, order=c(0,0,12), seasonal=c(0,1,0), include.drift=TRUE)

Fit22 <- Arima(D1, lambda=0, order=c(2,0,0), seasonal=c(0,1,1), include.drift = TRUE)
Fit22

Randomness.tests(Fit22$residuals)

Fit23 <- Arima(D1, lambda=0, order=c(11,0,11), seasonal=c(0,1,1), include.drift = TRUE)
Fit23

Fit24 <- Arima(D1, lambda=0, order=c(11,0,0), seasonal=c(0,1,1), include.drift = TRUE)
Fit24

Randomness.tests(Fit22$residuals)

D11 <- ts(D[,2], start=c(1960,1), freq=11)

Fit25 <- auto.arima(D11, lambda=0, stepwise = FALSE, approximation = FALSE)
Fit25

Randomness.tests(Fit25$residuals)

plot(log(D1), type='o')

plot(diff(log(D1)), type='o')

Stationarity.tests(diff(log(D1)))

plot(diff(log(D1), 12), type='o')

Stationarity.tests(diff(log(D1), 12))

Fit3 <- auto.arima(D1, d=1, D=0, lambda=0, stepwise = FALSE, approximation = FALSE)
Fit3

Randomness.tests(Fit3$residuals)

Fit4 <- auto.arima(D1, d=0, D=0, lambda=0, xreg=time(D1), stepwise=FALSE, approximation=FALSE)
Fit4

Randomness.tests(Fit4$resid)

FitE1 <- Arima(D1, lambda=0, order=c(2,0,0), seasonal=c(0,1,2), include.drift = TRUE)
FitE1

forecast1 <- forecast(FitE1, 12)
forecast1

plot(forecast1)

Y <- D1
window.size <- 80
Arima.order <- c(2,0,0)
pred.plot <- TRUE
include.mean = TRUE
include.drift = TRUE
lambda = 0
xreg = FALSE
seasonal = c(0, 1, 2)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

FitE2 <- Arima(D1, lambda=0, order=c(0,0,2), seasonal=c(1,0,0), xreg=time(D1))
FitE2

h = 12
forecast2 <- forecast(FitE2, xreg=last(time(D1))+(1:h)/frequency(D1))
forecast2

plot(forecast2)

Y <- D1
window.size <- 80
Arima.order <- c(0,0,2)
pred.plot <- TRUE
include.mean = TRUE
include.drift = FALSE
lambda = 0
xreg = TRUE
seasonal = c(1, 0, 0)

Rolling1step.forecast(Y, window.size, Arima.order, pred.plot, include.mean, include.drift, lambda, xreg, seasonal)

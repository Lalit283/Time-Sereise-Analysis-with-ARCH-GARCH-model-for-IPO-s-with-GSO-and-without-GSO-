# Libraries
#install.packages("quantmod")
library(quantmod)
#install.packages("xts")
library(xts)
#install.packages("nortsTest")
library(nortsTest)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("rugarch")
library(rugarch)
library(prophet)
library(readxl)
library(ggplot2)
set.seed(123)
#  with GSO stock prices
With_GSO <- read_excel("GSO IPO Abnormal returns.xlsx")
head(With_GSO)
gso<- With_GSO[,20]
head(gso)
str(gso)
date = seq(from = as.Date("2021-01-01"), 
           to = as.Date("2021-03-01"), by = 'day')
gso = xts(gso,date)
gso = as.zoo(gso)
chartSeries(gso)
hist(With_GSO$`Mean AR`)
chart.Histogram(With_GSO$`Mean AR`, 
                main = "Histogram for Daily stock price for with GSO",
                methods = c('add.density', 'add.normal'))


################################################
################################################
################################################
################################################
par(mfrow = c(2,1))
plot(gso ,type = "l",xlab = "Day",ylab ="With GSO" ,main = "Original 'With GSO' data")
plot(diffinv(gso),type = "l",xlab = "Day",ylab ="diff invese With GSO" ,main = "Diff inverse 'With GSO' data")
par(mfrow = c(1,1))
plot(With_GSO$`Mean AR`[1:30],type = "l")
plot(With_GSO$`Mean AR`[31:60],type = "l")
library(tseries)
library(lmtest)

## ADF test for Statinarity 

# ADF test for first 30 rows 
adf.test(With_GSO$`Mean AR`[1:30]) # non stationary 
# ADF test for last 30 rows
adf.test(With_GSO$`Mean AR`[31:60]) # non stationary 
# ADF test for overall dataset
adf.test(gso) # stationary 

## LM test for ARCH/GARCH model
Lm.test(gso,lag.max = 2,alpha = 0.05) # or
Lm.test(With_GSO$`Mean AR`[1:30],lag.max = 2,alpha = 0.05)

# Daily returns
return <- gso 
return <- as.zoo(return) 
return1 <- CalculateReturns(gso$`Mean AR`[1:30])
return1 <- return1[-1]
hist(return)
chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return, theme = 'white')


# 1. sGARCH model with contant mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return1, spec = s);m
plot(m,which = "all")
f <- ugarchforecast(fitORspec = m, n.ahead = 20)
plot(fitted(f))
plot(sigma(f))



# 2. GARCH with sstd
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m,which = "all")

# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m,which="all")

#4. AR(1) GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m, which = "all")

#5. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm =T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m, which = "all")

# Simulation
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return, spec = s)
plot(m, which  = 'all')
plot(m, which = 3)

garchvol = sigma(m)
plot(garchvol, main = "Volatility in standard deviation of abnormal returns IPO's with GSO")
qplot(x=c(1:30),y=garchvol[1:30],main ="Volatility in standard deviation of abnormal returns IPO's with GSO")+geom_line(color = "#00AFBB", size = 1)+
  xlab("Days")+ ylab("Volatality")
sfinal <- s
setfixed(sfinal) <- as.list(coef(m))
sfinal
f <- ugarchforecast(data = return,
                        fitORspec = sfinal,n.ahead = 20)

par(mfrow = c(1,1))
plot(fitted(f))

plot(sigma(f))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
tail(gso)
p <- (-0.003275611)*apply(fitted(sim), 2, 'cumsum') + (-0.003275611)
matplot(p, type = "l", lwd = 3)



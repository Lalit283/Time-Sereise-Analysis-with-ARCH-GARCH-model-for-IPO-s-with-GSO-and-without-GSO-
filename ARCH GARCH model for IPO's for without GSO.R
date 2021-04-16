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
library(readxl)
#  with without_gso stock prices
Without_without_gso <- read_excel("Non GSO IPO Abnormal returns.xlsx")
head(Without_without_gso)
without_gso<- as.data.frame(Without_without_gso[,260])

length(without_gso)
str(without_gso)
date = seq(from = as.Date("2021-01-01"), to = as.Date("2021-01-30"), by = 'day')
length(date)
df <- data.frame(Without_without_gso$`Mean Daily Abnormal Return`,date)
df 
class(df)
without_gso = xts(without_gso,date)
without_gso = as.zoo(without_gso)
chartSeries(without_gso)
hist(without_gso)
chart.Histogram(without_gso,main = "Histogram for Stock price without GSO",
                methods = c('add.density', 'add.normal'))

length(without_gso)
################################################
################################################
################################################
################################################
par(mfrow = c(2,1))
plot(without_gso ,type = "l",xlab = "Day",ylab ="With without_gso" ,main = "Original 'With without_gso' data")
plot(diffinv(without_gso),type = "l",xlab = "Day",ylab ="diff invese With without_gso" ,main = "Diff inverse 'With without_gso' data")
par(mfrow = c(1,1))
plot(without_gso,type = "l")
library(tseries)
library(lmtest)
### ADF test for check of Statinarity

adf.test(without_gso)
Lm.test(without_gso,lag.max = 2,alpha = 0.05)

# Daily returns
library(tibble)
return <- df %>% column_to_rownames('date')
return1 <- CalculateReturns(without_gso$`Mean Daily Abnormal Return`)
return1 <- return1[-1]
head(return)
hist(return$Without_without_gso..Mean.Daily.Abnormal.Return.)
chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return, theme = 'white')

# Annualized volatility
chart.RollingPerformance(R = return,
                         width = 2,
                         FUN = "sd.annualized",
                         main = "IPO's with GSO  rolling volatility")

# 1. sGARCH model with contant mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return1, spec = s);m
f <- ugarchforecast(fitORspec = m, n.ahead = 20)
plot(fitted(f))
plot(sigma(f))


# 2. GARCH with sstd
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m, which = "all")
plot(m, which = 3)

# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return1, spec = s);m
plot(m,which="all")

#4. AR(1) GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'std')
m <- ugarchfit(data = return1, spec = s);m
plot(m, which = "all")

#5. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm =T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'std')
m <- ugarchfit(data = return1, spec = s);m
plot(m, which = "all")
#####
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'std')
m <- ugarchfit(data = return, spec = s);m
plot(m, which  = 'all')

##### Volatility Plot 
garchvol = sigma(m)
plot(garchvol,main = "Volatility in standard deviation of abnormal returns IPO's without GSO")
garchval=as.data.frame(garchvol)
garchval=garchval$V1
library(ggplot2)
qplot(x=c(1:30),y=garchval, main = "Volatility of standard deviation of abnormal returns IPO's with GSO", xlab = "Days", ylab = "Volatility" )+
  geom_line(color = "#00AFBB", size = 1)

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
                  n.sim = 1*5,
                  rseed = 123)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
tail(gso)
p <- (-0.003275611)*apply(fitted(sim), 2, 'cumsum') + (-0.003275611)
matplot(p, type = "l", lwd = 3)

# Application example - portfolio allocation
v <- sqrt(1) * sigma(m)
w <- 0.1/v
plot(merge(v, w), multi.panel = T)



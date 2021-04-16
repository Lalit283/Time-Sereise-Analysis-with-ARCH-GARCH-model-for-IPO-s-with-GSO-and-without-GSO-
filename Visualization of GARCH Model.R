## Import libraries 
#install.packages("lubridate")
library(lubridate)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("ggthemes")
library(ggthemes)
library(readxl)
## create sine wave with trend
library(forecast)
library(stats)
library(devtools)
#install.packages("MARSS")
library(MARSS)
df <- read_excel("GSO IPO Abnormal returns.xlsx")
df
str(df)
x <- as.numeric(df$Day)
y <- as.numeric(df$`Mean AR`)
###
qplot(x[1:30], y[1:30], na.rm=TRUE,
      main="IPO's with GSO vs 30 days",
      xlab="Days", ylab="IPO's with GSO")+
  geom_line(color = "#00AFBB", size = 1)

###
# plot with GSO Add trend smoothed line
qplot(x[1:30], y[1:30], na.rm=TRUE,
      main="IPO's with GSO vs 30 days",
      xlab="Days", ylab="IPO's with GSO")+
  geom_line(color = "#00AFBB", size = 1)+
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess")

# Before after simple plot
par(mfrow = c(2,1))
plot(x[1:30],y[1:30],main="IPO's with GSO vs Before 30 days",
     xlab="Days", ylab="IPO's with GSO",type = "l")
plot(x[31:60],y[31:60],main="IPO's with GSO vs After 30 days",
     xlab="Days", ylab="IPO's with GSO", type = "l")

# Before and After 30 days plots in single plot
qplot(x, y, na.rm=TRUE,xlab="Days", ylab="IPO's with GSO")+
  geom_line(color = "#00AFBB", size = 1)+
  geom_vline(xintercept =30, linetype="dashed", color = "red")+
  labs(title = "IPO's with GSO Before and After Stabilization")     

#####################################################################
#####################################################################
#####################################################################
##  Dataset Import for Non GSO IPO's Abnormal Returens             ##

df1 <- read_excel("Non GSO IPO Abnormal returns.xlsx")
df1
str(df1)
x1 = as.numeric(df1$Day)
y1 = as.numeric(df1$`Mean Daily Abnormal Return`)

# Plot IPO's without GSO


qplot(x1, y1, na.rm=TRUE,
      main="IPO's without GSO vs 30 days",
      xlab="Days", ylab="IPO's without GSO")+
  geom_line(color = "#00AFBB", size = 1)

qplot(x1, y1, na.rm=TRUE,
      main="IPO's without GSO vs 30 days",
      xlab="Days", ylab="IPO's without GSO")+
  geom_line(color = "#00AFBB", size = 1)+
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess")

#ACF / PACF plot for IPO's with GSO
par(mfrow = c(1,1))
acf(df$`Mean AR`, lag.max = 35, main = "ACF Plot for IPO's with GSO")
pacf(df$`Mean AR`,lag.max = 35, main = "PACF Plot for IPO's with GSO")

## ACF/PACF plot for after and before 30 days 
par(mfrow = c(2,2))
acf(x[1:30], lag.max = 35, 
    main = "ACF Plot for IPO's with GSO Before 30 days")
pacf(x[1:30],lag.max = 35,
     main = "PACF Plot for IPO's with GSO Before 30 days")

acf(x[31:60], lag.max = 35, 
    main = "ACF Plot for IPO's with GSO After 30 days")
pacf(x[31:60],lag.max = 35,
     main = "PACF Plot for IPO's with GSO After 30 days")


#ACF / PACF plot for IPO's without GSO
par(mfrow = c(1,1))
acf(df1$`Mean Daily Abnormal Return`, lag.max = 35, main = "ACF Plot for IPO's without GSO")
pacf(df1$`Mean Daily Abnormal Return`, lag.max = 35, main = "PACF Plot for IPO's without GSO")



## set up plot area
par(mfrow = c(1, 2))


## better ACF plot
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}

## plot line
par(mfrow = c(1, 2))
plot.ts(df1$`Mean Daily Abnormal Return`, ylab = expression(italic(x[t])))
## get ACF
sili_acf <- acf(df1$`Mean Daily Abnormal Return`,main = "The correlogram of its ACF",plot = F)
## plot ACF
plot.acf(sili_acf)




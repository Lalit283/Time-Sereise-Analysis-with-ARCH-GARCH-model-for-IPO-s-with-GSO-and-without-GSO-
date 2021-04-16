## Import Dataset
#install.packages("readxl")
library(readxl)
df <- read_excel("GSO IPO Abnormal returns.xlsx",sheet = 1)
df1 <- read_excel("Non GSO IPO Abnormal returns.xlsx")


# Extract needed data from both dataset
x <- df$`Mean AR`[1:30]
y <- df1$`Mean Daily Abnormal Return`
head(x)
head(y)
tail(x)
tail(y)


## Length of x and y
length(x)
length(y)


# F-Test: Compare Two Variances in R
# F-test is used to assess whether the variances of two populations (A and B) are equal.
var.test(x,y, alternative = "two.sided")


## To find the mean and variance 
Mean_x <- mean(x);Mean_x
Mean_y <- mean(y);Mean_y
var_x <- var(x);var_x
var_y <- var(y);var_y




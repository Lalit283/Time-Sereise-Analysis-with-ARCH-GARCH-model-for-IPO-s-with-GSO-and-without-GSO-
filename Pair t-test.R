library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
## Import dataset
df <- read_excel("GSO IPO Abnormal returns.xlsx")
head(df)
df
str(df)
summary(df)
before <- df[1:30,20]
after <- df[31:60,20]
after <- after
id = c(1:30)
df1=data.frame(id,before,after)
head(df1)
## Colunm name rename 
df1 <-df1 %>% 
  rename(
    Before = Mean.AR,
    After = Mean.AR.1
  )

class(df1)
str(df1)
head(df1)

# Transform into long data: 
# gather the before and after values in the same column
stack_data <- df1 %>%
  gather(key = "group", value = "weight", Before, After)
head(stack_data, 3)
dim(stack_data)
#First, start by computing the difference between groups:
df1 <- df1 %>% mutate(differences = Before - After)
head(df1, 3)
#Identify outliers
#Outliers can be easily identified using boxplot methods, implemented in the R function identify_outliers() [rstatix package].
df1 %>% identify_outliers(differences)

#Check normality by groups
#The normality assumption can be checked by computing the Shapiro-Wilk test for each group. If the data is normally distributed, the p-value should be greater than 0.05.
df1 %>% shapiro_test(differences) 

## Conclusion ##
#From the output, the two p-values are greater than the significance level 0.05 indicating that the distribution of the data are not significantly different from the normal distribution. In other words, we can assume the normality.

#You can also create QQ plots for each group. QQ plot draws the correlation between a given data and the normal distribution

ggqqplot(df1, "differences")
## Conclustion ## 
#All the points fall approximately along the (45-degree) reference line, for each group. So we can assume normality of the data.



# Paired sample t-test
stack_data %>% t_test(weight ~ group, paired = TRUE)

# Compute t-test
res <- t.test(df1$Before, df1$After, paired = TRUE)
res














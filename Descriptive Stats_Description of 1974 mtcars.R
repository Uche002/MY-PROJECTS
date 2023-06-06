#------------
# MSc Assessment Project
#------------

#------------
#View mtcars data set details
#------------

?mtcars

#------------
#
#BRIEF SUMMARY of qsec, wt, carb COLUMNS FROM mtcars DATA SET
#
#------------

#------------
#1. Filter out columns 6(wt), 7(qsec), and 11(carb) from main data frame,
#using column number
#------------

library(dplyr)
df<-mtcars
df
df[c(6, 7, 11)]

#------------
#2. Descriptive Stats; summary of filtered columns (6, 7 and 11)
#------------

summary(df[c(6, 7, 11)])
summary(df$wt)
summary(df$qsec)
summary(df$carb)

#------------
#
#Descriptive Graphics using summary of filtered columns (wt,qsec,carb)
#
#------------
#Boxplot graphics using wt column summary
#------------

boxplot(df$wt, col = "coral")
boxplot(df$wt, xlab = "X-cars distribution",
        ylab = "Y-cars wt distribution (lbs)", las = 1,
        col = "coral",main= "wt of cars in mtcars record of 1974")

locator(1)
text(1.238936,1.56236, "min")
locator(2)
text(1.261627,2.604957, "1st Qrt")
locator(3)
text(1.261627,3.307577, "Median")
locator(4)
text(1.261627,3.760881, "3rd Qrt")
locator(5)
text(1.225321,5.188786, "max")

#-----------
#Descriptive Stats for wt column; Median and Interquartile
#------------

#------------
#Median
#------------

med.len = median(df$wt)
med.len

#------------
#Interquartile Range
#------------
med.q1=quantile(df$wt, 0.25)
med.q1 
med.q3=quantile(df$wt, 0.75)
med.q3 
Int.range= IQR(df$wt,na.rm = FALSE, type = 7)
Int.range

#------------
#histogram graphics using qsec column summary
#------------

hist(df$qsec, col="darkseagreen1", pch=2, cex="1",
     ylab="Frequency distribution (lbs)", las = 1, xlab = "X-cars",
     main="qsec of cars in mtcars record of 1974")

qsec.mean = mean(df$qsec)  
qsec.mean
qsec.sd = sd(df$qsec)
qsec.sd
std = qsec.sd / sqrt(length(qsec.sd))
std

abline(v = qsec.mean,                      
       col = "red",
       lwd = 3)
text(x = qsec.mean * 1.2,                   
     y = qsec.mean * 1.2,
     paste("Mean =", qsec.mean),
     col = "red",
     cex = 2)
locator(1)
text(22.05856,6.163402, "Positive Skew")
locator(2)
text(19.20696,9.588137,"Mean = 17.85", col = "Red")

#--------------
#Barplot using carb data from mtcars
#--------------

mean.carb = mean(df$carb, na.rm = TRUE)
mean.carb
sd.carb = sd(df$carb, na.rm = TRUE)
sd.carb

freq.cars = table(df$carb)
freq.cars
bar.carb = barplot(freq.cars, col = "aquamarine1", las = 1,
                   ylab = "Frequency distribution", xlab = "Carb variables",
                   main = "Distribution of Caburetors of mtcars data in 1974")




### CORRELATION ASSIGNMENT

### MSc Assessment Project



############################# QUESTION 1 #######################################



################### Relationship between wt and mpg ############################


## To know the data we are working on,
## we need to load the data set "mtcars". Therefore;


?mtcars                        # A look at mtcars data set details on Rstudio

spd = mtcars                   # Assigning variable name 'spd' to mtcars data.
spd
str(spd)                       # Checking the data structure

View(spd)                      # Before doing any analysis,
                               # its always good to have table look at the data.

## Here we do not know what kind of data set we are dealing with,
## whether or not the weight and miles per gallon variables are Parametric or/
## non-Parametric. Hence we carry out a QQ plot of the predictor variable (wt),
## against the response variable (mpg). What this means is we are trying to see/
## how the mpg variable behaves in response to changes in wt variable.
## If we have parametric or non-parametric, we test for normality,
## if normally distributed, we test for parametric using Pearson's Correlation/
## Coefficient, and if Non-Parametric, we use spearman. Hence:


## Having established weight (wt) as the predictor variable (x-axis),
## and miles per gallon (mpg) as the response variable (y-axis),
## we first, use a scatter plot with the command below to see how mpg responds/
## to changes in wt.


plot.colors = c("Coral","Green" )
color.vector = rep(x=plot.colors, each=1)
color.vector

with(spd,plot(wt ~ mpg, pch=20,cex=3,las=1,col=color.vector,
                main="Relationship between weight and miles per gallon", 
                xlab="Weight (wt)", ylab="Miles per gallon (mpg)"))


## Analysis of the plot: based on the plot result, speculatively as wt/
## decreases, mpg seems to also be decreasing, suggesting a negative linear/
## relationship between both. However, this is not enough to infer whether/
## both variables are Parametric on non-Parametric. Hence we carry out a/
## screening check to test for normality as shown for wt and mpg below:

#Null Hypothesis - Data (the variable named wt) are normally distributed

qqnorm(spd$wt)

?shapiro.test
sh.wt = shapiro.test(spd$wt)
sh.wt

## Report of wt normality test result: 
## wt does not conform to normal distribution as W = 0.94326, p-value = 0.09265,
## hence we accept the null hypothesis that wt variable conforms to normal/
## distribution.

#Null Hypothesis - Data (the variable named mpg) are normally distributed

qqnorm(spd$mpg)

?shapiro.test
sh.mpg = shapiro.test(spd$mpg)
sh.mpg

## Report of mpg normality test result: 
## mpg does not conform to normal distribution as W = 0.94756, p-value = 0.1229,
## hence we accept the null hypothesis that mpg variable conforms to normal/
## distribution.

## Having confirmed both wt and mpg variables are normally distributed,
## it is safe to therefore carry out a parametric test using the Pearson's test,
## as spearman will only be suitable for non-parametric test.

cor.wt = cor.test(spd$wt, spd$mpg, method = "pearson")
cor.wt

## FINAL REPORT BASED ON NULL HYPOTHESIS, THE SCREENING CHECK AND THE COR. TEST

## Zach (2021) suggests that the APA Format of reporting is the best way to/
## report the Pearson Correlation Coefficient.

## 1. THERE IS A SIGNIFICANT NEGATIVE LINEAR RELATIONSHIP BETWEEN wt AND mpg
## Pearson correlation coefficient test was carried out on the predictive/
## variable wt and the response variable (mpg) to establish the relationship/
## between them. At the end of the test, it was seen that there is a negative/
## correlation between wt and mpg, as correlation coefficient; cor = -0.8676594,
## t = -9.559, df = 30, p-value = 1.294e-10
## based on all the analysis carried out, qsec has a weak linear relationship/
## with wt and mpg has a strong negative linear relationship with wt



##################### Relationship Between wt and qsec #######################


## Having established weight (wt) as the predictor variable (x-axis),
## and quarter miles time (qsec) as the response variable (y-axis),
## we then use a scatter plot with the command below to see how qsec responds
## to changes in wt.


plot.colors = c("Coral","grey" )
color.vector = rep(x=plot.colors, each=1)
color.vector

with(spd,plot(wt ~ qsec, pch=20,cex=3,las=1,col=color.vector,
              main="Relationship between weight and quarter miles time", 
              xlab="Weight (wt)", ylab="Quarter miles time (qsec)"))

#P.S: The las function changes the y-axis label from horizontal to vertical

## Analysis of the plot: based on the plot result, speculatively as wt/
## decreases, mpg seems to also be decreasing, suggesting a negative linear/
## relationship between both. However, this is not enough to infer whether/
## both variables are Parametric on non-Parametric. Hence we carry out a/
## screening check to test for normality as shown for wt and mpg below:

#Null Hypothesis - Data (the variable named wt) are normally distributed

qqnorm(spd$wt)

?shapiro.test
sh.wt = shapiro.test(spd$wt)
sh.wt

## Report of wt normality test result: 
## wt does not conform to normal distribution as W = 0.94326, p-value = 0.09265,
## hence we accept the null hypothesis, as wt variable conforms to normal/
## distribution.

#Null Hypothesis - Data (the variable named mpg) are normally distributed

qqnorm(spd$qsec)

?shapiro.test
sh.qsec = shapiro.test(spd$qsec)
sh.qsec

## Report of mpg normality test result: 
## mpg does not conform to normal distribution as W = 0.97325, p-value = 0.5935,
## hence we accept the null hypothesis, as qsec variable conforms to normal/
## distribution.

## Having confirmed both wt and qsec variables are normally distributed,
## it is safe to therefore carry out a parametric test using the Pearson's test.

cor.wt_qsec = cor.test(spd$wt, spd$qsec, method = "pearson")
cor.wt_qsec

## FINAL REPORT BASED ON NULL HYPOTHESIS, THE SCREENING CHECK AND THE COR. TEST


## 1. THERE IS A SIGNIFICANTLY NEGATIVE LINEAR RELATIONSHIP BETWEEN wt AND mpg
## Pearson correlation coefficient test was carried out on the predictive/
## variable wt and the response variable (qsec) to establish the relationship/
## between them. At the end of the test, it was seen that there is a negative/
## correlation between wt and qsec, as correlation coefficient; cor= -0.1747159,
## t = -0.97191, df = 30, p-value = 0.3389



################################ QUESTION 2 ###################################



?iris                           # Checking the iris data on Rstudio


eyeris = iris
eyeris                          # Assigning variable name 'eyeris' to iris data
str(eyeris)                     # Checking the data structure

View(eyeris)                    #Viewing the iris data frame on a table


library(dplyr)                  # trying to seperate the sepal.length, Width/
                                # and Setosa species from the rest data set/
                                # by calling the dplr function so we can filter.

filter.eyeris = eyeris[c(1, 2, 5)]    # specific Column Filtering 
filter.eyeris                       
fil.set = filter.eyeris %>% filter(Species == 'setosa')
fil.set                         #specifically row filtering code


plot.colors = c("Grey","Brown" )     #choosing colour for both variables on the
color.vector = rep(x=plot.colors, each=1)    #plot
color.vector

with(fil.set,plot(fil.set$Sepal.Length ~ fil.set$Sepal.Width, pch=20,cex=3,
                  
                  las=1,col=color.vector,
                  main="Relationship between SL and SW of Setosa  Species", 
              xlab="Sepal Length (SL)", ylab="Sapal Width (SW)"))

## Analysis of the plot: based on plot result, speculatively as Sepal.Length/
## increases, Sepal.Wisth seems to also increase, suggesting a positive linear/
## relationship between both. However, this is not enough to infer whether/
## both variables are Parametric on non-Parametric. Hence we carry out a/
## screening check to test for normality as shown for Sepal.Length and
## Sepal.Width below:

## Null Hypothesis - Data (the variable named Sepal.Length),
## is normally distributed

qqnorm(fil.set$Sepal.Length)

?shapiro.test
sh.sl = shapiro.test(fil.set$Sepal.Length)
sh.sl

## Report of Sepal.Length (SL) of Setosa species normality test result: 
## SL does not conform to normal distribution as W = 0.9777, p-value = 0.4595,
## hence we accept the null hypothesis that SL variable conforms to normal/
## distribution.

#Null Hypothesis -Data (the variable named Sepal.Width) are normally distributed

qqnorm(fil.set$Sepal.Width)

?shapiro.test
sh.sw = shapiro.test(fil.set$Sepal.Width)
sh.sw

## Report of Sepal.Width normality test result: 
## Sepal.Width does not conform to normal distribution,
## as W = 0.97172, p-value = 0.2715,
## hence we accept the null hypothesis that mpg variable conforms to normal/
## distribution.

## For the purpose of this test, Sepal.Length will be SL and Sepal.Width SW.
## Having confirmed both SL and SW variables are normally distributed,
## it is safe to therefore carry out a parametric test using the Pearson's test.

cor.ss = cor.test(fil.set$Sepal.Length, fil.set$Sepal.Width, method = "pearson")
cor.ss


## FINAL REPORT BASED ON NULL HYPOTHESIS, THE SCREENING CHECK AND THE COR. TEST


## 1. THERE IS A SIGNIFICANTLY POSITIVE LINEAR RELATIONSHIP BETWEEN SL AND SW
## Pearson correlation coefficient test was carried out on the predictive/
## variable SL and the response variable SW to establish the relationship/
## between them. At the end of the test, it was seen that there is a positive/
## correlation between SL and SW, as correlation coefficient; cor = 0.7425467,
## t = 7.6807, df = 48, p-value = 6.71e-10
## Sepal.Width has a strong positive linear relationship with Sepal.Length,
## in the Setosa species data set.


############################# REFERENCES #######################################


## R Documentation: Motor Trend Car Road Tests (1974).
## R Documentation: Edgar, A., (n.d)- iris data.
## Zach (2021) - How t filter rows in R. Available online:/
## https://www.statology.org/filter-rows-r/ [Accessed n 13/11/2022]
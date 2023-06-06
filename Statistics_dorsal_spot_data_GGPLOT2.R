
##------------------------------------------------------------------------
### GGPLOT2 ASSIGNMENT

### MSc Assessment Project
##------------------------------------------------------------------------


## Load the dorsal_spot_data into Rstudio


#Variable name 'dors.data' given to the dorsal spot data, will be used to call
#the data dorsal spot data subsequently.


dors.data=read.csv(file.choose()) #Opens a window to choose a file in my laptop
dors.data          

#-----------------------------------------------------
#
## Load the required packages, check the data
#
#-----------------------------------------------------
install.packages("ggpmisc") ##set off extension
install.packages("ggpubr") ##shows colour pallete, enables us to  choose colour

library(ggpmisc)
library(ggpubr)

library(ggplot2)
str(dors.data) # checking the structure of dorsl spot data


## --------------------------QUESTION 1-----------------------------------

# -------------------------------------------
## Null Hypothesis: "ed data are normally distributed"
## Conduct screening checks for ed data
## Test for normality
## ----------------------------------------------

qqnorm(dors.data$ed)
shap.ed1 = shapiro.test(dors.data$ed)
shap.ed1 #significant difference yes, but not normal

#-----------------SCREENING CHECK REPOORT-------------------------
## There is significant difference as the p-value (1.177e-08) is less than 0.05,
## hence we reject the null hypothesis as ed is not normally distributed
#-----------------------------------------------------------------

## Considering there is significant difference as shown by the p-value,
##  we would transform the data using the log function as shown below:

log.ed = log(dors.data$ed)
log.ed
str(log.ed)   #Checking the structure of the transformed ed data

## Reconduct the screening check after transforming the data

qqnorm(log.ed)
shapiro.test(log.ed)

#----------SCREENING CHECK REPORT AFTER ed DATA TRANSFORMATION-------
## After transformation, there is no significant difference,
## as the p-value(0.1256) is greater than 0.05,
## hence we accept the null hypothesis that the data is normally distributed.
#-----------------------------------------------------------------------

#-------------------------------------------------------------------
## Having concluded log.ed data to be normally distributed,
## we then carry out F test to compare 
## variance and know whether or not we have equal variance,
## as this will reveal we a dealing with parametric test,
## when it is equal variance (Bhandari, 2022).
#------------------------------------------------------------------

dors.var = var.test(log.ed ~ dors.data$ds) # variance test
dors.var

t.dors = t.test(log.ed ~ dors.data$ds) #student test for normal data
t.dors

## generate the mean and standard deviation for the required variables 

mean.ed1 = aggregate (log.ed~dors.data$ds,FUN=mean) #checking the mean
mean.ed1

sds.ed1 = aggregate (log.ed~dors.data$ds,FUN=sd) #standard deviation
sds.ed1

data.frame= data.frame(log.ed, dors.data$ds) 
data.frame  #shows only data of both columns of ed and ds


#------------ generate the barplot and error bar from the mean comparison
b = ggplot(data.frame, aes(x = dors.data$ds, y = log.ed, fill = dors.data$ds,))+
  geom_bar(stat = "summary", fun = "mean")+
  labs(title = "Figure of mean comparison of ed vs ds (N/Y)",
       x = "dorsal spot present (N/Y)",
       y = "Transformed ed",)+
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)+
  theme_bw()

b       #assigned variable name 'b' to the ggplot2 above for ease of reference

##---------------------SCIENTIFIC REPORT OF RESULTS--------------------------

## According to Bhandari (2022), the following reporting format is how to report
## statistical results scientifically:
## Mean and Standard Deviation Report: The average mean for the dsd N/Y data is
## 0.110 (SD = 0.6889). The means were -0.3655 and 0.1457 respectively (SD = 
## 0.6806 and 0.6971 respectively).
## T-test and Confidence Level (Cl) Report: dsd not present tends to negative
## as t = -3.67, p = 0.0004, d = 95.59, 95% Cl [-0.788, -0.235].

##---------------QUESTION 2----------------------------------

library(dplyr) #Will help in data manipulation; filtering of dors.data

dos2 = dors.data #Attaching a new variable name to our data frame
dos2
filter.dos2 = dos2[c(15, 16)] #Filtering out the ed(15) and dsd (16) columns
filter.dos2

remove_na=na.omit(filter.dos2) # Removing the NA cells in our filtered data
remove_na
str(remove_na)      #checking the structure of our data after removing NA cells


## ---------------------- Conduct screening checks for ed & dsd ---------------

## ------------ Test for normality ----------------
## Null hypothesis - "eye diameter (ed) data are normally distributed"
#------------------------------------------------------------------------------

qqnorm(remove_na$ed)      #Normal Q-Q Plot
shap.ed.1 = shapiro.test(remove_na$ed)#Conducting shapiro-test to reveal p-value
shap.ed.1       #significant,but not normal

##----------Screening Check Report------------------
## data is not normally distributed as it has some level of significance,
## with p-value (1.177e-08) < 0.05. Hence there is need to transform data,
## to see if we can get a normal distribution.
##---------------------------------------------------

## Transforming eye diameter (ed) data

log.ed.1= log(remove_na$ed)
log.ed.1
str(log.ed.1)   #checking the structure of transformed ed data

##-------------------------------------------------------
## transformed eye diameter screening check for normality
##--------------------------------------------------------

qqnorm(log.ed.1)  #Normal Q-Q Plot
shap.ed.2 = shapiro.test(log.ed.1) #Shapiro-test to reveal significance level
shap.ed.2 

##----------Screening Check Report for Transformed eye diameter data-----------
## After transforming ed,
## data has no level significance with p-value (0.20788) > 0.05,
## hence we accept the original null hypothesis of ed to be normally distributed
##-------------------------------------------------------------------------


##---------------------------------------------------------------------
## Null Hypothesis- "Dorsal spot diameter data is normally distributed".
##----------------------------------------------------------------------

qqnorm(remove_na$dsd) # #Normal Q-Q Plot for dsd
shap.dsd.1 = shapiro.test(remove_na$dsd) ##Shapiro-test to reveal p-value
shap.dsd.1       #significant, but not normal

##---------------------------------------------------------------------
## Transforming dorsal spot diameter (dsd) data using the log function
##---------------------------------------------------------------------
log.dsd.1= log(remove_na$dsd)
log.dsd.1
str(log.dsd.1)    #Checking the new structure of transformed dsd data

## Reconduct the screening check after dsd data transformation
qqnorm(log.dsd.1) ##Normal Q-Q Plot for transformed data
shap.dsd.2 = shapiro.test(log.dsd.1) #shapiro test on tranformed dsd data
shap.dsd.2    

##----------Screening Check Report for Transformed dsd data-----------
## After transforming dsd,
## data has no level significance with p-value (0.07595) > 0.05,
## hence we accept the original null hypothesis dsd to be normally distributed
##----------------------------------------------------------------------------

##------------------------------------------------------------------------
## using appropriate GGPLOT2 to represent transformed ed and dsd data
## we assign a variable name 'ds' to the figure, for ease of reference
##-----------------------------------------------------------------------

ds=ggplot(remove_na,aes (x = log.ed.1, y = log.dsd.1, col=log.dsd.1))+
  geom_point()+           #simply means a scatter plot
  geom_smooth(method=lm)+
  theme_classic()+
  labs(x = "Transformed eye diameter (ed)",
       y = "Transformed dorsal spot diameter(dsd)")+
  theme(axis.text.x = element_text (color = "black", size = 10),
        axis.text.y = element_text (color = "black", size = 10))+
  labs(title = "eye diameter vs dorsal spot diameter ",)+
  theme(plot.title = element_text(hjust=0.5,size = 15))+
  theme(legend.position=c(1,1), legend.justification=c(1,2.5))

ds    #assigned variable name 'd' to ggplot2 above, for ease of reference
## P.S: aes stands for aesthetics, it beautifies our plot, the rest code serves
## to add text to x and y-axis, legend, figure title and axis title

## run a simple linear model of normally distributed ed and dsd data
lm.dors_spt = lm(log.ed.1 ~ log.dsd.1)

lm.dors_spt

#summarize linear model results
summary(lm.dors_spt)

#plot all graphs in one window, for easy visualization
par(mfrow=c(2,2))
plot(lm.dors_spt)

## ----------------------------SCIENTIFIC REPORT OF LINEAR MODEL-------------
## According to Zach (2022), the follwing is how to report regression results:
## Simple linear regression was used to test if ed 
## appropriately predicts ds.
## The overall regression was statistically significant (R2 = 0.746,
## F(df regression, df residual) = 129.1 on 1 and 44 DF, p = 1.13e-14).


############################# REFERENCES #######################################


## Bhandari, P., (2022) - What is Variance| Definition, Examples & Formulas.
## Available online: https://www.scribbr.com/statistics/variance/
## #:~:text=Variance%20is%20important%20to%20consider%20before%20performing%20
## parametric,samples%20result%20in%20biased%20and%20skewed%20test%20results.
## [Accessed 05/12/2022].

## Bhandari, P., (2022) - Reporting Statistics in APA Style| Guidelines and
## Examples.
## Available online: https://www.scribbr.com/apa-style/numbers-and-statistics/
## [Accessed 05/12/2022]

## NNK, (2022) - Hpw to Remove Rows with NA in R. Available online:
## https://sparkbyexamples.com/r-programming/remove-rows-with-na-in-r/
## #:~:text=By%20using%20na [Accessed 5/12/2022].

## Zach (2021) - How t filter rows in R. Available online:/
## https://www.statology.org/filter-rows-r/ [Accessed 13/11/2022].

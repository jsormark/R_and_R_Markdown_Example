#title: Income and Democracy Analysis
#author: "John Sormark"

rm(list = ls())


### Loading packages
library(tidyverse)
library(doBy)
library(foreign)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(AER)
library(gdata)
library(wooldridge)
library(openintro)


cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

# Reading CSV data file.
incdemo=read.csv("incdemo1995.CSV", header=T, sep=",")


#Descriptive statistical analysis of identified variables. 
stargazer(incdemo[c("log_gdppc", "dem_ind", "log_pop", "educ", "age_median")], type="text", digits=2 ,summary.stat=c("n", "mean", "median", "sd", "min", "max"), title="Income and Democracy Descriptive Statistics", flip=FALSE, covariate.labels=c("log gdppc", "dem ind", "log pop", "educ", "age median"))



# creates a scatterplot of log of real GDP per capita against the democracy index. 
ggplot(incdemo, aes(x=log_gdppc, y=dem_ind)) + geom_point(col="blue") + geom_text(aes(label=country), hjust=1, vjust=1, size=2) +
  labs(title = "Index of Democracy verses Logarithm of Real GDP per Capita", x = "Log GDPPC", y = "Index of Democracy") + geom_smooth(method = "lm", col = "red", se=FALSE)


# Runs a simple regression of log of real GDP per capita on the democracy index.
regr1<-lm(log_gdppc~dem_ind, data=incdemo)
# Runs multiple regressions of log of real GDP per capita on the democracy index, log of population, education and median age (variations of, as noted in formulations).
regr2<- lm(log_gdppc~dem_ind+log_pop, data=incdemo)
regr3<-lm(log_gdppc~dem_ind+log_pop+educ, data=incdemo)
regr4<-lm(log_gdppc~dem_ind+log_pop+educ+age_median, data=incdemo)

# displays the regression results
stargazer(regr1, regr2, regr3, regr4, se=list(cse(regr1), cse(regr2), cse(regr3), cse(regr4)), 
          title="Income and Democracy Regression Results", type="text", 
          star.cutoffs=NA, df=FALSE, digits=3, omit.stat=c( "f"))

#Input to run command predict of the regression results with the terms input below.

#Creating the new data frame with the updated values for the predict function. 
newpredict1 <- data.frame(dem_ind = 1, log_pop = log(300000), educ = 12, age_median = 35)

newpredict2 <- data.frame(dem_ind = 0, log_pop = log(300000), educ = 12, age_median = 35)

#Executing the predictions
prediction1 <- predict(regr4, newdata = newpredict1)
prediction2 <- predict(regr4, newdata = newpredict2)

#Output the results.
print(paste("The predicted log of real GDP per capita with a democracy index equal to one:", prediction1))
print(paste("The predicted log of real GDP per capita with a democracy index equal to zero:", prediction2))


#Exponentiating the results to convert predictions of log real GDP per capita to real GDP per capita. 
log_prediction1 <- predict(regr4, newdata = newpredict1)
log_prediction2 <- predict(regr4, newdata = newpredict2)

#Accounting for correct units of scale.
prediction1 <- exp(log_prediction1)
prediction2 <- exp(log_prediction2)

#Output for the predictions and calculated difference.
print(paste("The predicted real GDP per capita with a democracy index equal to one:", prediction1))
print(paste("The predicted real GDP per capita with a democracy index equal to zero:", prediction2))

difference <- prediction1 - prediction2
print(paste("Calculated difference in the predicted real GDP per capita between a democracy index of one and zero:", difference))

# Hypothesis test on updated regression variables, as noted.
regr5<-lm(log_gdppc~dem_ind+educ, data=incdemo)
#Linear hypothesis test for F-statistic and p-value outputs.
lht(regr5,c("dem_ind=0", "educ=0"), white.adjust="hc1")
#Load the relevant package for running regression
library(lmtest)
library(devtools)
library(dma)
#install_github('JeffreyRacine/R-Package-ma')
#categorical regression spline
library(crs)
#model averaging
#library(ma)

library(MASS)
library(BMA)

#Read the data file
#=================================================
NewData <- data.frame(read.csv("New Data.csv"))
#==================================================


# SCATTER PLOTS and PAIRWISE CORRELATION Analysis 
#==================================================

# NIFTY Bank Return vs FII 
plot(NIFTY_Bank_Return ~ FII, data=NewData, xlab="FII", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ FII, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$FII, data=NewData, method = "pearson")
#positive correlation between FII and Bank NIFTY returns

# NIFTY Bank Return vs Change in Foreign Exchange Reserves 
plot(NIFTY_Bank_Return ~ FER_Change, data=NewData, xlab="FER Change", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ FER_Change, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$FER_Change, data=NewData, method = "pearson")
#positive correlation between change in FER and returns on Bank NIFTY 

# NIFTY Bank Return vs Change in M3 Money supply 
plot(NIFTY_Bank_Return ~ M3Change, data=NewData, xlab="Change in M3 money supply", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ M3Change, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$M3Change, data=NewData, method = "pearson")
#No correlation between Change in M3 money supply and Bank NIFTY returns. 

# NIFTY Bank Return vs one period Lagged value of Change in M3 Money supply 
plot(NIFTY_Bank_Return ~ M3ChangeLag, data=NewData, xlab="Lagged change in M3 Money supply", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ M3ChangeLag, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$M3ChangeLag, data=NewData, method = "pearson")
#No significant correlation between Lagged change in M3 Money supply and Bank NIFTY returns

# NIFTY Bank Return vs DJIA return 
plot(NIFTY_Bank_Return ~ DJIA_return, data=NewData, xlab="Return on DJIA", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ DJIA_return, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$DJIA_return, data=NewData, method = "pearson")
#positive correlation between DJIA returns and Bank NIFTY returns

# NIFTY Bank Return vs Change in Fx rate (USD/INR)  
plot(NIFTY_Bank_Return ~ FX_Rate_Change, data=NewData, xlab="Change in USD/INR (INR per USD)", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ FX_Rate_Change, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$FX_Rate_Change, data=NewData, method = "pearson")
#No significant correlation between Change in USD/INR rate and Bank NIFTY returns

# NIFTY Bank Return vs 1 period Lagged Change in Fx rate (USD/INR)  
plot(NIFTY_Bank_Return ~ FxRateChangeLag, data=NewData, xlab="Lagged Change in USD/INR (INR per USD)", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ FxRateChangeLag, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$FxRateChangeLag, data=NewData, method = "pearson")
#Negative correlation between Lagged Change in USD/INR rate and Bank NIFTY returns

# NIFTY Bank Return vs IIP General Index  
plot(NIFTY_Bank_Return ~ IIPG, data=NewData, xlab="IIP General Index", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ IIPG, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$IIPG, data=NewData, method = "pearson")
#No correlation between IIP General Index and Bank NIFTY returns

# NIFTY Bank Return vs 1 period lagged IIP General Index  
plot(NIFTY_Bank_Return ~ IIPG_Lag, data=NewData, xlab="Lagged IIP General Index", ylab="Return on NIFTY Bank")
abline(lm(NIFTY_Bank_Return ~ IIPG_Lag, data=NewData), col="red")
cor.test(NewData$NIFTY_Bank_Return, NewData$IIPG_Lag, data=NewData, method = "pearson")
#No correlation between IIP General Index and Bank NIFTY returns

#==================================================

sample_rows <- sample(60, 55)

train_data <- NewData[sample_rows,]
dim(train_data)
#summary(train_data)
test_data <- NewData[-sample_rows,]
dim(test_data)
#summary(test_data)




#MULTIPLE REGRESSION ANALYSIS
#==================================================

Fit1 <- lm(NIFTY_Bank_Return ~ FER_Change + M3ChangeLag + FxRateChangeLag + IIPG_Lag, data = NewData)
summary(Fit1)
#R-square of 49.84%. The independent variables: "Change in FER", "Lagged change in M3 Money supply" and "Lagged valued of IIP General Index" are insignificant ind etermining the Return on NIFTY BANK Index

# Dropping the insignificant variables and running the MULTIPLE REGRESSION
Fit2 <- lm(NIFTY_Bank_Return ~ FII + DJIA_return + FxRateChangeLag, data = NewData)
summary(Fit2)
# R square of 46.5%.
# All the independent variables: "FII", "Lagged change in USD/INR", "and Return on DJIA" are significant in determining the "Return on NIFTY Bank"
# FII and Return on DJIA have positive slope coefficients. This iumples Increase in FII and postive returns on DJIA , leads to higher returns on NIFTY Bank.
# But Lagged change in USD/INR has a negative coefficient. This means when INR depreciates w.r.t. USD, the return on NIFTY Bank decreases.

#==================================================
#BAYESIAN MODEL AVERAGING 
#==================================================

y <- NewData[,9]
x <- NewData[,c(1,3,5,12,13,15)]

lma<- bicreg(x, y, strict = FALSE, OR = 20)
summary(lma)
#The Best 5 models, picked on the basis of BIC criteria account for a cumulative posterior probability of ~ 80%
# The R square of all the 5 models hovers between 41% to 48%
# IIP_Lag and FER_Change have coefficients of zero for all the models
# Instead of providing a posint estimate for each slope coefficient, the BMA provides a distribution of the slope coefficient with expected value and standard deviation
# two variables: IIPG_lag and FER_Change have been excluded in all the 5 models.





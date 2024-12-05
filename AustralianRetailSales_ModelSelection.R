###Analysing Australian retail sales data 

#Loading necessary libraries
library(forecast)
##a. Reading the excel and loading the dataset
retaildata <- readxl::read_excel("AustralianRetailSales.xlsx", skip=1)
##b. Selection
Southfood <- ts(retaildata[,"A3349398A"], frequency=12, start=c(1982,4))
##c. Exploration
autoplot(Southfood)
ggseasonplot(Southfood)
ggsubseriesplot(Southfood)
gglagplot(Southfood)
ggAcf(Southfood)
#Can you spot any seasonality, cyclicity and trend? What do you learn about the series?

###2. For the chosen retail time series:
##a. Splitting the data
Southfood.train <- window(Southfood, end=c(2010,12))
Southfood.test <- window(Southfood, start=2011)
##b. Verifying the split is carried out effectively.
autoplot(Southfood) +
autolayer(Southfood.train, series="Training") + autolayer(Southfood.test, series="Test")
##c. Forecasting using snaive
fc <- snaive(Southfood.train)
##d. Comparison of accuracy for our forecast wrt test set data
accuracy(fc,Southfood.test)

##Comment on the results.

##e. Check the residuals.
checkresiduals(fc)
##Do the residuals appear to be uncorrelated and normally distributed?

##f. How sensitive are the accuracy measures to the training/test split?
###Decreased training set
Sf.train <- window(Southfood, end=c(2009,12))
Sf.test <- window(Southfood, start=2010)
fc1 <- snaive(Sf.train)
##d. Comparison of accuracy for our forecast wrt test set data
accuracy(fc1,Sf.test)
###Increased training set
Sf.train2 <- window(Southfood, end=c(2011,12))
Sf.test2 <- window(Southfood, start=2012)
fc2 <- snaive(Sf.train2)
##d. Comparison of accuracy for our forecast wrt test set data
accuracy(fc2,Sf.test2)

##g. Apply auto.arima() and repeat the steps (c, d, and e)
fc3 <- auto.arima(Southfood,seasonal = TRUE)
accuracy(fc3)
checkresiduals(fc3)

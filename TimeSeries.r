# Load the data frame using tsv.  
#This data was initially used for my data visualization course where I developed a dashboard to explore industry trends visually
#Link is here http://bit.ly/1j4LGfv
data <- "C:/Users/gerson64/Documents/GitHub/R_TimeSeries/Census_Retail_and_Trade_Data.txt"
CensusData <- read.delim(data,header =TRUE,sep="\t", stringsAsFactors = FALSE)
newcar<-unique(CensusData$Kind.of.Business)[4]#New Car Dealers
newCarData <- CensusData[CensusData$Kind.of.Business== newcar,]
newCarData<-data.frame(as.Date(newCarData$Date,"%m/%d/%Y"),newCarData$Value)
##Goal to enforce stationarity on a model with 
plot(newCarData, type="l")
newCarPurchases<-newCarData[,2]
census_newcar_TS<-ts(newCarPurchases,start=c(1992,1), frequency = 12)
#The below data has separate, non seasonal data aspects that need to be accounted for.  
#The main non season aspect of this data is the general positive increase in the economy.  
#Differencing in this case is used to enforce stationarity on the data.  Ideally differencing will create data that is stationary with all aspects of the data but thetime-series components filtered out.
diff(census_newcar_TS,differences=1)
acf(census_newcar_TS, lag.max=12)
acf(diff(census_newcar_TS,differences=1), lag.max=24)
acf(diff(census_newcar_TS,differences=2), lag.max=24)
#The difference in the ACF plots is minimal between difference 1 and 2
# A augmented dickey fuller test is used to quantify this assumption
library(fUnitRoots)
adfTest(diff(census_newcar_TS,differences=1),lags=12,type = "ct")
#This model is acceptanle but starting with a lag of 12 is very aggressive.  While trying to model this data better the hope is to create a more parsimonious design.
adfTest(diff(census_newcar_TS,differences=1),lags=1,type = "ct")
#The ACF test indicates that there are large spikes at 0 and 12.  This (along with the fact this is monthly data)  says this needs a seasonal model.
tsdisplay(diff(census_newcar_TS,differences=1),main="")
#THe model
test<-arima(census_newcar_TS, order = c(0,1,0), seasonal = list(order=c(0,0,0)))
Acf(residuals(test))
#The below function appears to get rid of most of the seasonal discrepancy but the ACF at lag 12 but there is still a high residual at lag 1
test<-arima(census_newcar_TS, order = c(0,1,0), seasonal = list(order=c(1,0,0)))
tsdisplay(residuals(test))
summary(test)
#The model originally has a lag at  1 which can be solved by MA1
test1<-arima(census_newcar_TS, order = c(0,1,1), seasonal = list(order=c(1,0,0)))
tsdisplay(residuals(test1))
summary(test1)
#The AIC does decrease slightly when AR 1 is added , this doesn't seem as neccesary though
test2<-arima(census_newcar_TS, order = c(1,1,1), seasonal = list(order=c(1,0,0)))
tsdisplay(residuals(test2))
summary(test2)
#Interestingly enough this model appears to handle the 2008 crisis well based on the residuals but suffers when dealing with the fallout from 9/11 which was a much smaller event.  I believe this is because the model is levered by that event. 
#Because of this I will also run an arima model with the same parameters for the data up to 08-2008 which is the first moment that a large dip in the stock market is noticed
newCarPurchases2<-newCarData[1:201,2]
census_newcar_TS2<-ts(newCarPurchases2,start=c(1992,1), frequency = 12)
test3<-arima(census_newcar_TS2, order = c(1,1,1), seasonal = list(order=c(1,0,0)))
tsdisplay(residuals(test3))
summary(test3)
#Forecasting of the data
plot(forecast.Arima(test1,h=12,level=c(50,99.5)))
plot(forecast.Arima(test2,h=12,level=c(50,99.5)))
plot(forecast.Arima(test3,h=12,level=c(50,99.5)))
#Looking at the plot for test3 (data before the crash) the low 99.5% likelihood estimate still estimates that total sales of cars is almost $700,000,000 higher than what actually happened.
forecast.Arima(test3,h=12,level=c(50,99.5))

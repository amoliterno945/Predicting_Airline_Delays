dataPath <- "C:/Users/Anthony/Desktop/Storage 2017July/desktop/U Chicago stuff/Time Series/Project/"
flight.delay.data <- read.csv(file=paste0(dataPath, "flight_data_2015.csv"), header = TRUE)

#Remove NAs
flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY != 0 & flight.delay.data$DEP_DELAY <= 90),]
#-----------------------------------------------------------------------------------------------------------------------------------
#Generate hourly data - average of delay time per hour for all hours of the day for the whole year
flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data$CRS_DEP_TIME)
#concatenate all datetime info
flight.delay.data$DELAY_DATE <- ISOdatetime(flight.delay.data$YEAR, flight.delay.data$MONTH, flight.delay.data$DAY_OF_MONTH, substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))
#Flights originating from ORD only
flights.ord.data <- flight.delay.data[which(flight.delay.data$ORIGIN == 'ORD'),] 
#New dataset of bucketed hourly delays
hourly.data.avg <- aggregate(flight.delay.data$DEP_DELAY, by=list(format(flight.delay.data$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.data.avg, 30)
colnames(hourly.data.avg) <- c("date", "delay")
hist(hourly.data.avg$delay, breaks = 200)
hourly.data.avg$date <- as.POSIXct(hourly.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")
#-----------------------------------------------------------------------------------------------------------------------------------
#Impute missing values
library(imputeTS)
library(xts)
library(lubridate)
#Create time series using xts package
df.xts <- xts(hourly.data.avg$delay, start = c(2015,1), order.by = hourly.data.avg$date)
#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
head(df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge
#Create data frame containing dates for all months starting from 2015-2016
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))
head(all.dates.df)
#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)
df.agg[which(df.agg$delay == 0),]$delay <- NA
df.agg$date.hour <- hour(df.agg$date) 
#Get data between 5 am and 10 pm
df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ] 
df.agg[which(is.na(df.agg$delay) == T),]$delay
plotNA.distribution(df.agg$delay)
statsNA(df.agg$delay)
plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))
hourly.data.avg <- na.interpolation(df.agg$delay, option = "spline")

#Re-establish datetimes for hourly data avg data (so we can line up with xreg weather data using a join function)
formatted_date<-data.frame("Date"=as.POSIXct(df.agg$date, origin="1970-01-01"))
hourly.data.avg<-cbind(formatted_date,hourly.data.avg)


#colnames(hourly.data.avg)[1]<-"Date" #USE ONLY IF YOU ARE NOT IMPUTING HOURLY VALUES
#Weather - https://www.ncdc.noaa.gov/cdo-web/datasets
weather<-read.csv(file=paste0(dataPath, "weather2015.csv"), header=TRUE)
formatted_date<-data.frame("Date"=as.POSIXct(weather[,1],format="%Y-%m-%d %H:%M:%OS"))
weather<-cbind(formatted_date+60*9,weather[,-1]) #rounding to nearest hour (all weather info is 9 minutes early)
library(plyr)
weather<-join(data.frame("Date"=hourly.data.avg$Date),weather,by="Date")
weather<-na.interpolation(weather[,3:ncol(weather)], option = "spline")

# Check for NAs:
# for (i in 1:ncol(weather)){
#   print(sum(is.na(weather[,i])))
#   }


#Data examination
library(tseries)
library(forecast)
dat<-ts(hourly.data.avg[,2],frequency=18)
plot(dat)
adf.test(dat) #stationary
kpss.test(dat) #nonstationary
##We note that the adf and kpss tests conflict.

#Examining visually for suitable MA and AR values:
acf(dat) #MA(25), seasonal MA(0)
pacf(dat) #AR(1), seasonal AR(1)

#Examining for improvement with 1 difference: it improves
dat_diff<-diff(dat, lag=1, differences=1) 
plot(dat_diff)
adf.test(dat_diff) #stationary
kpss.test(dat_diff) #stationary
##We note that the 1 difference improves the stationarity tests.

#Examining visually for suitable MA and AR values:
acf(dat_diff,main="ACF after 1 diff") #MA(2), seasonal MA(18)
pacf(dat_diff,main="PACF after 1 diff") #AR(1), seasonal AR(1)

#Testing different arima models.
# auto0<-auto.arima(dat) #(2,0,0) #AIC: 23777
# arima0<-Arima(dat,order = c(1,0,25)) #AIC:27418
# arima1<-Arima(dat,order = c(1,1,2),seasonal=list(order = c(1, 0, 1))) #AIC:23087




#XREG models
round(cor(cbind(hourly.data.avg[,2],weather)),2)
##We note that windspeed and precipitation are most significant predictors, however they are very low on an absolute basis
##The worst predictor is probably Hourly dry/wet bulbs, since they are least correlated to the target variable while highly correlated to other predictors

#Choosing optimal set of weather predictors using 3 different methods: regsubsets, step, and drop1:
library(leaps) 
subsets1<-regsubsets(x=weather,y=hourly.data.avg[,2]) #Regsubset: shows the optimal combination of predictors to include in order of least to most. This is useful for removing unnecessary variables in data while considering the correlations between variables (removing one impacts the rest's ability in explaining Y).
summary(subsets1)$which
##Regsubsets confirms our initial opinions' variables are valuable if we want a small model.

mdl<-lm(hourly.data.avg[,2]~weather[,1]+weather[,2]+weather[,3]+weather[,4]+weather[,5]+weather[,6]+weather[,7]+weather[,8]+weather[,9])
summary(step(mdl)) 
#Step function suggests most predictors are worth keeping.

#We now narrow doesn our selection of different models and check performance:
big_mdl<-lm(hourly.data.avg[,2]~weather[,1]+weather[,2]+weather[,3]+weather[,4]+weather[,5]+weather[,8]+weather[,9]) #all predictors suggested by step and drop1
summary(big_mdl) #R-squared:  0.05524
##As drop1 and step function suggested: the big model is optimal. A smaller model is more tempting to use, but since we have such large degrees of freedom due to high samples, we will use the bigger model.
##We note that R^2 is nevertheless very low.

#Using our optimal lm, we look to see for the optimal ARIMA parameters
plot(resid(big_mdl))
adf.test(resid(big_mdl)) #stationary
kpss.test(resid(big_mdl)) #nonstationary
acf(resid(big_mdl,main="ACF with NO diff")) #MA(26), seasonal MA(0) #GRAPH NOT ADJUSTED FOR 18 FREQ
pacf(resid(big_mdl,main="PACF with NO  diff")) #AR(3), seasonal AR(1) #GRAPH NOT ADJUSTED FOR 18 FREQ

#We do the same but take a first difference so that adf and kpss tests agree with each other
resid_diff<-diff(resid(big_mdl), lag=1, differences=1) 
adf.test(resid_diff) #stationary
kpss.test(resid_diff) #stationary
acf(resid_diff,main="ACF after 1 diff") #MA(2), seasonal MA(1)
pacf(resid_diff,main="PACF after 1 diff") #AR(1), seasonal AR(1)

# auto2<-auto.arima(dat,xreg=(weather[,c(1,2,3,4,5,6,7,8,9)])) #(3,1,2) #AIC:23742
xreg_arima<-Arima(dat,order = c(1,0,26),seasonal=list(order = c(1, 0, 0)),xreg=(weather[,c(1,2,3,4,5,6,7,8,9)])) #AIC:22857
#xreg_arima2<-Arima(dat,order = c(1,1,1),seasonal=list(order = c(2, 0, 2)),xreg=(weather[,c(1,2,3,4,5,6,7,8,9)])) #AIC:23035 #BEST PERFORMING



#We narrow down to our best models:


#Imput best model here:
arima_final<-xreg_arima



#We now examine the errors:

plot(dat[0:500],type="l")
lines(fitted(arima_final)[0:500],col="green")
##The plots show there seems to be issues matching extreme values. 

##We now examine the residuals further by visualizing...
par(mfrow=c(2,2))
hist(resid(arima_final),main="Frequency Distribution") #shows total frequency distribution
qqnorm(y=resid(arima_final),main="Quantiles Comparison: Normality") #shows comparison to normality
qqline(y=resid(arima_final))
plot(arima_final$residuals[order(fitted(arima_final))],main="Residuals vs. Ordered Fitted values: correlation") #Shows if errors are related to fitted value levels. Low fitted values correspond to underestimation, high fitted values correspond to overestimation
plot(resid(arima_final),type="b",main="Residuals: autocorrelation")  #Errors ordered chronologically, shows autocorrelation
par(mfrow=c(1,1))
##The distribution of errors show that they are leptokurtic (fat tails) and right skew. It also shows high fitted values tend to slightly overestimate compared to lower fitted values. There does not seem to be material heteroskedasticity or autocorrelation in errors after applying our model.

##We now continue to examine the residuals further by testing them quantitatively...
acf(resid(arima_final), main="ACF") 
pacf(resid(arima_final), main="PACF") 
adf.test(resid(arima_final))  #stationary
kpss.test(resid(arima_final)) #stationary
Box.test(resid(arima_final),lag=1,type="Ljung-Box") #residuals are independent


#MAE, MPE, MAPE, SMAPE
(CorrSq<-cor(arima_final$fitted,hourly.data.avg[,2])^2)
(MAE<-mean(abs(resid(arima_final))))
(MPE<-mean(resid(arima_final)/hourly.data.avg[,2]))
(MAPE<-mean(abs(resid(arima_final)/hourly.data.avg[,2])))
(SMAPE<-mean(abs(resid(arima_final))/((abs(hourly.data.avg[,2])+abs(fitted(arima_final)))*.5)))
##The model seems not too useful in predicting delays, as can be seen by the model correlation of .27 between fitted values and delays and low MPE. It  has difficulty predicting the extent of delays due to some tail samples exhibiting extremely large delays. MAE shows better performance because the large errors offset each other.

#Save results for later comparison:
train.scores<-data.frame(matrix(c(CorrSq,MAE,MPE,MAPE,SMAPE)),row.names=c("CorrSq","MAE","MPE","MAPE","SMAPE"))
colnames(train.scores)<-"Train"










#TEST ANALYSIS - test using 2016

#Prep 2016 flight data
flight.delay.data2 <- read.csv(file=paste0(dataPath, "flight_data_2016.csv"), header = TRUE)
flight.delay.data2 <- flight.delay.data2[which(is.na(flight.delay.data2$DEP_DELAY) == FALSE & flight.delay.data2$DEP_DELAY != 0 & flight.delay.data2$DEP_DELAY <= 90),]
flight.delay.data2$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data2$CRS_DEP_TIME)
flight.delay.data2$DELAY_DATE <- ISOdatetime(flight.delay.data2$YEAR, flight.delay.data2$MONTH, flight.delay.data2$DAY_OF_MONTH, substr(flight.delay.data2$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data2$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))
flights.ord.data2 <- flight.delay.data2[which(flight.delay.data2$ORIGIN == 'ORD'),] 
#Prep hourly
hourly.data.avg2 <- aggregate(flight.delay.data2$DEP_DELAY, by=list(format(flight.delay.data2$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
colnames(hourly.data.avg2) <- c("date", "delay")
hourly.data.avg2$date <- as.POSIXct(hourly.data.avg2$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")
df.xts2 <- xts(hourly.data.avg2$delay, start = c(2016,1), order.by = hourly.data.avg2$date)
df.xts2 <- data.frame(delay = df.xts2)
df.xts2$date <- as.POSIXct(rownames(df.xts2), origin="1970-01-01") #Add date column used in merge
all.dates.df2 <- data.frame(date = seq(as.POSIXct("2016-01-01 00:00"), to = as.POSIXct("2016-12-31 23:00"), by="hour"))
df.agg2 <- merge(all.dates.df2, df.xts2, by.x="date", by.y="date", all.x=T, all.y=T)
df.agg2[which(df.agg2$delay == 0),]$delay <- NA
df.agg2$date.hour <- hour(df.agg2$date) 
df.agg2 <- df.agg2[which(df.agg2$date.hour >= 5 & df.agg2$date.hour <= 22), ] 
hourly.data.avg2 <- na.interpolation(df.agg2$delay, option = "spline")
formatted_date2<-data.frame("Date"=as.POSIXct(df.agg2$date, origin="1970-01-01"))
hourly.data.avg2<-cbind(formatted_date2,hourly.data.avg2)
colnames(hourly.data.avg2)[1]<-"Date"

#Prep 2016 Weather data
weather2<-read.csv(file=paste0(dataPath, "weather2016.csv"), header=TRUE)
formatted_date2<-data.frame("Date"=as.POSIXct(weather2[,1],format="%Y-%m-%d %H:%M:%OS"))
weather2<-cbind(formatted_date2+60*9,weather2[,-1]) #rounding to nearest hour (all weather info is 9 minutes early)
weather2<-join(data.frame("Date"=hourly.data.avg2$Date),weather2,by="Date")
weather2<-na.interpolation(weather2[,3:ncol(weather2)], option = "spline")







#We examine the same steps with TEST data
dat2<-ts(hourly.data.avg2[,2],frequency=18)
pred<-matrix(Arima(dat2,model=arima_final,xreg=(weather2[,c(1,2,3,4,5,6,7,8,9)]))$fitted) #Uses our train model
res<-dat2-pred

plot(dat2,type="l")
lines(pred,col="blue")
##Still has issues with predicting extreme values

##We now examine the residuals further by visualizing...
par(mfrow=c(2,2))
hist(res,main="Frequency Distribution") 
qqnorm(y=res,main="Quantiles Comparison: Normality") 
qqline(y=res)
plot(res[order(pred)],main="Residuals vs. Ordered Fitted values: correlation") 
plot(res,type="b",main="Residuals: autocorrelation") 
par(mfrow=c(1,1))
##Same as with training: The distribution of errors show that they are leptokurtic (fat tails). It also shows that low fitted values tend to underestimate, and high fitted values tend to overestimate. There does not seem to be material heteroskedasticity or autocorrelation in errors.

##We now examine the residuals further by testing them quantitatively...
acf(res,main="ACF") 
pacf(res,main="PACF") 
adf.test(res)  #stationary
kpss.test(res) #stationary
Box.test(res,lag=1,type="Ljung-Box") #residuals are independent


#MAE, MPE, MAPE, SMAPE
(CorrSq=cor(pred,dat2)^2 )
(MAE<-mean(abs(res)))
(MPE<-mean(res/dat2))
(MAPE<-mean(abs(res/dat2)))
(SMAPE<-mean(abs(res)/((abs(dat2)+abs(pred))*.5)))
##The model seems useful in predicting directional movements of delays, as can be seen by the model correlation of .64 between fitted values and delays and low MAE, however it  has difficulty predicting the extent of delays due to some tail samples exhibiting extremely large delays. MAE shows better performance because the large errors offset each other.

test.scores<-data.frame(matrix(c(CorrSq,MAE,MPE,MAPE,SMAPE)),row.names=c("CorrSq","MAE","MPE","MAPE","SMAPE"))
colnames(test.scores)<-"Test"
scores<-cbind(train.scores,test.scores)
#The test scores were overall better than train. This is likely due to 2015 data having more spikes in sudden/extreme delays








#Final test: Does the model add value over using the last value?
dat0<-dat2[1:(length(dat2)-1)]
pred0<-dat2[2:length(dat2)]
res0<-dat0-pred0
plot(dat0[0:400],type="l")
lines(pred0[0:400],col="green")

par(mfrow=c(2,2))
hist(res0,main="Frequency Distribution") 
qqnorm(y=res0,main="Quantiles Comparison: Normality") 
qqline(y=res0)
plot(res0[order(pred0)],main="Residuals vs. Ordered Fitted values: correlation") 
plot(res0,type="b",main="Residuals: autocorrelation")  
par(mfrow=c(1,1))

acf(res0) 
pacf(res0) 
adf.test(res0)  #stationary
kpss.test(res0) #stationary
Box.test(res0,lag=1,type="Ljung-Box") #residuals are independent
#Does not lead to white-noise errors

(CorrSq<-cor(pred0,dat0)^2)
(MAE<-mean(abs(res0))) 
(MPE<-mean(res0/dat0)) 
(MAPE<-mean(abs(res0/dat0)))
(SMAPE<-mean(abs(res0)/((abs(dat0)+abs(pred0))*.5)))

#Compare all model results:
last.scores<-data.frame(matrix(c(CorrSq,MAE,MPE,MAPE,SMAPE)),row.names=c("CorrSq","MAE","MPE","MAPE","SMAPE"))
colnames(last.scores)<-"Last-Value"
(scores<-round(cbind(train.scores,test.scores,last.scores),4))

#Using the last value as the next prediction is shown to be better at predicting the extreme spikes in tails and overall led to a more balanced mean error. This makes sense since our arima model is smoother since it is a function of many past values. 
#However, the arima model was slightly better with mean percent error (MPE) and correlation, meaning that more accurate predictions in modest delays or delay directional movements more than compensated for the loss in extreme delay values.




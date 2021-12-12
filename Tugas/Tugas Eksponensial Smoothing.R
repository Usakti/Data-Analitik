#Calling Library Forecast & zoo
library(forecast)
library(zoo)

#Get Gold Data From R Library (Data Library)
data("gold")
gold

#Create Dataset
dataset = na.locf(gold, fromLast = TRUE)

#Create Timeseries Started from 1985 with Frequency 1 year or 365 days
dataset = ts(data = dataset, start = 1985, frequency = 365)

#Check Dataset
dataset

#Simple Exponential Smoothing
model = ses(dataset)

#Create Plot Model from Exponential Smoothing
plot(model)


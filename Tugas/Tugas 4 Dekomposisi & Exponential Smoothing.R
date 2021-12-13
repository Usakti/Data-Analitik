#4 Dekomposisi
data = USAccDeaths
class(USAccDeaths)
start(data); end(data); frequency(data)
plot(data, xlab="time", ylab="death", main="Quarterly Accidentially in US")

boxplot(data~cycle(data))

plot(decompose(data), xlab="Year", col="red")

data.dekom<-decompose(data, type="mult")
plot(data.dekom, col="blue")

trend<-data.dekom$trend
seasonal<-data.dekom$seasonal
ts.plot(trend, trend * seasonal, lty=1:2, xlab="Year", ylab="Deaths", main="Trend * Seasonal Plot")

fit = stl(data, s.window = "periodic")
fit

#Exponential Smoothing
data1 = UKDriverDeaths
data1
summary(data1)

plot(data1)

data1 = HoltWinters(data1, alpha = 0.2, beta = FALSE, gamma = FALSE)
data1

data1.pred = predict(data1, n.ahead = 10, prediction.interval=TRUE)
data1$fitted

data = UKDriverDeaths
data

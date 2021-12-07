library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost')
myQuery <- "select * from ricesupply;"
ricesupply <- dbGetQuery(con, myQuery)
View(ricesupply)

supply <- ts(ricesupply$Karawang, start = c(2011, 1), frequency = 12)
plot(supply)

library(forecast)
fit <- HoltWinters(supply)
accuracy(forecast(fit))
f1 <- forecast(fit,h=12)
print(f1)
forecast(fit,12)
plot(fit)

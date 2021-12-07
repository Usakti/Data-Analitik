library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost')
myQuery <- "select * from ricesupply;"
ricesupply <- dbGetQuery(con, myQuery)
View(ricesupply)

supply <- ts(ricesupply$Cianjur, start = c(2011, 1), frequency = 12)
plot(supply)

library(forecast)
fit <- HoltWinters(supply)
accuracy(forecast(fit))
f1 <- forecast(fit,h=12)
print(f1)
forecast(fit,12)
plot(fit)

ricets <- ts(ricesupply$Cianjur, frequency=12, start=c(2011,1))
ricets
plot.ts(ricets)

fit <- stl(ricets, s.window = "periodic")
plot(fit)

fit <- forecast(ricets)
accuracy(forecast(fit))
ricedec <- decompose(ricets)
ricedec$seasonal
plot(ricedec)

View(ricets)
library(xlsx)
write.xlsx(ricets, "D:/Cianjur-RiceSupply.xlsx")

library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost')
myQuery <- "select * from hargaberas;"
harga <- dbGetQuery(con, myQuery)
View(harga)

supply <- ts(harga$Setra, start = c(2011, 1), frequency = 12)
plot(supply)

library(forecast)
fit <- HoltWinters(supply)
accuracy(forecast(fit))
f1 <- forecast(fit,h=12)
print(f1)
forecast(fit,12)
plot(fit)

harga_beras <- ts(harga$Setra, frequency=12, start=c(2011,1))
harga_beras
plot.ts(harga_beras)

fit <- stl(harga_beras, s.window = "periodic")
plot(fit)

fit <- forecast(harga_beras)
accuracy(forecast(fit))
hargadec <- decompose(harga_beras)
hargadec $seasonal
plot(hargadec)

View(hargadec)
library(xlsx)
write.xlsx(ricets, "D:/Cianjur-HargaBeras.xlsx")















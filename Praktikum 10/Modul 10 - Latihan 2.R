ricets <- ts(ricesupply$Cirebon, frequency=12, start=c(2011,1))
ricets
plot.ts(ricets)

fit <- stl(ricets, s.window="periodic")
plot(fit)

accuracy(forecast(fit))
fit <- forecast(ricets)
accuracy(fit)

ricedec <- decompose(ricets)
ricedec$seasonal
plot(ricedec)

View(ricets)
library(xlsx)
write.xlsx(ricets, "D:/prak9azhar.xlsx")




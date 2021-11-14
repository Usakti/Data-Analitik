library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'directmarketing', host = 'localhost')
myQuery <- "select * from directmarketing;"
dataAzhar <- dbGetQuery(con, myQuery)

head(dataAzhar)

dataAzhar$Gender <- ifelse(dataAzhar$Gender == "Male", 0, 1)

head(dataAzhar)

tugas1_reg<-dataAzhar
head(tugas1_reg)

View(tugas1_reg)
model=lm(AmountSpent ~ Salary + Gender, data = tugas1_reg)
model

summary(model)

library(car)
library(olsrr)
library(lmtest)
ols_plot_resid_qq(model)

lmtest::bptest(model)

dwtest(model)

ols_vif_tol(model)

#tugas 2
dataAzhar$Y <- dataAzhar$AmountSpent
dataAzhar$X1 <- dataAzhar$Salary
dataAzhar$X2 <- ifelse(dataAzhar$Age == "Old", 1, 0)
dataAzhar$X3 <- ifelse(dataAzhar$Age == "Middle", 1, 0)

head(dataAzhar)

tugas2_reg<-dataAzhar
head(tugas2_reg)

View(tugas2_reg)

model=lm(Y ~ X1 + X2 + X3, data = tugas2_reg)
model

summary(model)

ols_plot_resid_qq(model)

lmtest::bptest(model)

dwtest(model)

ols_vif_tol(model)

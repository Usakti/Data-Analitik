library(olsrr)
library(car)
library(lmtest)
library(ggpubr)
library(RMySQL)

#Regresi Linear Sederhana

#Houseprices
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'houseprices', host = 'localhost')
myQuery <- "select * from houseprices;"
df <- dbGetQuery(con, myQuery)
View(df)
relasi = lm(df$Price ~ df$SqFt)
relasi

#Direct Marketing
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'directmarketing', host = 'localhost')
myQuery <- "select * from directmarketing;"
DirectMarketing <- dbGetQuery(con, myQuery)
View(DirectMarketing)
regresi = lm (DirectMarketing$AmountSpent ~ DirectMarketing$Salary)
regresi

summary(regresi)

#MTCars
data(mtcars) #memanggil data mtcars
View(mtcars) #melihat data mtcars
plot(mpg ~ wt, data=mtcars) #membuat plot regresi

model <- lm(mpg ~ wt, data=mtcars) #membuat model regresi
abline(model) #membuat garis pada plot regresi

summary(model) #melihat model regresi

predict(model, newdata = data.frame(wt=6))

ggscatter(mtcars, x = "mpg", y = "wt", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")

#Regresi Linear Berganda
data("mtcars")
model=lm(mpg ~ am + wt + hp, data = mtcars)
summary(model)

hist(mtcars$am)
plot(mpg~am,data=mtcars)

#Melihat korelasi antar variabel
dataku=mtcars[,c(1,4,6,9)]
cor(dataku,method = "pearson")

ols_correlations(model)

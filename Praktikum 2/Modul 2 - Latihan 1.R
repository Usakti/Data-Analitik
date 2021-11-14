data()
View(Titanic)
cross <- xtabs(Freq ~ Class+Sex, data=Titanic)
cross
prop.table(cross, 1)
chisq.test(cross)

head(iris)
cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)
library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point()

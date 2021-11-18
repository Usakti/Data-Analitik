#databaru = read.delim("clipboard")
databaru = read.csv("D:/titanic.csv")
databaru$Name <- NULL

str(databaru)

View(databaru)

sampel1 <- sample(1:nrow(databaru),0.75*nrow(databaru)) 
traininglogistik <- data.frame(databaru)[sampel1,] 
testinglogistik <- data.frame(databaru)[-sampel1,]
modellogistik = glm(Survived~.,data = traininglogistik, family = binomial) 
summary(modellogistik)

prediksilogistik=predict(modellogistik,testinglogistik) 
pred_logreg<-as.numeric(prediksilogistik>.5) 
tabel_logreg<-table(pred_logreg, testinglogistik$Survived) 
tabel_logreg

library(caret)

confusionMatrix(factor(pred_logreg), factor(testinglogistik$Survived), positive = "1")

# elkom 2
library(dplyr)
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))
# logistic regression
glm.out <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, data = iris.small, family = binomial) # family = binomial required for logistic regression
summary(glm.out)

exp(coef(glm.out))

glm.out <- glm(Species ~ Sepal.Width + Petal.Width + Petal.Length, data = iris.small, family = binomial)

exp(coef(glm.out))
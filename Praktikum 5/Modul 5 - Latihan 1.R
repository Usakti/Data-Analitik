#Uji Asumsi
library(olsrr)
library(ggpubr)
library(lmtest)
library(car)

model <- lm(mpg ~ wt, data = mtcars) 
ols_plot_resid_qq(model)

ols_test_normality(model)

par(mfrow=c(2,2))
model <- lm(mpg ~ wt, data=mtcars) 
plot(model)

lmtest::bptest(model)

dwtest(model)

ols_rsd_hist(model)
ols_plot_resid_hist(model)

model = lm(mpg~ am + wt + hp, data=mtcars)
ols_plot_resid_qq(model)

ols_norm_test(model)
ols_test_normality(model)

ols_plot_resid_hist(model)

lmtest::bptest(model)

dwtest(model)

ols_vif_tol(model)

require(FNN)

library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'Advertising', host = 'localhost')
myQuery <- "select * from advertising;"
Advertising <- dbGetQuery(con, myQuery)

head(Advertising)

trainx=Advertising[,-4] #X-matrix
ad.knn <- knn.reg(trainx, test = NULL, Advertising$Sales, k = 3)
plot(Advertising$Sales, ad.knn$pred, xlab="y", ylab=expression(hat(y)))

var(ad.knn$pred)/var(Advertising$Sales)

#This formula may not work for multiple regression or other models
y=Advertising$Sales
yhat=ad.knn$pred
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2);rsq

cor(yhat,y)^2 #approximate rsq very well. 

ad.lm <- lm(Sales~., data=Advertising)

yhat2=predict(ad.lm, Advertising)
plot(Advertising$Sales,yhat2,xlab="y",ylab=expression(hat(y)))

rsq2=1-sum((y-yhat2)^2)/sum((y-mean(y))^2);rsq2

cor(yhat2,y)^2 #approximate rsq very w

dim(Advertising)

train <- sample(1:dim(Advertising)[1],.7*dim(Advertising)[1])
test=-train
train.Ad <- Advertising[train,]
test.Ad <- Advertising[test,]
lm.tr <- lm(Sales ~., data=train.Ad)
summary(lm.tr)

ols_plot_resid_qq(model)

lmtest::bptest(model)

dwtest(model)

ols_vif_tol(model)

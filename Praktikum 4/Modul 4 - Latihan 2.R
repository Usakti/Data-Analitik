library(MASS)
library(ISLR)
library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'Advertising', host = 'localhost')
myQuery <- "select * from advertising;"
Advertising <- dbGetQuery(con, myQuery)
head(Advertising)

summary(Advertising)

pairs(Advertising, pch=".")

ad.lm <- lm(Sales~., data=Advertising)
summary(ad.lm)

rse=summary(ad.lm)$sigma
#RSE= 1.686 
mean(Advertising$Sales) 

rse/mean(Advertising$Sales)

rsq=summary(ad.lm)$r.sq
rsq #0.8972106

# rsq is calculated by the following formuala
yhat=ad.lm$fitted.values #predicted 
y=Advertising$Sales #observed 
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula

var(yhat)/var(y) #other formula

# rsq is calculated by the following formuala
yhat=ad.lm$fitted.values #predicted 
y=Advertising$Sales #observed 
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula

#Other way to get R2
var(yhat)/var(y) #other formula

1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula 
cor(yhat,y)^2 #alternate formula

Coef1=summary(ad.lm)$coefficients #Coefficient matrix
Coef1

lolim=Coef1[,1] - 1.96*Coef1[,2]
uplim=Coef1[,1] + 1.96*Coef1[,2]
cbind(lolim,uplim)

confint(ad.lm)

require(car)
vif(ad.lm)

#for the  average  response f(X)
predict(ad.lm, newdata=data.frame(TV=149,Radio=22,Newspaper=25), interval="confidence")

plot(ad.lm) #diagnostic plot

ad.lm2 <- lm(Sales~.^2, data=Advertising)
summary(ad.lm2)

summary(ad.lm2)$r.sq;summary(ad.lm)$r.sq 

ad.lm3 <- lm(Sales~.+I(TV^2), data=Advertising)
summary(ad.lm3)

par(mfrow=c(2,2))
plot(ad.lm3)

ad.lm4 <- lm(Sales~.+poly(TV,3), data=Advertising)
summary(ad.lm4)

anova(ad.lm,ad.lm4)

anova(ad.lm3,ad.lm4)

par(mfrow=c(2,2))
plot(ad.lm4)

ad.lm5 <- lm(Sales~.+poly(TV,3)+poly(Radio,3), data=Advertising)
plot(ad.lm5)

anova(ad.lm4,ad.lm5)

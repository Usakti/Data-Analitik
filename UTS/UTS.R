library(ggplot2)

data = read.csv("D:/insurance.csv")
View(data)

#1A
by(data$charges, data$sex, mean)
by(data$charges, data$smoker, mean)
by(data$charges, data$region, mean)

by(data$charges, data$sex, sd)
by(data$charges, data$smoker, sd)
by(data$charges, data$region, sd)

by(data$charges, data$sex, summary)
by(data$charges, data$smoker, summary)
by(data$charges, data$region, summary)

#1B
qplot(sex, charges, data=data, geom="boxplot", fill=sex)
qplot(smoker, charges, data=data, geom="boxplot", fill=smoker)
qplot(region, charges, data=data, geom="boxplot", fill=region)


#2A
cross <- xtabs(charges ~ sex+region, data=data)
cross
prop.table(cross, 1)
chisq.test(cross)

#2B
head(data)
cor(data$charges, data$bmi)
cor(data$charges, data$children)
cor(data$charges, data$age)

#2C
library(ggplot2)
ggplot(data, aes(x=bmi, y=charges, color=region)) + geom_point()
ggplot(data, aes(x=children, y=charges, color=region)) + geom_point()
ggplot(data, aes(x=age, y=charges, color=region)) + geom_point()


# 3 A
relasi = lm(data$charges ~ data$bmi)
relasi
summary(relasi)

# 3 B
model = lm(charges ~ bmi + children + age, data = data)
summary(model)

# 3 C
#Uji Asumsi
library(lmtest)
library(car)
library(olsrr)

# Uji Normalitas
ols_test_normality(model)
shapiro.test(model$residuals)

#Uji Heterokedasitas
par(mfrow=c(2,2))
plot(model)
bptest(model)

#Uji Multikolinearitas
vif(model)
ols_vif_tol(model)

#Uji Heterokedasitas
dwtest(model)

# 4 D
data = read.delim("clipboard")

# male = 1, female = 0
data$xSex <- ifelse(data$Sex == "male", 1, 0)
View(data)

# yes = 1, no = 0
data$xSmoker <- ifelse(data$Smoker == "yes", 1, 0)
View(data)

# northeast = 1 0 0, northwest = 0 1 0, southeast = 0 0 1, southwest = 0 0 0
data$x1Region <- ifelse(data$Region == "northeast", 1, 0)
data$x2Region <- ifelse(data$Region == "northwest", 1, 0)
data$x3Region <- ifelse(data$Region == "southeast", 1, 0)
View(data)


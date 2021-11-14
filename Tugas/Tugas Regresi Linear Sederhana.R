#Azhar Rizki Zulma
#065001900001

dataset = read.delim('clipboard')
View(dataset)

dataset = data.frame(dataset)
x = dataset$x
y = dataset$y
cor(x,y)
cor.test(x,y)

plot(x,y)
abline(lm(y~x))
lm(y~x)

c<-lm(y~x)
summary(c)

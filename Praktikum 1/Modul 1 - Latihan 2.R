library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password= '', dbname='data_analitik', host='localhost')
myQuery <- "SELECT * FROM train;"
dtAzhar <- dbGetQuery(con, myQuery)
View(dtAzhar)

by(dtAzhar$Fare, dtAzhar$Survived, mean)
by(dtAzhar$Fare, dtAzhar$Survived, sd)
by(dtAzhar$Fare, dtAzhar$Survived, summary)

library(ggplot2)
qplot(Sex,Survived, data=dtAzhar, geom="boxplot", fill=Sex)
qplot(Sex,Pclass, data=dtAzhar, geom="boxplot", fill=Sex)
qplot(Sex,Fare, data=dtAzhar, geom="boxplot", fill=Sex)
hist(dtAzhar$Fare[dtAzhar$Survived=='1'])
hist(dtAzhar$Fare[dtAzhar$Survived=='0'])


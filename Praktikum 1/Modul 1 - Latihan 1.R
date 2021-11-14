install.packages('RMySQL')
library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password= '', dbname='data_analitik', host='localhost')
myQuery <- "SELECT * FROM iris;"
dtAzhar <- dbGetQuery(con, myQuery)
View(dtAzhar)

by(dtAzhar$petalwidth, dtAzhar$iris, mean)
by(dtAzhar$petalwidth, dtAzhar$iris, sd)
by(dtAzhar$petalwidth, dtAzhar$iris, summary)

library(ggplot2)
qplot(iris, petallength, data=dtAzhar, geom="boxplot", fill=iris)

library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'houseprices', host = 'localhost')
myQuery <- "select * from houseprices;"
df_azhar <- dbGetQuery(con, myQuery)
View(df_azhar)

cross <- xtabs(Price ~ Brick+Neighborhood, data=df_azhar)
cross
prop.table(cross, 1)
chisq.test(cross)

head(df_azhar)
cor(df_azhar$Price, df_azhar$SqFt)
cor(df_azhar$Price, df_azhar$Bedrooms)
cor(df_azhar$Price, df_azhar$Bathrooms)


library(ggplot2)
ggplot(df_azhar, aes(x=SqFt, y=Price, color=Neighborhood)) + geom_point()

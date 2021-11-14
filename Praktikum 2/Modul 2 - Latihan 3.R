library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'directmarketing', host = 'localhost')
myQuery <- "select * from directmarketing;"
df_azhar <- dbGetQuery(con, myQuery)
View(df_azhar)

cross <- xtabs(Salary ~ OwnHome+Married, data=df_azhar)
cross
prop.table(cross, 1)
chisq.test(cross)

head(df_azhar)
cor(df_azhar$Salary, df_azhar$AmountSpent)
cor(df_azhar$Salary, df_azhar$Catalogs)
cor(df_azhar$Salary, df_azhar$Children)
library(ggplot2)
ggplot(df_azhar, aes(x=AmountSpent, y=Salary, color=Age)) + geom_point()

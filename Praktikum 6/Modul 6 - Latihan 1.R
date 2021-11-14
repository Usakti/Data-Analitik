#Regresi dengan satu var independent bersifat kualitatif (2 kelas) 1 var dummy
library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_reg', host = 'localhost')
myQuery <- "select * from reg;"
reg <- dbGetQuery(con, myQuery)
  
View(reg)
head(reg)

model=lm(Y ~ X1 + X2, data = reg)
model

summary(model)

#
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_reg', host = 'localhost')
myQuery <- "select * from reg2;"
reg <- dbGetQuery(con, myQuery)
View(reg2)
head(reg2)

model=lm(Y ~ X1 + X2 + X3, data = reg2)
model

summary(model)

reg3=read.delim("clipboard")
head(reg3)

model=lm(Y ~ X + X2, data = reg3)
model

summary(model)


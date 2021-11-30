library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost')
myQuery <- "select * from komoditas;"
azhar <- dbGetQuery(con, myQuery)
View(azhar)

install.packages("factoextra")
install.packages("NbClust")

library(factoextra)
library(NbClust)
str(azhar)

azhar.pca=dataku[,2:6]
View(azhar.pca)
library(NbClust)
nb <- NbClust(azhar.pca, distance = "euclidean", min.nc =2, max.nc = 10, method = "complete", index ="all")

library(ggpubr)
km.res=kmeans(azhar.pca,3,nstart = 25)
fviz_cluster(km.res, data = azhar.pca, geom = "point", stand = FALSE, ellipse.type = "norm")
fviz_cluster(km.res, data = azhar.pca)



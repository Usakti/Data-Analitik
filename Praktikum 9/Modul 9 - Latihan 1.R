library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost')
myQuery <- "select * from komoditas;"
dataku <- dbGetQuery(con, myQuery)
View(dataku)

str(dataku)

dataku.pca=dataku[,2:6]
komoditas.pca <- prcomp(dataku.pca, center = TRUE, scale. = TRUE) 
print(komoditas.pca)
plot(komoditas.pca, type = "l")


summary(komoditas.pca)
predict(komoditas.pca,  newdata=tail(dataku))
hasil=predict(komoditas.pca, newdata=tail(dataku))
View(hasil)

komoditas.kota <- dataku[, 1]
install.packages('devtools')
library(devtools)
install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot)
g <- ggbiplot(komoditas.pca,ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

g <- ggbiplot(komoditas.pca, obs.scale = 1, var.scale = 1, groups= komoditas.kota, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

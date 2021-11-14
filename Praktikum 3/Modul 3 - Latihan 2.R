library(caret)
library(dplyr)
library(ggmap)

data_azhar=read.csv("D:/Azhar/Data Analitik/Praktikum 3/HargaBerasProvinsi.csv")
df <- data_azhar %>% group_by(Provinsi) %>% summarize(median_price = median(Harga), transactions = n(), latitude = mean(Latitude), longitude = mean(Longitude))

ggplot() + geom_point(data = df, mapping = aes(x = longitude, y = latitude, col = median_price, size = transactions)) + scale_color_distiller(palette = "YlOrRd", direction = 1)

ggplot(data = df, mapping = aes(x = longitude, y = latitude)) + geom_point(aes(col = median_price, size = transactions)) + geom_text(aes(label = Provinsi), size = 2, nudge_y = 0.01) + scale_color_distiller(palette = "YlOrRd", direction = 1)

height <- max(data_azhar$Latitude) - min(data_azhar$Latitude)
width <- max(data_azhar$Longitude) - min(data_azhar$Longitude)
sac_borders <- c(bottom  = min(data_azhar$Latitude) - 0.1 * height, top = max(data_azhar$Latitude)  + 0.1 * height, left = min(data_azhar$Longitude) - 0.1 * width, right   = max(data_azhar$Longitude) + 0.1 * width)

map <- get_stamenmap(sac_borders, zoom = 5, maptype = "toner-lite")
ggmap(map)

ggmap(map) + geom_point(data = df, mapping = aes(x = longitude, y = latitude, col = median_price, size = transactions)) + scale_color_distiller(palette = "YlOrRd", direction = 1)

qmplot(x = longitude, y = latitude, data = df, maptype = "watercolor", geom = "point", color = median_price, size = transactions) + scale_color_gradient(low = "blue", high = "red")



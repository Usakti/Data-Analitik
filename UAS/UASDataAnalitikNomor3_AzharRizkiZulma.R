# No 3 A Mengambil Data Sebanyak 120 baris + NIM Terbelakang = 121 baris dengan Label
data_azhar <- read.delim("clipboard")

# Menampilkan datanya
View(data_azhar)

# No 3 B Menampilkan Tipe Data dari setiap Variabel
str(data_azhar)

# Merubah Tipe Data Variabel Jumlah.penumpang menjadi variabel numeric
data_azhar$Jumlah.penumpang = as.numeric(data_azhar$Jumlah.penumpang)

# Menghitung rerata variabel Jumlah.penumpang
mean(data_azhar$Jumlah.penumpang, na.rm = "TRUE")

# Mengecek Nilai NA/NaN/Nilai Kosong dan Menggantinya dengan Mean
data_azhar$Jumlah.penumpang = ifelse(is.na(data_azhar$Jumlah.penumpang), ave(data_azhar$Jumlah.penumpang, FUN = function(x) mean(x, na.rm = 'TRUE')), data_azhar$Jumlah.penumpang)

# Menampilkan data dengan nilai NA yang sudah tergantikan dengan rerata
View(data_azhar)

# No 3 C merubah data menjadi data timeseries
Jumlah.penumpang <- ts(data_azhar$Jumlah.penumpang, start = c(2007, 1), frequency = 12)

# Membuat plot data aktual
plot(Jumlah.penumpang)

# Import library forecast
library(forecast)

# No 3 D Stl dan Plotnya
fitStl <- stl(Jumlah.penumpang, s.window = "periodic")
plot(fitStl)

# Decompose additive
fitDecA <- decompose(Jumlah.penumpang, type="additive")
plot(fitDecA)

# Decompose Multiplicative
fitDecM <- decompose(Jumlah.penumpang, type="multiplicative")
plot(fitDecM)

# No 3 E melakukan peramalan HoltWinter dengan metode additive
fitAdditive <- HoltWinters(Jumlah.penumpang, seasonal = "additive")
plot(fitAdditive)
fitAdditive

# No 3 F melakukan peramalan HoltWinter dengan metode multiplicative
fitMultiplicative <- HoltWinters(Jumlah.penumpang, seasonal = "multiplicative")
plot(fitMultiplicative)
fitMultiplicative

# No 3 G Mengukur akurasi dan kesalahan metode HoltWonters additive
accuracy(forecast(fitAdditive))

# No 3 H Mengukur akurasi dan kesalahan metode HoltWonters multiplicative
accuracy(forecast(fitMultiplicative))

# No 3 I Seasonal Plot
seasonPlot <- ggseasonplot(Jumlah.penumpang)
plot(seasonPlot)

# Prediksi Additive
prediksiA <- forecast(fitAdditive, h=5)
print(prediksiA)

# Prediksi Multiplicative
prediksiM <- forecast(fitMultiplicative, h=5)
print(prediksiM)

# No 3 J Multiple Plot
par(mfrow = c(2, 2))
plot(Jumlah.penumpang) # Plot Data Aktual
plot(fitMultiplicative) # Plot Time Series
plot(prediksiM) # Plot Prediksi


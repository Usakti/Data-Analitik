#Membuat Dataframe serta mengimport file titanic ke R
dfTitanic = read.csv(choose.files())

#Mengecek Data Teratas
head(dfTitanic)

#Melakukan Pengecekan data, apakah terdapat data yang null/na
any(is.na(dfTitanic))

#Melakukan instalasi library Amelia
install.packages("Amelia")

#Memanggil library Amelia
library(Amelia)

#Melakukan pemetaan serta menampilkan visualisasi data yang tidak lengkap/na dengan nama grafiknya adalah Titanic Data dan dikategorisasikan berdasarkan warna merah dan putih dengan keterangan/legend tidak diaktifkan
missmap(dfTitanic, main="Titanic Data", col = c("red", "white"), legend = FALSE)

#memastikan berapa banyak data yang belum lengkap/na pada kolom age dengan fungsi summary
summary(dfTitanic)

#Mengisi data nilai age yang NA/tidak lengkap dengan rata-rata age/mean
dfTitanic$Age = ifelse(is.na(dfTitanic$Age), mean(dfTitanic$Age, na.rm = TRUE), dfTitanic$Age)

#Melakukan pengecekan ulang apakah masih terdapat nilai NA atau tidak pada data Titanic
summary(dfTitanic)
any(is.na(dfTitanic))
missmap(dfTitanic, main="Titanic Data", col = c("red", "white"), legend = FALSE)

#Melihat Struktur dan Tipe Data pada Dataframe Titanic
str(dfTitanic)

#memanggil library dplyr
library(dplyr)

#Menyeleksi Atribut/Kolom serta menghilangkan atribut yang tidak digunakan yaitu PassengerId, Ticket, Cabin dan juga Name
dfTitanic = select(dfTitanic, -PassengerId, -Ticket, -Cabin, -Name)

#Mengecek datanya apakah data yang sudah dihilangkan sudah berhasil atau belum dengan memanggil 3 kolom teratas
head(dfTitanic, 3)

#Mengubah jenis kolom/atribut menjadi jenis faktor
dfTitanic$Survived = factor(dfTitanic$Survived)
dfTitanic$Pclass = factor(dfTitanic$Pclass)
dfTitanic$SibSp = factor(dfTitanic$SibSp)
dfTitanic$Parch = factor(dfTitanic$Parch)

#Memanggil package caTools
library(caTools)

#Membagi 2 data yaitu training dan testing
set.seed(101)

#Mendeklarasikan variabel split dari hasil mengiris data sample berdasarkan kolom survive dengan rasio split 0.7
split = sample.split(dfTitanic$Survived, SplitRatio = 0.7)

#membuat data Train berdasarkan subset dari data Titanic yang bernilai split True
finalTrain = subset(dfTitanic, split == TRUE)

#membuat data Test berdasarkan subset dari data Titanic yang bernilai split False
finalTest = subset(dfTitanic, split == FALSE)

#Membuat model regresi logistik dengan fungsi glm, dengan formula dari variabel terikat yaitu survived dengan semua variabel dan juga menggunakan semua atribut, lalu dengan family binomial dengan link yang logit dengan data yang digunakan ialah data Train
finalLogModel = glm(formula = Survived ~ ., family = binomial, data = finalTrain)

#Melihat Summary Model Data finalLogModel
summary(finalLogModel)

#Mengevaluasi Model dengan memprediksi model dengan data baru yaitu data tes dengan tipe response
fitProb = predict(finalLogModel, newdata = finalTest, type = 'response')

#Membuat Result dari nilai probabilitas yang diubah menjadi dua kategori menjadi 0 dan 1
results = ifelse(fitProb > 0.5, 1, 0)

#Menampilkan Result dengan fungsi View
View(results)

#Mendeklarasikan Akurasi data berdasarkan nilai yang tidak sama dengan kolom survived pada data test
akurasi = mean(results != finalTest$Survived)

#Menampilkan hasil akurasi data yang sebelumnya telah dideklarasikan
print(paste('Akurasi', 1-akurasi))

#Confusion Matriks berdasarkan nilai probabilitas lebih besar dari 0.5
table(finalTest$Survived, fitProb > 0.5)

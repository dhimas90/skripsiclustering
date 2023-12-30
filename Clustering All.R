library(readr)
library(dplyr)
library(DT)

#input data
kedatangan_data<- read.csv("data sebaran.csv", header = T)
datatable(kedatangan_data, caption = "Data Kedatangan Penduduk ke Wilayah Jakarta")

str(kedatangan_data)
#normalisasi data
library(factoextra)

data.kmeans<-kedatangan_data
#standarized <- scale(kedatangan_data[,2:7])
#standarized1 <- kedatangan_data[,2:7] 
#library(gapminder)
#kedatangan_data %>%
  #mutate(bulan = as.numeric(Bulan))
datatable(standarized, caption = "Data yang Sudah Distandarisasi")


#jumlah_klaster <- c(1:7)
#within_ss <- c()

#for (i in jumlah_klaster) {
 # within_ss <- c(within_ss, kmeans(x = kedatangan_data, centers = i, nstart = 25)$tot.withinss)  
#}

#pencarian k 
library(factoextra)
fviz_nbclust(kedatangan_data, kmeans, method = "wss")
#Elbow Mendapatkan nilai k = 3
fviz_nbclust(kedatangan_data, kmeans, method = "silhouette")
#shilouette mendapatkan nilai k = 2
#fviz_nbclust(kedatangan_data, kmeans, method = "gap_stat")
#gap_statistics mendapatkan nikai k=1

set.seed(123)
kmeans_clustering <- kmeans(x = kedatangan_data, centers = 3, nstart = 25)
kmeans_clustering

# return nilai centroid
#kedatangan_data %>%
 # mutate(klaster = kmeans_clustering$cluster) %>%
  #group_by(klaster) %>%
  #summarise(bulan = mean(bulan)
  #summarise(Mean_bulan = mean(bulan),Mean_kota_kabupaten = mean(kota_kabupaten), Mean_kecamatan = mean(kecamatan), Mean_kelurahan=mean(kelurahan), Mean_jenis_kelamin = mean(jenis_kelamin),Mean_jumlah = mean(jumlah))

#Mean_kota_kabupaten = mean(kota_kabupaten), Mean_kecamatan = mean(kecamatan), 

#kedatangan_data %>%
#mutate(klaster = kmeans_clustering$cluster) %>%
#select(Label, klaster) %>%
#arrange(klaster)

visualisasi <- data.frame(kedatangan_data, kmeans_clustering$cluster)
visualisasi
datatable(visualisasi)
fviz_cluster(kmeans_clustering, data = kedatangan_data)


write.csv(visualisasi,"skripsi/skripsi all record finish.csv")

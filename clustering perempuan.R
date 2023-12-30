library(readr)
library(dplyr)
library(DT)

#input data
kedatangan_data <- read.csv("datadhimas2.csv", header = T)
datatable(kedatangan_data, caption = "Data Kedatangan Penduduk ke Wilayah Jakarta")

#normalisasi data
data.plot <- kedatangan_data[,2:6]
data_standarized <- round(scale(kedatangan_data[,2:6]),5)
datatable(data_standarized, caption = "Data yang Sudah Distandarisasi")


jumlah_klaster <- c(1:7)
within_ss <- c()

for (i in jumlah_klaster) {
  within_ss <- c(within_ss, kmeans(x = data_standarized, centers = i, nstart = 25)$tot.withinss)  
}

#pencarian k 
library(factoextra)
fviz_nbclust(data_standarized, kmeans, method = "wss")
#Elbow Mendapatkan nilai k = 3
  fviz_nbclust(data_standarized, kmeans, method = "silhouette")
#shilouette mendapatkan nilai k = 2
fviz_nbclust(data_standarized, kmeans, method = "gap_stat")
#gap_statistics mendapatkan nikai k=1

set.seed(123)
kmeans_clustering <- kmeans(x = data.plot, centers = 3, nstart = 25)
kmeans_clustering

# return nilai centroid
kedatangan_data %>%
  mutate(klaster = kmeans_clustering$cluster) %>%
  group_by(klaster) %>%
  summarise(Mean_Januari = mean(Januari), Mean_Februari = mean(Februari), Mean_Maret = mean(Maret), Mean_April=mean(April), Mean_Mei = mean(Mei))


#kedatangan_data %>%
#mutate(klaster = kmeans_clustering$cluster) %>%
#select(Label, klaster) %>%
#arrange(klaster)

visualisasi <- data.frame(kedatangan_data, kmeans_clustering$cluster)
visualisasi
datatable(visualisasi)
fviz_cluster(kmeans_clustering, data = data.plot)


write.csv(visualisasi,"skripsi/Clustering kedatangan Perempuan Result.csv")

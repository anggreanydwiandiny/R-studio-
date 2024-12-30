# Load necessary libraries
library(tidyverse)
library(cluster)

# Load dataset
file_path <- "D:/dataset&pdf/221141-anggreany-kmeans/tugas5/Housing.csv"
housing_data <- read.csv(file_path)

# Inspeksi awal data
summary(housing_data)
str(housing_data)

# Memilih kolom numerik untuk clustering
numerical_data <- housing_data %>%
  select(where(is.numeric))  # Memilih hanya kolom numerik

# Memeriksa missing values
sum(is.na(numerical_data))

# Mengatasi missing values (opsional, jika ada)
numerical_data <- na.omit(numerical_data)

# Standarisasi data
scaled_data <- scale(numerical_data)

# Menentukan jumlah cluster optimal menggunakan metode Elbow
set.seed(123)  # Untuk reproduktifitas
wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
})

# Visualisasi metode Elbow
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")

# Menentukan jumlah cluster (misalnya k = 3)
k <- 3
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)

# Menambahkan hasil cluster ke data asli
housing_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualisasi hasil clustering (contoh menggunakan PCA)
library(ggplot2)
pca_data <- prcomp(scaled_data)$x[, 1:2]  # Mengambil dua komponen utama
pca_df <- as.data.frame(pca_data)
pca_df$Cluster <- housing_data$Cluster

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Clustering Results", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Save hasil clustering
output_path <- "D:/dataset&pdf/221141-anggreany-kmeans/tugas5/Housing_with_Clusters.csv"
write.csv(housing_data, output_path, row.names = FALSE)

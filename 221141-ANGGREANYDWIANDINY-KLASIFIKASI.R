# Install library C50 jika belum terinstal
if (!require(C50)) install.packages("C50")

# Load library
library(C50)

# Load dataset
dataset <- read.csv("real_estate_dataset.csv")

# Periksa struktur dataset
str(dataset)

# Tambahkan kolom kategori harga
# Mengelompokkan harga menjadi "Low", "Medium", "High" berdasarkan kuantil
dataset$Price_Category <- cut(dataset$Price, 
                              breaks = quantile(dataset$Price, probs = c(0, 0.33, 0.66, 1)), 
                              labels = c("Low", "Medium", "High"), 
                              include.lowest = TRUE)

# Pastikan kolom target adalah faktor
dataset$Price_Category <- as.factor(dataset$Price_Category)

# Hapus kolom yang tidak relevan untuk model (seperti ID)
dataset <- subset(dataset, select = -c(ID, Price))

# Bagi data menjadi data latih (80%) dan data uji (20%)
set.seed(123) # Untuk reproduktifitas
train_index <- sample(1:nrow(dataset), 0.8 * nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]


# Bangun model C4.5
model <- C5.0(Price_Category ~ ., data = train_data)

# Lihat ringkasan model
summary(model)

# Prediksi pada data uji
predictions <- predict(model, test_data)

# Evaluasi model
confusion_matrix <- table(test_data$Price_Category, predictions)
print(confusion_matrix)

# Hitung akurasi
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

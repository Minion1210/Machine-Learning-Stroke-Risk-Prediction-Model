# 載入所需的R套件
library(class)
library(caret)
library(ggplot2)
library(pROC)
library(randomForest)
library(e1071)
install.packages("data.table")

library("data.table")
install.packages("caret")
library(caret)

dataset<-data.table(fread("C:\\Users\\user\\Desktop\\高醫醫資\\碩一下\\大數據分析\\python\\healthcare-dataset-stroke-data-2.csv"))

# 資料預處理函數
preprocessing <- function() {
  # 這裡是示例，你需要根據實際情況編寫資料預處理步驟
  dataset$target <- dataset[, 11]
  trainIndex <- createDataPartition(dataset$target, p = 0.8, list = FALSE)
  x_train <- dataset[trainIndex,1:10 ]
  x_test <- dataset[-trainIndex,1:10 ]
  y_train <- dataset$target[trainIndex]
  y_test <- dataset$target[-trainIndex]
  return(list(dataset = dataset, x_train = x_train, x_test = x_test, y_train = y_train, y_test = y_test))
}


# 呼叫資料預處理函數
library(caret)

data_list <- preprocessing()
dataset <- data_list$dataset
x_train <- data.table(scale(data_list$x_train))
x_test <- data.table(scale(data_list$x_test))
y_train <- data_list$y_train
y_test <- data_list$y_test
y_train <- as.numeric(y_train == 'n')
y_test <- as.numeric(y_test == 'n')

# 訓練KNN模型
install.packages("class")
library(class)

clf <- knn(train = x_train, test = x_test, cl = y_train, k = 3)

# 儲存訓練好的模型到檔案
save(clf, file = "CVA_KNN_model.RData")
print("模型已儲存到 CVA_KNN_model.RData")

# 計算訓練和測試準確度
train_pred <- knn(train = x_train, test = x_train, cl = y_train, k = 3)
test_pred <- knn(train = x_train, test = x_test, cl = y_train, k = 3)
cat("Accuracy of training:", mean(train_pred == y_train), "\n")
cat("Accuracy of testing:", mean(test_pred == y_test), "\n")
cat("Classification Report:\n")
library(caret)



library(caret)
# Convert test_pred and y_test to factors
test_pred <- factor(test_pred)
y_test <- factor(y_test)

# Ensure both factors have the same levels
levels(test_pred) <- levels(y_test)

print(confusionMatrix(test_pred, y_test))

# 繪製混淆矩陣
cm <- confusionMatrix(test_pred, y_test)$table

ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  # 添加文本标签
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()

# 計算AUC
install.packages("pROC")
library(pROC)
x_predict <- as.numeric(test_pred )
y_test_num <- as.numeric(y_test == 'n')

# Plot the ROC curve

roc_curve <- roc(y_test, x_predict)
plot(roc_curve)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
title(main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
cat("AUC of testing:", auc(roc_curve), "\n")


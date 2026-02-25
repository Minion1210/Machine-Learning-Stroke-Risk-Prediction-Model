# Install packages
install.packages(c("e1071", "readxl", "dplyr", "data.table", "broom", "pROC", "ROCR", "caret"))

# Load libraries
library(e1071)
library(readxl)
library(dplyr)
library(data.table)
library(broom)
library(pROC)
library(ROCR)
library(class)
library(caret)
library(ggplot2)
library(randomForest)

# Re-install data.table and caret
install.packages(c("data.table", "caret"))

# Load caret and data.table again
library(caret)
library(data.table)

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
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

library(e1071)

svm_model <- svm(x = x_train, y = y_train, kernel = "radial", cost = 1,probability=T)

svm_pred <- predict(svm_model, x_test,probability = T)

cat("Accuracy of testing (SVM):", mean(svm_pred == y_test), "\n")
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(svm_pred, y_test))

library(caret)

cm_svm <- confusionMatrix(svm_pred, y_test)$table
01library(ggplot2)

ggplot(data = as.data.frame(cm_svm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  # 添加文本标签
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()

library(pROC)
library(ROCR)
test_pred <- predict(svm_model, x_test,  probability = T)
probabilities <- attr(test_pred, "probabilities")
x_predict <- as.numeric(probabilities[,1])
true_labels <- ifelse(y_test == "y", 1, 0)
true_labels<-as.factor(true_labels)
svm_roc_curve <- roc(true_labels, x_predict)
cat("AUC of testing (SVM):", auc(svm_roc_curve), "\n")
#ROC畫圖
plot(svm_roc_curve , main = "ROC Curve", col = "blue")

#10折交叉
install.packages("caret")
library(caret)
#10-fold cross validation
train_control <- trainControl(method = "cv",number = 5) # k = 10
# specify the model 
# 支持向量機
svm_cv_model <- train(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status, 
                   data = dataset, 
                   method = "svmRadial", 
                   trControl = train_control, 
                   na.action = na.pass)
#最後得到的accuracy最好的拿來跟accuracy_dtree做比較，看看有無overfitting
svm_cv_model
# 計算訓練和測試準確度
train_pred <- predict(svm_model, x_train)
test_pred <- predict(svm_model, x_test)
cat("Accuracy of training:", mean(train_pred == y_train), "\n")
cat("Accuracy of testing:", mean(test_pred == y_test), "\n")
svm_roc_curve


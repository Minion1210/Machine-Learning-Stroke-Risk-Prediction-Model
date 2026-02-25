# 載入所需的R套件
library(randomForest)
library(caret)
library(pROC)
library(e1071)
library(ggplot2)
library(class)
install.packages("data.table")
library("data.table")
install.packages("caret")
library(caret)
install.packages("data.table")
library("data.table")
library(randomForest)

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
library("data.table")
library(caret)
# 呼叫資料預處理函數
data_list <- preprocessing()
dataset <- data_list$dataset
x_train <- data.table(scale(data_list$x_train))
x_test <- data.table(scale(data_list$x_test))
y_train <- data_list$y_train
y_test <- data_list$y_test
#將y轉換成factor
y_train <- as.factor(y_train )
y_test <- as.factor(y_test )





# 訓練隨機森林模型
library(randomForest)
clf <- randomForest(x = x_train, y = y_train, ntree = 500)
save(clf, file = "CVA_RandomForest_model.RData")
print("模型已儲存到 CVA_RandomForest_model.RData")
dev.new()
plot(clf)

#特徵重要度
importance(clf)

# 計算訓練和測試準確度
train_pred <- predict(clf, x_train)
test_pred <- predict(clf, x_test)
cat("Accuracy of training:", mean(train_pred == y_train), "\n")
cat("Accuracy of testing:", mean(test_pred == y_test), "\n")
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(test_pred, y_test))

# 繪製訓練集混淆矩陣
cm <- confusionMatrix(test_pred, y_test)$table

ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()


# 繪製測試集混淆矩陣
cm <- confusionMatrix(test_pred, y_test)$table
ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()

library(pROC)
# 計算AUC
test_pred <- predict(clf, x_test,  type = "prob")
x_predict <- as.numeric(test_pred[,1])
y_test_num <- as.numeric(y_test == 'n')
roc_curve <- roc(y_test_num, x_predict)
cat("AUC of testing:", auc(roc_curve), "\n")


# Plot the ROC curve
roc_curve <- roc(y_test_num, x_predict)
plot(roc_curve)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
title(main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
cat("AUC of testing:", auc(roc_curve), "\n")

install.packages("InformationValue")
library(InformationValue)
#random forest找最佳切點
optCutOff_rf <- optimalCutoff(y_test_num, x_predict)[1]
print(optCutOff_rf)
rf_pred_opt<-ifelse(x_predict>=optCutOff_rf,1,0)


#看預測及實際的差異in test_set
confus_matrix_rf<-table(real=y_test_num,predicted=test_pred )
print(confus_matrix_rf)
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(factor(test_pred),factor(y_test_num)))
confus_matrix_rf_opt<-table(real=y_test_num,predicted=rf_pred_opt)
print(confus_matrix_rf_opt)
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(factor(rf_pred_opt),factor(y_test_num)))

#搭配plot看
dev.new()
plot(clf)





#10折交叉
install.packages("caret")
library(caret)
#10-fold cross validation
train_control <- trainControl(method = "cv",number = 5) # k = 10
# specify the model 
my.dtree_k10.model <- train(stroke~ gender + age + hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, data = dataset, method = 'rpart',na.action = na.pass, trControl = train_control)
#和accuracy_dtree比較一下, 看看有無overfitting，CP欄位代表樹的成本複雜度參數，主要還是看accuracy來決定
my.dtree_k10.model   #最後得到的accuracy最好的拿來跟accuracy_dtree做比較，看看有無overfitting
      



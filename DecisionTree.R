# 載入所需的R套件
library(class)
library(caret)
library(ggplot2)
library(pROC)
library(randomForest)
library(e1071)
install.packages("data.table")
install.packages("caret")
library(caret)
library("data.table")
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
library("data.table")

data_list <- preprocessing()
dataset <- data_list$dataset
x_train <- data.table(scale(data_list$x_train))
x_test <- data.table(scale(data_list$x_test))
y_train <- data_list$y_train
y_test <- data_list$y_test
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)


library(rpart)
library(rpart.plot)
# 訓練決策樹模型
tree_model <- rpart(y_train~  gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, data=x_train, method = "class")
summary(tree_model)
##利用logistic regression跑回歸
my.logit <- glm(y_train~  gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, data=x_train, family = binomial(link="logit"))
summary(my.logit)
#linear regression
lr.model <-lm((as.numeric(y_train))~  gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, data=x_train)
summary(lr.model)



# 儲存訓練好的模型到檔案
save(tree_model, file = "CVA_Decision_Tree_model.RData")
print("模型已儲存到 CVA_Decision_Tree_model.RData")

# 預測
tree_pred_train <- predict(tree_model, x_train, type = "class")
tree_pred_test <- predict(tree_model, x_test, type = "class")

lr_test <- predict(lr.model,x_test)
lr_train <- predict(lr.model,x_train)

my.logit_test<-predict(my.logit,x_test)
my.logit_train<-predict(my.logit,x_train)

# 計算準確度
tree_pred_train <- predict(tree_model, x_train, type = "class")
tree_pred_test <- predict(tree_model, x_test, type = "class")
train_accuracy <- mean(tree_pred_train == y_train)
test_accuracy <- mean(tree_pred_test == y_test)
cat("Accuracy of decision tree training:", train_accuracy, "\n")
cat("Accuracy of decision tree testing:", test_accuracy, "\n")
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(tree_pred_test, y_test))


library(caret)
# 繪製混淆矩陣_tree_test
tree_pred_train <- predict(tree_model, x_train, type = "class")
tree_pred_test <- predict(tree_model, x_test, type = "class")
conf_matrix <- table(tree_pred_test, y_test)
cat("tree_test")
print(confusionMatrix(conf_matrix))

cm <- confusionMatrix(tree_pred_test, y_test)$table

ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  # 添加文本标签
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()

# 繪製混淆矩陣_tree_train
tree_pred_train <- predict(tree_model, x_train, type = "class")
tree_pred_test <- predict(tree_model, x_test, type = "class")
conf_matrix <- table(tree_pred_train, y_train)
cat("tree_train")
print(confusionMatrix(conf_matrix))


cm <- confusionMatrix(tree_pred_train, y_test)$table

ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  # 添加文本标签
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()


# 繪製決策樹
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree_model, yesno = 2)

# 計算Decsion Tree AUC
install.packages("pROC")
library(pROC)
tree_pred_test <- predict(tree_model, x_test, type = "prob")
x_predict <- as.numeric(tree_pred_test[,1])
y_test_num <- as.numeric(y_test == 'n')

# Plot the test ROC curve

roc_curve <- roc(y_test_num, x_predict)
plot(roc_curve)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
title(main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
cat("AUC of Decision tree testing:", auc(roc_curve), "\n")


install.packages("InformationValue")
library(InformationValue)
#random forest找最佳切點
optCutOff_rf <- optimalCutoff(y_test_num, x_predict)[1]
print(optCutOff_rf)
rf_pred_opt<-ifelse(x_predict>=optCutOff_rf,1,0)


#看預測及實際的差異in test_set
confus_matrix_rf<-table(real=y_test_num,predicted=test_pred )
print(confus_matrix_rf)
confus_matrix_rf_opt<-table(real=y_test_num,predicted=rf_pred_opt)
print(confus_matrix_rf_opt)




#計算logistic regression AUC

install.packages("pROC")
library(pROC)
x_predict <- as.numeric(my.logit_test)
y_test_num <- as.numeric(y_test == 'n')

# Plot the test ROC curve

roc_curve <- roc(y_test_num, x_predict)
plot(roc_curve)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
title(main = paste("linear ROC Curve (AUC =", round(auc_value, 2), ")"))
cat("AUC of logistic testing:", auc(roc_curve), "\n")


#計算linear regression AUC

install.packages("pROC")
library(pROC)
x_predict <- as.numeric(lr_test)
y_test_num <- as.numeric(y_test == 'n')

# Plot the test ROC curve

roc_curve <- roc(y_test_num, x_predict)
plot(roc_curve)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
title(main = paste("logistic ROC Curve (AUC =", round(auc_value, 2), ")"))
cat("AUC of logistic testing:", auc(roc_curve), "\n")


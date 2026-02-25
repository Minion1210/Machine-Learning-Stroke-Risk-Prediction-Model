library(data.table)
library(caret)
library(ggplot2)
library(pROC)
#讀取數據
data<-data.table(fread("C:\\Users\\user\\Desktop\\高醫醫資\\碩一下\\大數據分析\\python\\healthcare-dataset-stroke-data-2.csv"))

#將數據標準化
data_scaled_X <- data.frame(scale(data[, 1:10]))
data_scaled <- cbind(data_scaled_X, stroke=data$stroke)

#將數據分為有病與沒病
stroke_yes <- data_scaled[data_scaled$stroke == "y", ]
stroke_no <- data_scaled[data_scaled$stroke == "n", ]
set.seed(255)  # 设置种子以确保结果可重现
train_has_disease_id <- sample(nrow(stroke_yes), size = round(0.8 * nrow(stroke_yes)))
train_has_disease <- stroke_yes[train_has_disease_id, ]
test_has_disease <- stroke_yes[-train_has_disease_id, ]

train_no_disease_id <- sample(nrow(stroke_no), size = round(0.8 * nrow(stroke_no)))
train_no_disease <- stroke_no[train_no_disease_id, ]
test_no_disease <- stroke_no[-train_no_disease_id, ]

#合併訓練集和測試集
train_data <- rbind(train_has_disease, train_no_disease)
test_data <- rbind(test_has_disease, test_no_disease)


install.packages("neuralnet")

library(neuralnet)
#使用 neuralnet 函數訓練神經網絡模型
model <- neuralnet(stroke ~ gender+age + hypertension + heart_disease + ever_married + work_type+Residence_type+smoking_status+avg_glucose_level + bmi, 
                   data = train_data, 
                   hidden = c(100,100,100),
                   linear.output = FALSE, 
                   learningrate = 0.01)
#plot(model)

#進行預測
predictions <- compute(model,test_data[1:10])


#提取預測值
predictions_result <- predictions$net.result


#將預測結果轉換為二元分類
predictions_binary <- ifelse(predictions_result[, 2] > 0.5, "y", "n")

#預測結果和真實結果轉換為因子變量
predictions_factor <- factor(predictions_binary, levels = c("n", "y"))
test_data_factor <- factor(test_data$stroke, levels = c("n", "y"))

install.packages("caret")
library(caret)

#混淆矩陣
conf_matrix <- confusionMatrix(predictions_factor, test_data_factor)
conf_matrix_t <- confusionMatrix(predictions_factor, test_data_factor)$table
#印出混淆矩陣
print(conf_matrix)

#混淆矩陣畫圖
ggplot(data = as.data.frame(conf_matrix_t), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black") +  # 添加文本标签
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "True", y = "Pred", fill = "Count") +
  theme_minimal()


install.packages("pROC")
library(pROC)

#ROC畫圖
true_labels <- ifelse(test_data$stroke == "y", 1, 0)

roc_obj <- roc(true_labels, predictions_result[, 2])



plot(roc_obj, main = "ROC Curve", col = "blue")
#加AUC 到圖例
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), col = "blue", lty = 1)

#計算ROC
auc_score <- auc(roc_obj)
#印出 AUC
print(paste("AUC:", auc_score))


install.packages("InformationValue")
library(InformationValue)
#ANN找最佳切點
optCutOff_ANN <- optimalCutoff(true_labels, predictions_result[, 2])[1]
print(optCutOff_ANN)
ANN_pred_opt<-ifelse(predictions_result[, 2]>=optCutOff_ANN,1,0)


#看預測及實際的差異in test_set
confus_matrix_ANN<-table(real=true_labels,predicted=predictions_result[, 2] )
print(confus_matrix_ANN)
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(factor(predictions_result[, 2]),factor(true_labels)))


confus_matrix_ANN_opt<-table(real=true_labels,predicted=ANN_pred_opt)
print(confus_matrix_ANN_opt)
library(caret)
cat("Classification Report:\n")
print(confusionMatrix(factor(ANN_pred_opt),factor(true_labels)))


true_labels <- ifelse(test_data$stroke == "y", 1, 0)
library(pROC)
roc_obj <- roc(true_labels, ANN_pred_opt)



plot(roc_obj, main = "ROC Curve", col = "blue")
#加AUC 到圖例
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), col = "blue", lty = 1)

#計算ROC
auc_score <- auc(roc_obj)
#印出 AUC
print(paste("AUC:", auc_score))

library(readxl)
library(data.table)
library(pROC)
#讀取數據
data <- fread("C:/Users/User/Desktop/研究所/資料科學與統計特論/期末報告/healthcare-dataset-stroke-data-4-continue.csv")
data[, age_group := ifelse(age %in% 1:30, 1, ifelse(age %in% 31:60, 2, 3))]


# 檢查結果
table(data$age)

table(data$stroke, data$ hypertension)#選出中風+有高血壓
prop.table(table(data$stroke, data$ hypertension))*100 #中風+有高血壓百分比
# 創建交叉表進行卡方檢定
hyper <- table(data$stroke, data$ hypertension)
chisq.test(hyper)

#BMI
# 篩選出中風為y和n的人數
subset_st <- subset(data, stroke == "y")
subset_non_st <- subset(data, stroke == "n")
# 算出中風 = y BMI 平均值
mean(subset_st$bmi, na.rm = TRUE)
sd(subset_st$bmi, na.rm = TRUE)
#算出中風 = n BMI 平均值
mean(subset_non_st$bmi, na.rm = TRUE)
sd(subset_non_st$bmi, na.rm = TRUE)
#跑t-test
t_test_BMI <- t.test(subset_st$bmi, subset_non_st$bmi,alternative = "two.sided" )
print(t_test_BMI)

#avg_glucose
#算出中風 = y avg_glucose 平均值
mean(subset_st$avg_glucose_level, na.rm = TRUE)
sd(subset_st$avg_glucose_level, na.rm = TRUE)
#算出中風 = n avg_glucose平均值
mean(subset_non_st$avg_glucose_level, na.rm = TRUE)
sd(subset_non_st$avg_glucose_level, na.rm = TRUE)
#跑t-test
t_test_glu <- t.test(subset_st$avg_glucose_level, subset_non_st$avg_glucose_level,alternative = "two.sided" )
print(t_test_glu)

table(data$stroke, data$heart_disease)#選出中風+有心臟病
prop.table(table(data$stroke, data$heart_disease))*100 #計算Smoker+高血壓百分比
#進行卡方檢定
heart <-table(data$stroke, data$heart_disease)
chisq.test(heart)

table(data$stroke, data$age_group) #選出年齡群+有高血壓
prop.table(table(data$stroke, data$age_group))*100 #計算年齡群+高血壓百分比


Age_chi <- table(data$stroke, data$age_group)
elements <- c(Age_chi[1, 1], 
              Age_chi[1, 2] + Age_chi[1, 3], 
              Age_chi[2, 1], 
              Age_chi[2, 2] + Age_chi[2, 3])
Age_chi_0_30 <- matrix(elements, nrow = 2, byrow = TRUE)
chisq.test(Age_chi_0_30)

elements_31_60 <- c(Age_chi[1, 2], 
                  Age_chi[1, 1] + Age_chi[1, 3], 
                  Age_chi[2, 2], 
                  Age_chi[2, 1] + Age_chi[2, 3])
Age_chi_31_60 <- matrix(elements_31_60, nrow = 2, byrow = TRUE)
chisq.test(Age_chi_31_60)

elements_61_90 <- c(Age_chi[1, 1] + Age_chi[1, 2], 
                  Age_chi[1, 3], 
                  Age_chi[2, 1] + Age_chi[2, 2],
                  Age_chi[2, 3])
Age_chi_61_90 <- matrix(elements_61_90, nrow = 2, byrow = TRUE)
chisq.test(Age_chi_61_90)

###
#高血壓和stroke邏輯回歸模型
data$stroke <- ifelse(data$stroke =="y", 1, 0)
hyper <- glm(stroke ~ hypertension,data = data, family = binomial(link="logit"))
summary(hyper)
#95%CI 
confint(hyper)
#OR 值
exp(coef(hyper))
exp(confint(hyper))

#BMI和stroke邏輯回歸模型
BMI <- glm(stroke ~ bmi, data = data, family = binomial(link="logit"))
summary(BMI)
#95%CI 
confint(BMI)
#OR 值
exp(coef(BMI))
exp(confint(BMI))

#avg_glucose_level和stroke邏輯回歸模型
avg_glu <- glm(stroke ~ avg_glucose_level, data = data, family = binomial(link="logit"))
summary(avg_glu)
#95%CI 
confint(avg_glu)
#OR 值
exp(coef(avg_glu))
exp(confint(avg_glu))

#Heart disease和stroke邏輯回歸模型
Heart_diseas <- glm(stroke ~ heart_disease, data = data, family = binomial(link="logit"))
summary(Heart_diseas)
#95%CI 
confint(Heart_diseas)
#OR 值
exp(coef(Heart_diseas))
exp(confint(Heart_diseas))

#Age和stroke邏輯回歸模型
data$age_group <- as.factor(data$age_group)
Age_group <- glm(stroke ~ age_group, data = data, family = binomial(link="logit"))
summary(Age_group)
#95%CI 
confint(Age_group)
#OR 值
exp(coef(Age_group))
exp(confint(Age_group))

# 調整並構建邏輯回歸模型

model <- glm(stroke ~ hypertension + heart_disease + ever_married + avg_glucose_level +
               bmi + age_group, data = data, family = binomial(link="logit"))
summary(model)
#95%CI 
confint(model)
#OR 值
exp(coef(model))
exp(confint(model))



#預測
predictions <- predict(model, newdata = data, type = "response")

#預測結果和實際結果
ROC <- data.frame(stroke = data$stroke, prediction = predictions)

#對結果進行 ROC 分析
roc_obj <- roc(ROC$stroke, ROC$prediction)

#ROC 曲線下面積 (AUC)
auc <- auc(roc_obj)
print(auc)

#畫ROC曲線
plot(roc_obj,plot=TRUE, print.auc = TRUE)












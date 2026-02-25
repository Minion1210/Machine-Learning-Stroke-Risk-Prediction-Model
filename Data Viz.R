library(data.table)
library(ggplot2)


data <- fread("C:\\Users\\User\\Desktop\\添加新資料\\healthcare-dataset-stroke-data.csv")

#刪除 gender 列為 "Other" 的行
data <- data[gender != "Other"]
#將age分組
data[, age_group := ifelse(age %in% 1:30, "1-30 years", ifelse(age %in% 31:60, "31-60 years", "61-90 years"))]
#將stroke== y改stroke
data$stroke <- ifelse(data$stroke  == "y", "stroke", "Non_stroke")
#將hypertension== 1改hypertension
data$hypertension <- ifelse(data$hypertension  == 1 , "hypertension", "Non_hypertension")
#將heart_disease== 1heart_disease
data$heart_disease <- ifelse(data$heart_disease  == 1 , "heart_disease", "Non_heart_disease")
##bmi補缺失值
# 将 "N/A" 值轉乘 NA
data$bmi[data$bmi == "N/A"] <- NA
# 算bmi平均值，保留一位小数（排除 NA 值）
mean_bmi <- round(mean(data$bmi, na.rm = TRUE), 1)
# 填缺失值
data$bmi[is.na(data$bmi)] <- mean_bmi

##Gender長條圖視覺化
#age_group和gender長條圖視覺化
p <-ggplot(data, aes(x = factor(age_group), fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Gender Across Different Age Groups", x = "Age_group", y = "Count")
# 在 p 上添加文本標籤
p + geom_text(stat = 'count', aes(label = after_stat(count), y = after_stat(count)), 
      position = position_dodge(width = 0.9), vjust = -0.5, size = 5.) + theme_minimal() +  # 使用最小化的主題風格
       theme( plot.background = element_rect(fill = "white", color = NA),  # 設置背景為白色
       panel.grid.major = element_blank(),  # 隱藏主要網格線
       panel.grid.minor = element_blank())   # 隱藏次要網格線


##Stroke和age
subset_st <- subset(data, stroke == "stroke")
subset_non_st <- subset(data, stroke == "Non_stroke")
# 算出中風  年齡平均值
mean(subset_st$age, na.rm = TRUE)
sd(subset_st$age, na.rm = TRUE)
#算出中風 = n BMI 平均值
mean(subset_non_st$age, na.rm = TRUE)
sd(subset_non_st$age, na.rm = TRUE)
#age盒鬚圖
ggplot(data, aes(x = as.factor(stroke), y = age, fill = as.factor(stroke))) +
  geom_boxplot() + xlab("Group") +  ylab("Age") +
  labs(fill = "stroke")+theme(panel.background =element_blank())
#age數據分布圖
ggplot(data, aes(x = age, fill = age)) +
  geom_density(alpha = 0.8,stat = "count") + 
  labs( x = "Age", y = "Count") +
  theme(panel.background = element_blank())

#age和stroke數據分布圖
ggplot(data, aes(x = age, fill = stroke)) +
  geom_area(stat = "count") +
  labs( x = "Age", y = "Count") +
  theme(panel.background =element_blank())

#age點狀圖
ggplot(data , aes(x = age)) + geom_point(stat = "count")+labs( x = "Age", y = "Count") +
  theme(panel.background =element_blank())

#age和stroke點狀圖
ggplot(data , aes(x = age, colour = stroke)) + geom_point(stat = "count")+labs( x = "Age", y = "Count")+
  theme(panel.background =element_blank())

##hypertension和stroke
# 創建 ggplot 對象 p，顯示高血壓長條圖
p <- ggplot(data = data, aes(x = hypertension, fill = hypertension)) +
  geom_bar(position = "dodge") +
  labs( x = "hypertension", y = "Count")
# 在 p 上添加文本標籤
p + geom_text(stat = 'count', aes(label = after_stat(count), y = after_stat(count)), 
              position = position_dodge(width = 0.5), vjust = -0.3, size = 5.) + theme_minimal() +  # 使用最小化的主題風格
  theme( plot.background = element_rect(fill = "white", color = NA),  # 設置背景為白色
         panel.grid.major = element_blank(),  # 隱藏主要網格線
         panel.grid.minor = element_blank()  )   # 隱藏次要網格線

#hypertension和stroke長條圖
p <-ggplot(data, aes(x = factor(hypertension), fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(x = "Group", y = "Count")
# 在 p 上添加文本標籤
p + geom_text(stat = 'count', aes(label = after_stat(count), y = after_stat(count)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 5.) + theme_minimal() +  # 使用最小化的主題風格
  theme( plot.background = element_rect(fill = "white", color = NA),  # 設置背景為白色
         panel.grid.major = element_blank(),  # 隱藏主要網格線
         panel.grid.minor = element_blank())   # 隱藏次要網格線

##heart_disease和stroke長條圖
# 創建heart_disease長條圖
p <- ggplot(data = data, aes(x = heart_disease, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs( x = "heart_disease", y = "Count")
# 在 p 上添加文本標籤
p + geom_text(stat = 'count', aes(label = after_stat(count), y = after_stat(count)), 
              position = position_dodge(width = 0.5), vjust = -0.3, size = 5.) + theme_minimal() +  # 使用最小化的主題風格
  theme( plot.background = element_rect(fill = "white", color = NA),  # 設置背景為白色
         panel.grid.major = element_blank(),  # 隱藏主要網格線
         panel.grid.minor = element_blank()  )   # 隱藏次要網格線
#heart_disease和stroke長條圖
p <-ggplot(data, aes(x = factor(heart_disease), fill = stroke)) +
  geom_bar(position = "dodge") +
  labs(x = "Group", y = "Count")
# 在 p 上添加文本標籤
p + geom_text(stat = 'count', aes(label = after_stat(count), y = after_stat(count)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 5.) + theme_minimal() +  # 使用最小化的主題風格
  theme( plot.background = element_rect(fill = "white", color = NA),  # 設置背景為白色
         panel.grid.major = element_blank(),  # 隱藏主要網格線
         panel.grid.minor = element_blank())   # 隱藏次要網格線

###avg_glucose_level和stroke小提琴圖
ggplot(data, aes(x = as.factor(stroke), y = avg_glucose_level, fill = as.factor(stroke))) +
  geom_violin(trim = FALSE, color = "white") +  # 繪製小提琴圖
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +  # 繪製箱線圖
  labs(x = "Group", y = "Average Glucose Level") +  # 設置坐標軸標籤
  theme_minimal() +  # 使用簡約主題
  theme(panel.grid.major = element_blank(),  # 移除主要網格線
        panel.grid.minor = element_blank())  # 

#avg_glucose_level
subset_st <- subset(data, stroke == "stroke")
subset_non_st <- subset(data, stroke == "Non_stroke")
# 算出中風 =y avg_glucose_level平均值
mean(subset_st$avg_glucose_level, na.rm = TRUE)
sd(subset_st$avg_glucose_level, na.rm = TRUE)
#算出中風 = n avg_glucose_level 平均值
mean(subset_non_st$avg_glucose_level, na.rm = TRUE)
sd(subset_non_st$avg_glucose_level, na.rm = TRUE)



#bmi點狀圖
ggplot(data , aes(x = bmi)) + geom_point(stat = "count")+labs( x = "BMI", y = "Count") +
  theme(panel.background =element_blank())




#bmi和stroke點狀圖
ggplot(data , aes(x = bmi, colour = stroke)) + geom_point(stat = "count")+labs( x = "BMI", y = "Count")+
  theme(panel.background =element_blank())






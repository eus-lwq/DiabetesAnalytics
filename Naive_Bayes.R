original_data <- read.csv("/Users/yuluchen/Library/CloudStorage/OneDrive-Personal/大学/大四/STAT 362/Final project/diabetes_012_health_indicators_BRFSS2015.csv")
library(ggplot2)

############################################################################
# Naive Bayes classification Cross validation (using for loop)
rm(list = ls())

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)


data <- read.csv("/Users/yuluchen/Library/CloudStorage/OneDrive-Personal/大学/大四/STAT 362/Final project/diabetes_012_health_indicators_BRFSS2015.csv")

# Data preprocessing
data_clean <- na.omit(data)

data_clean <- as.data.frame(lapply(data_clean, as.factor))
data_clean$BMI <- as.numeric(data_clean$BMI)
data_clean$PhysHlth <- as.numeric(data_clean$PhysHlth)
data_clean$MentHlth <- as.numeric(data_clean$MentHlth)
str(data_clean)

# Data Partition
acc <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  ind <- sample(2, nrow(data_clean), replace = T, prob = c(0.8, 0.2))
  train <- data_clean[ind == 1,]
  test <- data_clean[ind == 2,]
  model_com <- naive_bayes(Diabetes_012 ~ ., data = train, usekernel = T)
  #model_com
  #plot(model_com)
  
  # confusion matrix
  predictions <- predict(model_com, newdata = test)
  conf_mat <- confusionMatrix(predictions, test$Diabetes_012)
  accuracy <- conf_mat$overall["Accuracy"]
  acc[i] <- accuracy
}
acc # the highest accuracy is when i = 7, which is 0.8404.
mean(acc) # 0.8388

###########################################################################

# Data Partition
set.seed(7)
ind <- sample(2, nrow(data_clean), replace = T, prob = c(0.8, 0.2))
train <- data_clean[ind == 1,]
test <- data_clean[ind == 2,]

# kernel density estimation + categorical distribution
model_com <- naive_bayes(Diabetes_012 ~ ., data = train, usekernel = T)
model_com
plot(model_com)

# confusion matrix
predictions <- predict(model_com, newdata = test)
conf_mat <- confusionMatrix(predictions, test$Diabetes_012)
conf_mat$overall["Accuracy"]






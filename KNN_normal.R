rm(list = ls())
library(caret)
health <- read.csv("/Users/chenyiying/Downloads/archive (1)/diabetes_binary_health_indicators_BRFSS2015.csv", header = TRUE)
health <- na.omit(health)
 # Set a seed for reproducibility
trainIndex <- createDataPartition(health$Diabetes_binary, p = 0.70, list = FALSE)
train <- health[trainIndex, ]
test <- health[-trainIndex, ]

x = health[,-1] # features
y = health$Diabetes_binary
y_test <- test$Diabetes_binary

train_362 <- data.frame(Diabetes_binary = train$Diabetes_binary, HighBP=train$HighBP, HighChol = train$HighChol, CholCheck = train$CholCheck, 
                        BMI = train$BMI, Smoker = train$Smoker, Stroke = train$Stroke, 
                        HeartDiseaseorAttack = train$HeartDiseaseorAttack, PhysActivity = train$PhysActivity, 
                        Fruits = train$Fruits, Veggies = train$Veggies, HvyAlcoholConsump = train$HvyAlcoholConsump,
                        AnyHealthcare = train$AnyHealthcare, NoDocbcCost = train $ NoDocbcCost, GenHlth = train $ GenHlth,
                        MentHlth = train$MentHlth, PhysHlth = train$PhysHlth, DiffWalk = train$DiffWalk, 
                        Sex = train $ Sex, Age = train$Age, Education = train$Education, Income=train$Income  )
test_362 <- data.frame(Diabetes_binary = test$Diabetes_binary, HighBP=test$HighBP, HighChol = test$HighChol, CholCheck = test$CholCheck, 
                        BMI = test$BMI, Smoker = test$Smoker, Stroke = test$Stroke, 
                        HeartDiseaseorAttack = test$HeartDiseaseorAttack, PhysActivity = test$PhysActivity, 
                        Fruits = test$Fruits, Veggies = test$Veggies, HvyAlcoholConsump = test$HvyAlcoholConsump,
                        AnyHealthcare = test$AnyHealthcare, NoDocbcCost = test $ NoDocbcCost, GenHlth = test $ GenHlth,
                        MentHlth = test$MentHlth, PhysHlth = test$PhysHlth, DiffWalk = test$DiffWalk, 
                        Sex = test $ Sex, Age = test$Age, Education = test$Education, Income=test$Income  )
train_min <- apply(train_362, 2, min)
train_max <- apply(train_362, 2, max)

for (i in 1:ncol(train_362)) {
  train_362[, i] <- (train_362[,i] - train_min[i]) / (train_max[i] - train_min[i])
  test_362[, i] <- (test_362[,i] - train_min[i]) / (train_max[i] - train_min[i])
}

#train_mean <- apply(train_362, 2, mean)
#train_sd <- apply(train_362, 2, sd)

#for (i in 1:ncol(train_362)) {
  #train_362[, i] <- (train_362[, i] - train_mean[i]) / train_sd[i]
  # use the mean and sd from training data to normalize the testing data
  #test_362[, i] <- (test_362[, i] - train_mean[i]) / train_sd[i]
#}

library(class)
train_labels <- train$Diabetes_binary
test_labels <- test$Diabetes_binary
knn_predicted <- knn(train = train_362[, -1], test = test_362[, -1], 
                     cl = train_labels, k = 35)
table_362 <- table(test_labels, knn_predicted)
q7_acc <- sum(diag(table_362)) / sum(table_362)
q7_acc 

library(ggplot2)

compareTestPred <- data.frame(y_test=test$Diabetes_binary, y_pred = knn_predicted)
knn_predicted_factor <- factor(knn_predicted, levels = unique(union(knn_predicted, test_labels)))
test_labels_factor <- factor(y_test, levels = unique(union(knn_predicted, test_labels)))
cm1 <- confusionMatrix(knn_predicted_factor,test_labels_factor)
cm_data <- as.data.frame(cm1$table)
colnames(cm_data) <- c("Prediction", "Reference", "Frequency")
plot_cm <- ggplot(data = cm_data, aes(x = Reference, y = reorder(Prediction, -as.numeric(Prediction)), fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix", x = "True Label", y = "Predicted Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_cm)

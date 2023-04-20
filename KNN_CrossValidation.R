

library(caret)
library(class)

# Load the dataset
# Assuming the dataset is in a CSV format, replace 'path/to/your/health.csv' with the actual path
health <- read.csv('/Users/chenyiying/Downloads/archive/diabetes_012_health_indicators_BRFSS2015.csv', header = TRUE)

# Create a k-fold cross-validation index
k <- 5 # Number of folds for cross-validation
folds <- createFolds(health$Diabetes_012, k = k, list = TRUE)

# Define the range of possible 'k' values for kNN
k_values <- seq(37, 50, 2) # Odd values from 1 to 15

# Perform k-fold cross-validation with multiple variables
accuracy <- data.frame()

for (k_value in k_values) {
  results <- vector()
  for (i in 1:k) {
    # Split the dataset into training and testing sets
    train <- health[-folds[[i]], ]
    test <- health[folds[[i]], ]
    
    # Train the kNN model
    knn_pred <- knn(subset(train, select =c(HighBP,HighChol,CholCheck, BMI, Smoker,Stroke, HeartDiseaseorAttack, PhysActivity ,Fruits, Veggies, HvyAlcoholConsump,AnyHealthcare, NoDocbcCost, GenHlth, MentHlth, PhysHlth, DiffWalk, Sex, Age, Education, Income)),
                    subset(test, select = c(HighBP,HighChol,CholCheck, BMI, Smoker,Stroke, HeartDiseaseorAttack, PhysActivity ,Fruits, Veggies, HvyAlcoholConsump,AnyHealthcare, NoDocbcCost, GenHlth, MentHlth, PhysHlth, DiffWalk, Sex, Age, Education, Income)),
                    train$Diabetes_012, k = k_value)
    
    # Calculate the accuracy for the current fold
    accuracy_fold <- sum(knn_pred == test$Diabetes_012) / nrow(test)
    results <- c(results, accuracy_fold)
  }
  
  # Calculate and store the mean accuracy for the current k-value
  mean_accuracy <- mean(results)
  accuracy <- rbind(accuracy, data.frame(K = k_value, MeanAccuracy = mean_accuracy))
}

# Print the accuracy for each k-value
print(accuracy)

# Choose the best k-value based on the highest mean accuracy
best_k <- accuracy[which.max(accuracy$MeanAccuracy),]$K
print(paste0("Best k-value: ", best_k))

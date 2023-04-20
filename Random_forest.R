

# Methodology 
# Random Forest Model
#load the dataset
diabetes_ds <- read.csv("/Users/xujingjing/Desktop/archive/diabetes_012_health_indicators_BRFSS2015.csv")
library(ggpubr)
library(caret)
#install.packages("caret")
library(ggplot2)
#install.packages("randomForest")
library(randomForest)

# Getting data
data <- diabetes_ds
data <- na.omit(data)

# load the data and convert the response variable to factor
data$Diabetes_012 <- as.factor(data$Diabetes_012)

table(data$Diabetes_012)
# 0      1      2
# 213703   4631  35346


# split the data into training and test sets
set.seed(1)
train_index <- createDataPartition(data$Diabetes_012, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


#train the random forest model with default settings
rf_model <- randomForest(Diabetes_012 ~., data = train_data)
print(rf_model)


# Call:
#   randomForest(formula = Diabetes_012 ~ ., data = train_data)
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
#
# OOB estimate of  error rate: 15.16%
# Confusion matrix:
#   0 1    2 class.error
# 0 167593 0 3370  0.01971187
# 1   3411 0  294  1.00000000
# 2  23700 1 4576  0.83817237
# Ntree is 500 and mtry is 4





# Prediction & Confusion Matrix - trained the model on the training dataset
# p1 <- predict(rf_model, train_data)
# confusionMatrix(p1, train_data$ Diabetes_012)

# Confusion Matrix and Statistics
#
# Reference
# Prediction      0      1      2
# 0 170938   2229   9797
# 1      0   1413      0
# 2     25     63  18480
#
# Overall Statistics
#
# Accuracy : 0.9403
# 95% CI : (0.9393, 0.9413)
# No Information Rate : 0.8424
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.7378
#
# Mcnemar's Test P-Value : < 2.2e-16
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2
# Sensitivity            0.9999 0.381377  0.65353
# Specificity            0.6240 1.000000  0.99950
# Pos Pred Value         0.9343 1.000000  0.99526
# Neg Pred Value         0.9987 0.988627  0.94686
# Prevalence             0.8424 0.018256  0.13933
# Detection Rate         0.8423 0.006962  0.09106
# Detection Prevalence   0.9015 0.006962  0.09149
# Balanced Accuracy      0.8119 0.690688  0.82652





# Prediction & Confusion Matrix - trained the model on the testing dataset
p2 <- predict(rf_model, test_data)
confusionMatrix(p2, test_data$ Diabetes_012)

# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1     2
# 0 41945   859  5931
# 1     0     0     0
# 2   795    67  1138
#
# Overall Statistics
#
# Accuracy : 0.8492
# 95% CI : (0.846, 0.8523)
# No Information Rate : 0.8424
# P-Value [Acc > NIR] : 1.354e-05
#
# Kappa : 0.1861
#
# Mcnemar's Test P-Value : < 2.2e-16
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2
# Sensitivity            0.9814  0.00000  0.16098
# Specificity            0.1507  1.00000  0.98026
# Pos Pred Value         0.8607      NaN  0.56900
# Neg Pred Value         0.6025  0.98175  0.87830
# Prevalence             0.8424  0.01825  0.13933
# Detection Rate         0.8267  0.00000  0.02243
# Detection Prevalence   0.9606  0.00000  0.03942
# Balanced Accuracy      0.5661  0.50000  0.57062


# plot the Error rate of Random Forest model
plot(rf_model, main = "Random Forest Model")

# create a legend
legend("topright", legend = c("No diabetes", "Prediebetes", "Diabetes"),
       col = c("red", "green", "blue"), lty = 1, cex = 0.6)



# plot the Vriable importance plot 
importance(rf_model)
varImpPlot(rf_model,main = "Top 10 - Variable Importance")


# cross validation
library(tree)
mytree <- tree(Diabetes_012 ~., data = test_data)

cv_results <- cv.tree(mytree, FUN = prune.tree)

plot(cv_results$size, cv_results$dev, type = "b")

# $size
# [1] 4 3 2 1
# 
# $dev
# [1] 43862.19 46111.65 46111.65 49940.87
# 
# $k
# [1]     -Inf 1098.244 1158.737 3832.112
# 
# $method
# [1] "deviance"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"


mytree2 <- tree(Diabetes_012 ~., data = test_data, control = tree.control(nobs = dim(data)[1], mindev = 0.001))

cv_results2 <- cv.tree(mytree2, FUN = prune.tree)

plot(cv_results2$size, cv_results2$dev, type = "b")


# $size
# [1] 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1
# 
# $dev
# [1] 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48
# [11] 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 43864.48 46116.91
# [21] 46116.91 49942.16
# 
# $k
# [1]       -Inf   53.11414   54.40100   54.97448   55.39073   57.27067   61.03782   70.17224
# [9]   81.80276   96.34396   97.07996  113.00498  119.00582  141.73563  184.28188  225.12500
# [17]  239.64298  241.36039  394.37374 1098.24382 1158.73682 3832.11168
# 
# $method
# [1] "deviance"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"
# 





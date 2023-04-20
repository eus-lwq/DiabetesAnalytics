library(ggplot2)
library(ggpubr)
library(tidyverse)
library(GGally) # to use ggpairs
library(ISLR2)
library(tree)
library(rpart)
data<-read.csv("/Users/catherine/Downloads/diabetes_012_health_indicators_BRFSS2015.csv")
data$Diabetes_012[data$Diabetes_012==0]<-"no diabetes"
data$Diabetes_012[data$Diabetes_012==1]<-"prediabetes"
data$Diabetes_012[data$Diabetes_012==2]<-"diabetes"
data$Diabetes_012<-as.factor(data$Diabetes_012)
str(data)
set.seed(1)
random_index <- sample(nrow(data), nrow(data)*0.005) 
train <- data[random_index,]
test <- data[-random_index, ]
nrow(test)
treenum<-tree(Diabetes_012~., data=train)


treenum <- rpart(Diabetes_012 ~ ., data = train, control = rpart.control(cp = 0.001, minsplit = 2, minbucket = 1))
summary(treenum)
plot(treenum)
text(treenum)
text(treenum, pretty = 0)
summary(treenum)
plot(treenum)
text(treenum,pretty=0)
predictnum<-predict(treenum, test)
result <- table(test$Diabetes_012, predict(treenum, test, type = "class"))

result
sum(diag(result)) / sum(result)
predictions <- predict(treenum, test, type = "class")
tree
conf_matrix <- table(test$Diabetes_012, predictions)
conf_matrix
specificity <- conf_matrix[1,1] / sum(conf_matrix[1,])
print(specificity)
conf_matrix <- confusionMatrix(predictions, test$Diabetes_012)
kappa <- conf_matrix$overall['Kappa']
print(kappa)
cv_fit <- rpart.control(cp = seq(0, 0.05, 0.001))
treenum_cv <- rpart(Diabetes_012 ~ ., data = train, method = "class", control = cv_fit)
pruned_treenum <- prune(treenum, cp = treenum_cv$cptable[which.min(treenum_cv$cptable[, "xerror"]), "CP"], method = "class")
plotcp(treenum)

# Plot the pruned tree
library(rpart.plot)
prp(pruned_treenum)
cv_fit <- cv.tree(treenum, FUN = prune.misclass)
cv_fit
plot(cv_fit$size, cv_fit$dev, type = "b")

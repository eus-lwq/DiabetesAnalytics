library(tidyverse)
library(caret)
library(scales)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(ggpubr)
library(ggplot2)
rm(list = ls())
data  <- read.csv("D:/STAT362/diabetes_binary_health_indicators_BRFSS2015.csv", 
                    header = TRUE)
# Convert appropriate columns to factors
#data$Age <- as.factor(data$Age)
#data$Education <- as.factor(data$Education)
#data$Income <- as.factor(data$Income)
#data$GenHlth <- as.factor(data$GenHlth)

# Define column types
ord_bin_cols <- c("Age", "Education", "Income", "GenHlth","HighBP", 
                     "HighChol", "CholCheck", "Smoker", 
                     "Stroke", "HeartDiseaseorAttack", "PhysActivity", 
                     "Fruits", "Veggies", "HvyAlcoholConsump", 
                     "AnyHealthcare", "NoDocbcCost", "DiffWalk", "Sex")

ord_bin_data <- data[, ord_bin_cols]
cor_matrix <- cor(ord_bin_data, method = "spearman")
colnames(cor_matrix) <- ord_bin_cols
rownames(cor_matrix) <- ord_bin_cols


cor_df <- melt(cor_matrix)

# plot the heatmap
SP <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

numeric_columns <- c("BMI", "MentHlth", "PhysHlth")
num_data <- data[, numeric_columns]
cor_matrix_num <- cor(num_data)
cor_df_num <- melt(cor_matrix_num)

PC <- ggplot(cor_df_num, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
SP
ggarrange(SP, PC,
          ncol = 2, nrow = 1)

data[, numeric_columns] <- apply(data[, numeric_columns], 
                                   MARGIN = 2, 
            FUN = function(x) (x - min(x)) / (max(x) - min(x)))
# ========================== tests are done ==========================
set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(data$Diabetes_binary, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit the logistic regression model
model <- glm(Diabetes_binary ~ .,
             family = binomial(link = "logit"), 
             data = train_data)

# Model summary
#summary(model)

# Predict on the test set
predicted_probs <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
library(caret)
library(pROC)
library(ggplot2)

# Calculate confusion matrix
cm <- confusionMatrix(factor(predicted_classes), 
                      factor(test_data$Diabetes_binary))

library(titanic)
library(pROC)
# Calculate predicted probabilities
predicted_probs <- predict(model, newdata = test_data, type = "response")

# Calculate the ROC curve values
roc_curve <- roc(test_data$Diabetes_binary, predicted_probs)
plot(roc_curve, main = "ROC Curve",
     xlab = "True negative Rate (Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     col = "#1f77b4")
abline(a=0, b=1, lty=2, col="gray") # add diagonal line
auc_score <- auc(roc_curve) # calculate AUC score
legend("bottomright", 
       legend=paste("AUC =", round(auc_score, 2)), 
       cex=0.8, bty="n", col="blue")

# Load necessary packages
library(ggplot2)
library(reshape2)

# Convert confusion matrix to data frame
conf_df <- as.data.frame.matrix(cm$table)
conf_df$predicted_class <- rownames(conf_df)

# Melt data for plotting
conf_melted <- melt(conf_df, id.vars = "predicted_class")

# Plot confusion matrix as heatmap
ggplot(conf_melted, aes(x = predicted_class, y = variable)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "#edf8b1", high = "#2c7fb8") +
  labs(x = "Actual Class", y = "Predicted Class", title = "Confusion Matrix") +
  theme_minimal()+
  geom_text(aes(label = paste0(format((value / 50736) * 
                                        100, digits = 2), '%')), 
            size = 4)

cor_threshold <- 0.4

# identify highly correlated columns
highly_corr_cols <- findCorrelation(cor_matrix, cutoff = cor_threshold)

high_cor_names <- colnames(cor_matrix)[highly_corr_cols]
high_cor_names
# drop highly correlated columns from the data
data <- data[, -highly_corr_cols]
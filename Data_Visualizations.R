rm(list = ls())

library(ggplot2)
library(dplyr)
library(ggpubr)
library(leaps)
library(glmnet)
library(geosphere)
library(tidyverse)
library(gcookbook)
library(ggplot2)
library(caret)
library(ranger)
library(gbm)
library(lubridate)
library(geosphere)
library(dplyr)
library(caret)
library(scales)
library(reshape2)


dt_diabetic = read.csv("/Users/wanqing/Downloads/STAT 362/Dataset/diabetics/diabetes_012_health_indicators_BRFSS2015.csv", na.strings = c("", " ", "NA", "N/A"))
# check missing values
# sum(apply(dt_diabetic, 1, function(x) any(is.na(x) | x == ""))) # 1110
# nrow(dt_diabetic)

# Figure 2. plot age - diabete/bmi (bar/ multiple line)
ggplot(dt_diabetic, aes(x = factor(Diabetes_012), y = Age, fill = factor(Diabetes_012))) + 
  geom_boxplot(show.legend = FALSE) + ggtitle("Age vs Diabete box plot") +
  xlab("Diabetic") + 
  scale_x_discrete(labels = c('no diabetes','prediabetes', 'diabetes')) +
  scale_fill_manual(values = c("#277DA1", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Variable")

# Figure 3. sex - bmi (violin)
ggplot(dt_diabetic, aes(y = BMI, x = factor(Sex),fill = factor(Sex))) + 
  geom_violin(show.legend = FALSE) + ggtitle("Sex vs BMI bar plot") + xlab("Sex") + 
  scale_x_discrete(labels = c('female','male'))  + 
  scale_fill_manual(values = c("#4D908E", "#F3722C"),
                    labels = c("Female", "Male"),
                    name = "Variable")

# figure 4 Age - count distribution (bar/ multiple line)
dt_ageplot= dt_diabetic %>% count(Age)

ggplot(dt_ageplot, aes(x = factor(Age), y = n, fill = factor(Age))) + 
  geom_bar(stat='identity') + ggtitle("Age count bar plot") + xlab("Age") +
  scale_fill_discrete(name = "Age groups", labels = c('Age 18 - 24','Age 25 to 29', 'Age 25 to 29', 
                              'Age 30 to 34','Age 35 to 39','Age 40 to 44',
                              'Age 45 to 49', 'Age 50 to 54','Age 55 to 59',
                              'Age 60 to 64','Age 65 to 69','Age 70 to 74',
                              'Age 75 to 79','Age 80 or older')) +
  ylab("Number of records in dataset") + geom_line()


# Figure 5. BMI histogram
ggplot(dt_diabetic, aes(x = BMI)) +
  geom_histogram(fill = "#90BE6D",color = "#90BE6D", aes(y = ..density..)) +
  geom_density(color = "#277DA1") + ggtitle("BMI histogram & density")
  

# Figure 7. Diabetes Label Distribution
label_count = dt_diabetic %>% count(Diabetes_012)
ggplot(label_count, aes(x = factor(Diabetes_012), y = n, 
                        fill = factor(Diabetes_012))) + 
  geom_bar(stat='identity',show.legend = FALSE) + ggtitle("Diabete label plot") +
  xlab("Diabetic Level") + ylab("Number of records") +
  scale_x_discrete(labels = c('no diabetes','prediabetes', 'diabetes')) +
  scale_fill_manual(values = c("#277DA1", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Variable")

######################### xyq

diabetes_ds <- dt_diabetic

# figure 8. different diabetes status vs BMI
ggplot(diabetes_ds, aes(y = BMI, x = factor(Diabetes_012))) +
  geom_boxplot(fill = c("#577590", "#F9C74F", "#4D908E")) +
  ggtitle("Diabetes_012 vs BMI") +
  ylab("BMI") +
  xlab("Diabetes Status") +
  scale_x_discrete(labels = c("No diabetes", "Prediabetes", "Diabetes"))

# figure 9. Graph of Different Health status (HighBP, HighChol, Stroke, 
# HeartDiseaseorAttack, DiffWalking)
hlth_status <- diabetes_ds[, c(2, 3, 7, 8, 18)]
p <- ncol(hlth_status)
data <- data.frame(matrix(0, nrow = p * 2, ncol = 3))
colnames(data) <- c("hlth_status", "Type", "Count")
for (j in 1:p) {
  data[(2*j)-1, 3] <- sum(hlth_status[, j] == 0)
  data[2*j, 3] <- sum(hlth_status[, j] == 1)
}
data[, 1] <- rep(colnames(hlth_status), each = 2)
data[, 2] <- factor(rep(c(0, 1), times = 5))

ggplot(data, mapping = aes(x = hlth_status, y = Count, fill = Type)) +
  geom_col() +
  ylab("Number of Records in Dataset") +
  xlab("Health Status") +
  ggtitle("Participants Health Status") +
  scale_fill_manual(name = "Type", labels = c("No", "Yes"), values = c("#4D908E", "#F9844A"))

#figure 10. diabetes vs HighBP/HighChol/DiffWalk/HeartDiseaseorAttack/Stroke

plot1 <- ggplot(diabetes_ds)+ 
  aes(x = factor(HighBP), fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#577590", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "HighBP", y = "Proportion", fill = "")+labs(title="HighBP vs Diabetes")


plot2 <- ggplot(diabetes_ds)+ 
  aes(x = factor(HighChol), fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#577590", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "HighChol", y = "Proportion", fill = "")+labs(title="HighChol vs Diabetes")


plot3 <- ggplot(diabetes_ds)+ 
  aes(x = factor(DiffWalk), fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#577590", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "DiffWalk", y = "Proportion", fill = "")+labs(title="DiffWalk vs Diabetes")

plot4 <- ggplot(diabetes_ds)+ 
  aes(x = factor(HeartDiseaseorAttack), fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#577590", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "HeartDiseaseorAttack", y = "Proportion", fill = "")+labs(title="HeartDiseaseorAttack vs Diabetes")

plot5 <- ggplot(diabetes_ds)+ 
  aes(x = factor(Stroke), fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#577590", "#F9C74F", "#4D908E"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Stroke", y = "Proportion", fill = "")+labs(title="Stroke vs Diabetes")

ggarrange(plot1, plot2, plot3, plot4, plot5, nrow =1, ncol = 5)

#################### cyl

original_data <- dt_diabetic

# figure 13. Living and Eating habits VS Diabetes
# Veggie vs diabetes
veggie <- original_data %>% 
  ggplot(mapping = aes(x = factor(Veggies), fill = factor(Diabetes_012))) +
  geom_bar(position="fill") +
  scale_x_discrete(labels = c('No', 'Yes')) +
  scale_fill_manual(values = c("#F9C74F", "#F8961E", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetic status") +
  ggtitle("Veggies vs Diabetes") +
  xlab("Veggie") +
  ylab("Proportion")

# Alcohol vs diabetes
alcohol <- original_data %>% 
  ggplot(mapping = aes(x = factor(HvyAlcoholConsump), fill = factor(Diabetes_012))) +
  geom_bar(position="fill") +
  scale_x_discrete(labels = c('No', 'Yes')) +
  scale_fill_manual(values = c("#F9C74F", "#F8961E", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetic status") +
  ggtitle("HvyAlcoholConsump vs Diabetes") +
  xlab("Heavy Alcohol Consumption") +
  ylab("Proportion")

# Smoker vs diabetes
smoker <- original_data %>% 
  ggplot(mapping = aes(x = factor(Smoker), fill = factor(Diabetes_012))) +
  geom_bar(position="fill") +
  scale_x_discrete(labels = c('No', 'Yes')) +
  scale_fill_manual(values = c("#F9C74F", "#F8961E", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetic status") +
  ggtitle("Smoker vs Diabetes") +
  xlab("Smoker") +
  ylab("Proportion")

# Physical activity vs diabetes
pa <- original_data %>% 
  ggplot(mapping = aes(x = factor(PhysActivity), fill = factor(Diabetes_012))) +
  geom_bar(position="fill") +
  scale_x_discrete(labels = c('No', 'Yes')) +
  scale_fill_manual(values = c("#F9C74F", "#F8961E", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetic status") +
  ggtitle("PhysActivity vs Diabetes") +
  xlab("PhysActivity") +
  ylab("Proportion")

ggarrange(veggie, alcohol, smoker, pa)

################################################### annie
health <- dt_diabetic
PhysHlth <- health $ PhysHlth
MentHlth <- health $ MentHlth
GenHlth <- health $ GenHlth
x <- health $Diabetes_012

plot362 <- data.frame(Diabetes = x, GenHlth = GenHlth)

group <- group_by(health,Diabetes_012) 
q8_dep <-summarize(group,mean_phy = mean(PhysHlth, na.rm=TRUE)) 
q8_arr <- summarize(group,mean_men = mean(MentHlth, na.rm=TRUE)) 
q8 <- data.frame(type = q8_dep $ Diabetes_012, Hlth= c(q8_dep$mean_phy,q8_arr$mean_men), Type = c("physical health","physical health","physical health","mental health","mental health","mental health"))
q8 %>% 
  ggplot(health,mapping = aes(x = type, y = Hlth, fill =Type)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  ggtitle("Average Days in the past 30 days felt not good in health condition")+
  ylab("Average days during past 30 days")+
  xlab("Diabetes type: 0-No diabetes, 1-prediabetes, 2-diabetes")+
  scale_fill_manual(values = c("#F9c74f","#4d908e" ))




type012 <- health$Diabetes_012
n <- length(type012)
count0 <- 0
count1 <- 1
count2 <- 2
for (i in 1:n) {
  if (i == 0) {
    
  }
}



group <- group_by(health,Diabetes_012) 
gen_hlth <-summarize(group,mean_gen = mean(GenHlth, na.rm=TRUE)) 
gen <- data.frame(type = gen_hlth $ Diabetes_012, Hlth= gen_hlth$mean_gen, Type = "General Health")

gen %>% 
  ggplot(health,mapping = aes(x = type, y = Hlth, fill =Type)) +
  geom_bar(stat = "identity",
           position = "stack")+
  ggtitle("Average General Health Score for each diabetes types")+
  ylab("Average General Health Score (The higher the worse)")+
  xlab("Diabetes type")

ggplot(health, aes(x = factor(GenHlth), y = x)) +
  geom_boxplot() +
  labs(x = "General Health Score", y = "Category") +
  scale_x_discrete(labels = c("1" = "Label 1", "2" = "Label 2", "3" = "Label 3", "4" = "Label 4", "5" = "Label 5")) +
  theme_minimal()

h01<-nrow(filter(health, Diabetes_012 == 0&GenHlth == 1))
h02<-nrow(filter(health, Diabetes_012 == 0&GenHlth == 2))
h03<-nrow(filter(health, Diabetes_012 == 0&GenHlth == 3))
h04<-nrow(filter(health, Diabetes_012 == 0&GenHlth == 4))
h05<-nrow(filter(health, Diabetes_012 == 0&GenHlth == 5))
h11<-nrow(filter(health, Diabetes_012 == 1&GenHlth == 1))
h12<-nrow(filter(health, Diabetes_012 == 1&GenHlth == 2))
h13<-nrow(filter(health, Diabetes_012 == 1&GenHlth == 3))
h14<-nrow(filter(health, Diabetes_012 == 1&GenHlth == 4))
h15<-nrow(filter(health, Diabetes_012 == 1&GenHlth == 5))
h21<-nrow(filter(health, Diabetes_012 == 2&GenHlth == 1))
h22<-nrow(filter(health, Diabetes_012 == 2&GenHlth == 2))
h23<-nrow(filter(health, Diabetes_012 == 2&GenHlth == 3))
h24<-nrow(filter(health, Diabetes_012 == 2&GenHlth == 4))
h25<-nrow(filter(health, Diabetes_012 == 2&GenHlth == 5))

count_data <- data.frame(
  Diabetes_012 = c("1","1","1","1","1","2","2","2","2","2"),
  GenHlth = rep(1:5, 2),
  Count = c(h11, h12, h13, h14, h15,
            h21, h22, h23, h24, h25)
)

count_data %>% 
  ggplot(count_data,mapping = aes(x = factor(GenHlth), y = Count, fill =Diabetes_012)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  ggtitle("Average General Health Score for each diabetes types")+
  ylab("Average General Health Score ")+
  xlab("General Health Score")+
  scale_x_discrete(labels = c("1" = "1=Excellent", "2" = "2=very good", "3" = "3=good", "4" = "4=fair", "5" = "5=poor")) +
  scale_fill_manual(values = c("#277DA1","#4D908E"))

count_data <- data.frame(
  Diabetes_012 = c("0","0","0","0","0","1","1","1","1","1","2","2","2","2","2"),
  GenHlth = rep(1:5, 3),
  Count = c(h01, h02, h03, h04, h05,
            h11, h12, h13, h14, h15,
            h21, h22, h23, h24, h25)
)

count_data %>% 
  ggplot(count_data,mapping = aes(x = factor(GenHlth), y = Count, fill =Diabetes_012)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  ggtitle("Average General Health Score for each diabetes types
          0-No diabetes, 1-prediabetes, 2-diabetes")+
  ylab("Average General Health Score ")+
  xlab("General Health Score")+
  scale_x_discrete(labels = c("1" = "1=Excellent", "2" = "2=very good", "3" = "3=good", "4" = "4=fair", "5" = "5=poor")) +
  scale_fill_manual(values = c("#277DA1","#F9C74F","#4d908e"))

########################### sixuan
data <- dt_diabetic
data$Diabetes_012[data$Diabetes_012==0]<-"no diabetes"
data$Diabetes_012[data$Diabetes_012==1]<-"prediabetes"
data$Diabetes_012[data$Diabetes_012==2]<-"diabetes"
data$Diabetes_012<-as.factor(data$Diabetes_012)
a<-ggplot(data)+ aes(x = Veggies, fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#F3722C", "#90BE6D", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  labs(x = "Veggie", y = "Proportion", fill = "")+labs(title="Veggie vs Diabetes")


b<-ggplot(data)+ aes(x = HvyAlcoholConsump, fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#F3722C", "#90BE6D", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  labs(x = "Heavy Drinker", y = "Proportion", fill = "")+labs(title="Alcohol vs Diabetes")


c<-ggplot(data)+ aes(x = Smoker, fill = factor(Diabetes_012)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#F3722C", "#90BE6D", "#277DA1"),
                    labels = c("No diabetes", "Prediabetes", "Diabetes"),
                    name = "Diabetes") +
  labs(x = "Smoker", y = "Proportion", fill = "")+labs(title="Smoker vs Diabetes")
ggarrange(a,b,c)

data$Veggies[data$Veggies==0]<-"no"
data$Veggies[data$Veggies==1]<-"yes"
data$HvyAlcoholConsump[data$HvyAlcoholConsump==0]<-"no"
data$HvyAlcoholConsump[data$HvyAlcoholConsump==1]<-"yes"
data$Smoker[data$Smoker==0]<-"no"
data$Smoker[data$Smoker==1]<-"yes"



########################### lu

data  <- dt_diabetic
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

dia_012 <- dt_diabetic

ed_dia <- dia_012 %>%
  group_by(Education, Diabetes_012) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(Diabetes_012 = factor(Diabetes_012))

ed_dia_plt<- ed_dia %>%
  ggplot(aes(x = Education, y = Count, group = Diabetes_012, color = Diabetes_012)) +
  geom_bar(aes(fill = Diabetes_012), stat = "identity", position = "dodge") +
  geom_line(aes(linetype = Diabetes_012), position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9)) +
  labs(title = "Education Level vs Diabetes (Percentage)",
       x = "Education Level",
       y = "Percentage",
       fill = "Diabetes Status",
       color = "Diabetes Status",
       linetype = "Diabetes Status") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes")) +
  scale_color_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes")) +
  scale_linetype_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes"))

inc_dia <- dia_012 %>%
  group_by(Income, Diabetes_012) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(Diabetes_012 = factor(Diabetes_012))

inc_dia_plt<-inc_dia %>%
  ggplot(aes(x = Income, y = Count, group = Diabetes_012, color = Diabetes_012)) +
  geom_bar(aes(fill = Diabetes_012), stat = "identity", position = "dodge") +
  geom_line(aes(linetype = Diabetes_012), position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9)) +
  labs(title = "Income Level vs Diabetes (Percentage)",
       x = "Income Level",
       y = "Percentage",
       fill = "Diabetes Status",
       color = "Diabetes Status",
       linetype = "Diabetes Status") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes")) +
  scale_color_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes")) +
  scale_linetype_discrete(labels = c("No Diabetes", "Prediabetes", "Diabetes"))

inc_nodoc <- dia_012 %>%
  group_by(Income, NoDocbcCost) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ggplot(aes(x = factor(Income), y = Count, fill = factor(NoDocbcCost))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_fill(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("#8E44AD", "#F4D03F"),
                    labels = c("No", "Yes"),
                    name = "No Doctor Because of Cost") +
  labs(x = "Income Level", y = "Proportion of No Doctor Bc of Cost", fill = "") +
  theme(legend.position = "bottom")

hea_inc <- dia_012 %>%
  group_by(Income, AnyHealthcare) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ggplot(aes(x = factor(Income), y = Count, fill = factor(AnyHealthcare))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_fill(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("#3498DB", "#E67E22"),
                    labels = c("No", "Yes"),
                    name = "Any Health Care") +
  labs(x = "Income Level", y = "Proportion of have Health care", fill = "") +
  theme(legend.position = "bottom")

#cho_inc <- ggplot(dia_012, aes(x = factor(Income), fill = factor(CholCheck))) +
#  geom_bar(position = "fill") +
#  scale_fill_manual(values = c("#27AE60", "#E74C3C"),
#                    labels = c("No", "Yes"),
#                    name = "Cholesterol Check") +
#  labs(x = "Income Level", y = "% checked Cholesterol", fill = "") +
#  theme_minimal() +
#  theme(legend.position = "bottom")
ggarrange(ed_dia_plt, inc_dia_plt, inc_nodoc, hea_inc, 
          ncol = 2, nrow = 2)


# Convert "Diabetes_012" and "Fruits" to factors with correct levels
dia_012$Diabetes_012 <- factor(dia_012$Diabetes_012, 
                               levels = c(0, 1, 2),
                               labels = c("No diabetes or only during pregnancy", "Prediabetes", "Diabetes"))
dia_012$Fruits <- factor(dia_012$Fruits, 
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

ggplot(dia_012, aes(x = Fruits, fill = Diabetes_012)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3", "#CCEBC5"),
                    labels = c("No diabetes or only during pregnancy", "Prediabetes", "Diabetes"),
                    name = "Diabetes Status") +
  labs(x = "Fruit Consumption", y = "Proportion", fill = "") +
  theme_minimal()


inc_nodoc_data <- dia_012 %>%
  group_by(Income, NoDocbcCost) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
inc_nodoc_data
# Income Level vs Any Health Care percentages
hea_inc_data <- dia_012 %>%
  group_by(Income, AnyHealthcare) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
hea_inc_data
# Education Level vs Diabetes percentages
edu_data <- dia_012 %>%
  group_by(Education, Diabetes_012) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
edu_data


# Income Level vs Diabetes percentages
inc_data <- dia_012 %>%
  group_by(Income, Diabetes_012) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
inc_data



rm(list = ls())


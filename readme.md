
<!-- ABOUT THE PROJECT -->
# Diabetes Health Indicator Analytics
Presentation google slides: https://docs.google.com/presentation/d/12Mv9h681LueRBsFd8HxsX2r532fXqK083eDiIJ7rN1k/edit?usp=sharing </br>
Report google doc: https://docs.google.com/document/d/1Qf1MbEfts9M1Enn2ZzlN5cLkTpCuBZx5t9W_OfliO3w/edit?usp=sharing </br>

### Contributors
* Wanqing Li 20146670 | 18wl19@queensu.ca
* Yulu Chen 20150856 | 18yc124@queensu.ca
* Lu Chen 20164422 | 18lc44@queensu.ca
* Yingqi Xu 20158700 | 18yx84@queensu.ca
* Sixuan Zhang 20204414 | 19sz61@queensu.ca
* Yiying Chen 20198951 | 19yc56@queensu.ca

### Instructor: Dr. Brian Ling

### Motivation
According to IDF Diabetes statistics [1], there are five thirty seven million adults all over the world living with diabetes which means 1 in 10 adults will experience diabetes.  Diabetes is responsible for 6.7 million deaths in 2021 which means 1 Death every 5 seconds. Diabetic patients more likely to be hospitalized due to serious risks such as Blindness, Heart Disease, Loss of arm or legs, kidney failure and reduce lifetime etc. Meanwhile, Diabetes also causes hundreds of billion US dollars in health expenditure around the world. That's why it's important for us to understand how to prevent and manage diabetes effectively. By analyzing risk factors. and use. We hope that we can help people to know their diabate possibility as an early screening tool. Make people live healthier. And reduce the overall burden of this disease on our society. To achieve our goal, we chose a dataset called Diabete Health Indicator [2] from Kaggle, prior works on Kaggle are mostly using the Neural Networks to train prediction models, and we only use statistical methods in this report to try to compare with them.

### Dataset Visualizations
The dataset we used is from Kaggle "Diabetes health indicators dataset [2]", the visualization file can be found in *Data_Visualizations.R*.
Can check the report section 2.0 in *STAT362_Diabetes_health_indicator_analytics_report.pdf*

### Approaches
All the files need to change the csv file path to *diabetes_012_health_indicators_BRFSS2015.csv*.
* Naïve Bayes - *Naive_Bayes.R*
* Decision tree - *Decision_Tree.R*
* Random forest - *Random_Forest.R*
* Logistic regression - *Logistic_ Regression.R*
* K Nearest Neighbor (KNN) - *KNN_normal.R* contains KNN method without using cross validation, *KNN_CrossValidation.R* contains cross validation.

### Result
check the report in *STAT362_Diabetes_health_indicator_analytics_report.pdf*

### Contributions
Contributions:
* Wanqing Li: organizations, data visualizations, build clustering model (discarded), presentation, introduction, discussion and conclusion
* Sixuan Zhang: data visualizations, build decision tree model, presentation
* Yingqi Xu: data visualizations, build random forest model, presentation
* Yiying Chen: data visualizations, build KNN model, presentation
* Yulu Chen: data visualizations, build Naïve Bayes model, presentation
* Lu Chen: data visualizations, build logistic regression model, presentation

### References
[1] International Diabetes Federation. (2021). Resources. Diabetes Atlas. Retrieved April 20, 2023, from https://diabetesatlas.org/resources/?gclid=EAIaIQobChMIiavtuqO3_gIVBQtlCh1L0AgDEAAYASAAEgJkx_D_BwE 

[2] Teboul, A. (2021). Diabetes health indicators dataset. Kaggle. Retrieved April 20, 2023, from https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset 

[3] Zhang Z. (2016). Naïve Bayes classification in R. Annals of translational medicine, 4(12), 241. https://doi.org/10.21037/atm.2016.03.38

[4] What is Naïve Bayes | IBM. (n.d.). https://www.ibm.com/topics/naive-bayes#:~:text=The%20Na%C3%AFve%20Bayes%20classifier%20is,a%20given%20class%20or%20category.

[5] Ling, B. (n.d.). Decision Tree.  Retrieved April 19, 2023, from https://onq.queensu.ca/d2l/le/content/763470/viewContent/4648485/View

[6] JavaTpoint. (n.d.). Machine Learning Random Forest Algorithm - Javatpoint. Www.javatpoint.com. https://www.javatpoint.com/machine-learning-random-forest-algorithm 

[7] Introduction to Random Forest in Machine Learning. (n.d.). Engineering Education (EngEd) Program | Section. Retrieved April 20, 2023, from https://www.section.io/engineering-education/introduction-to-random-forest-in-machine-learning/#:~:text=Advantages%20of%20random%20forest%201%20It%20can%20perform 

[8] Ling, B. (n.d.). K-nearest neighbors. Retrieved April 19, 2023, from https://brian-ling.github.io/k-nearest-neighbors.html

Acknowledgement: Our work is polished and grammar is fixed by ChatGPT.


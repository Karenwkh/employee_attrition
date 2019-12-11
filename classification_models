---
title: "Classification Models"
author: "Karen Wong"
date: "11/20/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction 

Employee turnover is a large problem for some companies – especially when it comes to the company’s high potentials. When an employee leaves the organization, the organization loses money. There is an additional number of things that may happen as well: Knowledge and contacts are lost, Negative impact on colleagues, Onboarding of new hires and Hiring is expensive. 

It is useful to uncover the factors that lead to employee attrition and even predict who are the ones who would leave the company. 
This articie will detail the data analysis on employee attrition based on a data set created by IBM data scientists.  The dataset can be found here https://www.ibm.com/communities/analytics/watson-analytics-blog/hr-employee-attrition/.

## Importing the dataset and checking the dimensions
```{r}

dataset_original <- read.csv("hr.csv")
str(dataset_original)
dataset <- dataset_original
names(dataset_original)
dataset_original_num <- dataset_original[,-c(3,5,8,12,16,18,22,23)]

```

The dataset has 1470 observations with 35 variables.


## Cleaning up the data
```{r}
# Rename a column in R
names(dataset)[1]<-"Age"
str(dataset)
head(dataset,50)

library(lattice)
library(caret)
nearZeroVar(dataset)

```

When we closely observe the structure of the dataset we find that the attributes Over18, EmployeeCount and StandardHours all carry the same value for each observation. EmployeeNumber is just the index of the employee number. We therefore drop these columns.

```{r}

dataset$EmployeeNumber=dataset$Over18=dataset$EmployeeCount=dataset$StandardHours = NULL

```


# EDA (Exploratory Data Analysis)

## Numerical Summaries
```{r}
summary(dataset)

```


```{r}
# Proportion of employee attrition
prop.table(table(dataset$Attrition))
```
The percentage of attrition is more than 80%. 


## Graphical summaries

We will try to analyse the variables visually to understand the nature of data in terms of distribution of the variables/features comparing those who left the company (Attrition = Yes) and those who did not (Attrition = No)

```{r}
library(ggplot2)

g <- ggplot(dataset)+theme_bw()

#Attrition:
g+geom_bar(aes(Attrition,fill=Attrition))
```

```{r}
#People in the company are 5 times more than the people leaving the company

#Yearssincelastpromotion vs Attrition
g+geom_bar(aes(YearsSinceLastPromotion,fill=Attrition))
```

```{r}
#People recently promoted quit the company more than the ones not promoted

#YearsWithCurrentManager vs Attrition:
g+geom_bar(aes(YearsWithCurrManager,fill=Attrition))
```
```{r}
#As the number of years with Current Manager increases, Attrition decreases

#TrainingTimeLastYear vs Attrition:
g+geom_bar(aes(TrainingTimesLastYear,fill=Attrition))
```

```{r}
#Attrition seen in employees trained betweeb 2-4times last year.

#YearsatCompany vs Attrition
g+geom_point(aes(YearsAtCompany,Attrition,size=YearsAtCompany))
```

```{r}
#People with less no of years tend to quit the company more.

#TotalWorking Years vs Attrition
g+geom_bar(aes(TotalWorkingYears,fill=Attrition))
```

```{r}
#People with less no of years tend to quit the company more.

#TotalWorking Years vs Attrition
g+geom_bar(aes(TotalWorkingYears,fill=Attrition))
```

```{r}
#People with Less Percent Hike leave the company.

#OverTime vs Attrition
g+geom_bar(aes(OverTime,fill=Gender,colour=Attrition))
```

```{r}
#Male employees working overtime leave the company more

#WorkLifeBalance vs Attrition
g+geom_bar(aes(WorkLifeBalance,fill=Attrition))
```

```{r}
#People with better work life balance may tend to quit more.

#Marital Status vs Attrition
g+geom_bar(aes(MaritalStatus,fill=Attrition))
```

```{r}
#Attrtion higest in Employees who are single,medium in Employees who are married and least in Divorced Employees.

#JObRole vs Attrition
g+theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                           size = 10, angle = 90))+
          (aes(JobRole))+
          geom_bar(aes(fill=Attrition))
```
```{r}
#Job ROle of Sales Representative has the most attrition in various job roles present.

#JobInvolvement vs Attrition
g+(aes(JobInvolvement))+geom_bar(aes(fill=Attrition))
```

```{r}
#People leaving the company are highly involved in their jobs

#JobSatisfaction vs Attrition:
g+(aes(JobSatisfaction))+geom_bar(aes(fill=Attrition))
```

```{r}
#Low JobSatisfaction results in people leaving the company.

#StockOptionLevels vs Attrition:
g+(aes(StockOptionLevel))+geom_bar(aes(fill=Attrition))
```

```{r}
#Attrition high in people with No or Less Stock Options 

#Gender vs Attrition
g+geom_bar(aes(Attrition,fill=Gender))
```


```{r}
#More in Male employees

#DistanceFromHome vs Attrition
g+geom_histogram(binwidth=40,aes(DistanceFromHome,fill=Attrition))
```

```{r}
#People living in shorter distances from office leave more.

#Hourly,Daily & Monthly Rates vs Attrition:
g+geom_point(aes(DailyRate,Attrition),alpha = 0.05)
```

```{r}
#Education Field, Education vs Attrition
g+geom_bar(aes(Education,fill=Attrition))
```

```{r}
g+geom_bar(aes(EducationField,fill=Attrition))
```

```{r}
#Attrtion seems to be higher in Bachelors, Life Sciences and Medical

#Department vs Attrition
g+aes(Department,fill=Attrition)+geom_density(position = "stack")
```


```{r}
#People from Reasearch&Development and Sales tend to quit more compared to HR

#Business Travel vs Attrition
g+geom_bar(aes(BusinessTravel,fill=Attrition))
```


```{r}
#Attrition is directly proportional to Travel among the employees.

#Age vs Attrition
g+geom_histogram(binwidth = 10,aes(Age,fill=Attrition),colour="Black")
```


```{r, fig.width= 12}
#Employees around 30years leave the company more

#Others:
g+geom_bar(aes(OverTime,fill=Attrition))+facet_grid(.~JobRole,scales = "free")
```


```{r}
library(psych)
str(dataset)
```


After carefully examining the correlation plot and table, we find that there are a lot of correlated features. We run the basic logistic regression model to examine which features are to be dropped from the pair of each correlated features. This has been done by excluding one feature and dropping the other and vice-versa while running the algorithm. After several iterations, it is concluded that features like MaritalStatus, MonthlyIncome and PerformanceRating degrade the model. We hence drop these columns.

```{r, fig.height=8}


#dataset$MaritalStatus=dataset$MonthlyIncome=dataset$PerformanceRating= NULL
```

```{r}
str(dataset)


```

## Alternative Graphs 

Let’s now do some bivariate/multivariate analysis. Here we’ll explore the independent variables with respect to the target variable. The objective is to discover hidden relationships between the independent variables and the target variable.

A. Employee Personal Demographics - Numerical Variables
```{r}

library(dplyr)
library(gridExtra)
pA <- dataset %>%
  ggplot(aes(x = Age, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Age") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pB <- dataset %>%
  ggplot(aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Distance From Home")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pC <- dataset %>%
  ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Number of Companies")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

pD <- dataset %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Total Working Years")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

grid.arrange(pA, pB, pC, pD, nrow = 2, ncol = 2)
```

Observations :

Younger employees within 25-35 years have a higher attrition rate.
We see a lower attrition rate when the Distance from home is within 10 kms. The attrition rate increase post 10kms.
The attrition rate tends to be higher with employees who have worked with 5 to 7 companies.
Attrition rate seems to be extremely high amongst employees who have a total working experience between 0 to 7 years approximately.

B. Employee Billing Rate Demographics - Numerical Variables

```{r}
pE <- dataset %>%
  ggplot(aes(x = HourlyRate, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Hourly Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pF <- dataset %>%
  ggplot(aes(x = DailyRate, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Daily Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pG <- dataset %>%
  ggplot(aes(x = MonthlyRate, fill = Attrition)) + geom_density(alpha = 0.5)+ ggtitle("Monthly Rate") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


grid.arrange(pE, pF, pG)
```
Observations :

No specific trend observed except for Daily rate where we see two modes. The attrition is highest near 400 whereas the attrition is lowest near 1200.
C. Employee Work Demographics - Numerical Variables


```{r}

pH <- dataset %>%
  ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Percentage Salary Hike") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pI <- dataset %>%
  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years At Company") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pJ <- dataset %>%
  ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years in Current Role") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pK <- dataset %>%
  ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years Since Last Promotion") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


pL <- dataset %>%
  ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years With Current Manager") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())


grid.arrange(pH, pI, pJ, pK, pL , nrow = 3, ncol = 2)
```
Observations :

We also see a peak in attrition rate when the employee is with the company for 0-2 years approx.
From the two modes observed in “Years in Current Role” plot we can say that the attrition rate is higher when the employee is in the same role for 0-2 years or 6 years approx.
Also, From the two modes observed in “Years with current manager” plot we can say that employee also tends to leave if he is with the same manager for less than 1.5 years or 6 years approximately.

D. Employee Personal Demographics - Categorical Variables

```{r}
pM <- dataset %>%
  group_by(Gender) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Gender, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Gender") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))


pN <- dataset %>%
  group_by(Education) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Education, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Education") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))

pO <- dataset %>%
  group_by(EducationField) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = EducationField, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Education Field") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))



pP <- dataset %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(RelationshipSatisfaction), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Relationship Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

pQ <- dataset %>%
  group_by(WorkLifeBalance) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(WorkLifeBalance), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Work Life Balance") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 35))

grid.arrange(pM, pN, pO, pP, pQ, nrow = 2, ncol = 3)
```
Observations :

Attrition Rate is slightly more in Males as compared to Females.
18% attrition rate is observed amongst employees have below college education.
Attrition rate is very high amongst employees from HR, Marketing and Technical backgrounds.
As expected, the attrition rate is very high amongst employees who have a bad work life balance.
E. Employee Work Demographics - Categorical Variables

```{r}
p1 <- dataset %>%
  group_by(BusinessTravel) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = BusinessTravel, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Business Travel") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p2 <- dataset %>%
  group_by(EnvironmentSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = EnvironmentSatisfaction, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Environment Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p3 <- dataset %>%
  group_by(JobInvolvement) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = JobInvolvement, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Job Involvement") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 40))

p4 <- dataset %>%
  group_by(JobSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = JobSatisfaction, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Job Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p5 <- dataset %>%
  group_by(OverTime) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = OverTime, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "coral3") + ggtitle("Attrition Rate - Over Time") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 35))



grid.arrange(p1, p2, p3, p4, p5,  nrow = 2, ncol = 3)
```
Observations :

Attrition rate is higher amongst people who travel frequently.
Its also higher amongst employees who have a low environment satisfaction, low job involvement and low job satisfaction.
The attrition rate is almost 30% amongst employees who work over time.
Sales department have the highest attrition at 20% whereas Sales Representatives have the highest attrition at 40%.


# Modeling

As we are predicting the attrition as output which can either have a yes or a no output, this analysis falls under the classification category.

We will be building three models to predict the attrition of an employee. These models are :

Naive Bayes
Logistic Regression
Decision Tree
Random Forest
K- Nearest Neighbour
Neural Network

Let’s start implementing the models. The method we will be following for all models is :

Train the model on the training dataset.
Predicting the output for the testing dataset.
Use the confusion matrix to check the accuracy.
Calculate the AUC(Area Under Curve) of the ROC plot. (Higher the AUC better the performance)



We need to ensure that the churn data in both training and test set is of the same proporation as we have it in our main dataset to avoid any biases in prediction. For this we will use createDataPartition function.Our training dataset with have 70% of the rows whereas the test dataset will have the remaining 30%.


## Dividing the dataset into training and test set
```{r}
library(caret)

#dataset_intrain <- sample(nrow(dataset),nrow(dataset)*0.8)
#train <- dataset[dataset_intrain,]
#test <- dataset[-dataset_intrain,]


set.seed(777)
library(caTools)

split = sample.split(dataset$Attrition, SplitRatio = 0.70)

# Create training and testing sets
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

```


To make sure that the classes are equally represented in proportion in both the train and test sets
```{r}
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))
```
From the results above, it seems that the proportions in both the train and test sets are similarly represented.

## Baseline Model
Baseline Accuracy - If we just predict attrition as “No” for every observation, we will get an accuracy of 83.90%.

```{r}
# Baseline Accuracy
table(test$Attrition)
baseline <- 370/nrow(test)
```
Accuracy of the predictive models should be higher than the baseline accuracy.


## Naive Bayes

A Naive Bayes classifier is a essentially a probabilistic machine learning model that’s used for classification task. The crux of the classifier is based on the Bayes theorem.
Bayes Theorem to calculate conditional probability:
$P(A|B) = \frac{P(A \cap B)}{P(B)} = \frac{P(B | A)P(A)}{P(B)}$

Using Bayes theorem, we can find the probability of A happening, given that B has occurred. Here, B is the evidence and A is the hypothesis. The assumption made here is that the predictors/features are independent. That is presence of one particular feature does not affect the other. Hence it is called naive.

```{r}

#train <- dataset[dataset_intrain,-2]
#test <- dataset[-dataset_intrain,-2]

train_labels <- train[,2]
test_labels <- test[,2]


library(e1071)
dataset_NBmodel <- naiveBayes(train[,-2], train_labels, laplace = 1)
dataset_NBmodel

```
### Training the model using Naive Bayes Classifier
```{r}
Attrition_pred_NB <- predict(dataset_NBmodel, test)
```


### Evaluating the naive Bayes classifier
Let's create a confusion matrix as well as it's estimated accuracy on unseen data:
```{r}
table(prediction = Attrition_pred_NB, actual=test_labels)
accuracy_NB <- sum(Attrition_pred_NB == test_labels)/length(test_labels)*100

accuracy_NB
```
Based on the above calculations, the accuracy of the prediction using the Naive Bayes classifier is `r accuracy_NB`



### ROC for Naive Bayes

```{r}
# ROC for Naive Bayes
library(ROCR)

ROCRpred <- prediction(as.numeric(Attrition_pred_NB), as.numeric(test$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_lr, digits=5, scientific=FALSE)))

```



## Decision Tree
Decision trees and tree-based models are powerful, incredibly versatile and are is quite likely the single most popular choice for machine learning tasks. Their output are also a powerful form of representing rules, which has the benefit of being interpretable and adaptable to almost any scenario.  

``````{r}
library(partykit)
Attrition_dt_model <- ctree(Attrition ~ ., train)
plot(Attrition_dt_model)
plot(Attrition_dt_model, type="simple")


```
Based on the decision tree, the importance features of attrition seems to be overtime, age, job role, environment satistifaction, Years with Current Manager, StockOption Level and Job Satisfaction, business travel and Education Field.

### Evaluating the performance of the decision tree model
```{r}
Attrition_pred_dt <- predict(Attrition_dt_model,test[,-2])

confusionMatrix(Attrition_pred_dt, test[,2])
```
Based on the decision tree above, the accuracy is 84.35%


## Random Forest

The Random Forest Classifier. Random forest, like its name implies, consists of a large number of individual decision trees that operate as an ensemble. Each individual tree in the random forest spits out a class prediction and the class with the most votes becomes our model's prediction
Let's create our Random Forest, using a 5-Fold Cross Validation, with 3 repeats as follows:

```{r eval=F}

set.seed(100)
dataset_train <- train
ctrl <- trainControl(method="repeatedcv", number=5, repeats=3)
#dataset_forest <- train(Attrition ~ ., data=dataset_train, method="rf", trControl = ctrl)

saveRDS(dataset_forest, file = "dataset_forest.RDS")
```

```{r}
dataset_forest <- readRDS("dataset_forest.RDS")
dataset_forest


```

We can get a visual representation of this selection process and confirm that the model constructed with mtry=21 gives us the highest cross-validation accuracy.
```{r}
plot(dataset_forest)
```

### Evaluating the performance of the random forest classification model
```{r}
Attrition_pred_RF <- predict(dataset_forest, test[,-2])
table(Attrition_pred_RF, test$Attrition)
accuracy_RF <- sum(Attrition_pred_RF == test_labels)/length(test_labels)*100

#(234+7)/(234+7+3+50)


```
Based on the above calculations, the accuracy of the random forest classifier is `r accuracy_RF`

### Random Forest Model 2

```{r}
# Load Random Forest package
library(randomForest)

modelRf = randomForest(Attrition ~ ., data=train, ntree = 100, mtry = 5, importance = TRUE, method="class")

#Plot the model
print(modelRf)
```

```{r}
#OOB vs No. Of Trees
plot(modelRf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")
```

### Feature Importance
```{r}
## List the importance of the variables.
impVar <- round(randomForest::importance(modelRf), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

```
```{r, figure.width=8}
library(randomForest)
varImpPlot(modelRf,type=1)

```


The important features according to the plot of Mean Accuracy above are Overtime, Job Level, Age, YearsAtCompany, StockOption Level, Job Role, Years With Current Manager, Total Working Years, Years in Current Role, Number of Companies Worked. 



```{r}
## Tuning Random Forest
tunedRf <- tuneRF(x = train[,-2], 
              y=as.factor(train$Attrition),
              mtryStart = 5, 
              ntreeTry=60, 
              stepFactor = 2, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 5, 
              importance=TRUE
              )
```

```{r}
impvarTunedRf <- tunedRf$importance
impvarTunedRf[order(impvarTunedRf[,3], decreasing=TRUE),]
```

```{r}
predictionRf2 <- predict(tunedRf, test[,-2], type="class")

#RandomForest Accuracy
#Confusion matrix 
t2 <- table(test$Attrition, predictionRf2)

#RandomForest model accuracy
accuracy_RF2 <- (t2[1]+t2[4])/(nrow(test))*100
accuracy_RF2
```
Based on the above calculations, the accuracy of the prediction using the Random Forest classifier 2 is `r accuracy_RF2`


```{r}
library(gmodels)
CrossTable(x=predictionRf2 , y=test$Attrition, prop.chisq = FALSE, prop.t = FALSE,dnn=c("prediction", "actual"),)
```


## Logistic Regression

```{r}
#Model 1
Attrition_glm1 <- glm(Attrition ~ ., data=train, family="binomial") 

summary(Attrition_glm1)

# From the summary of our logistic regression model, creditrisk, the more significant variables are installment, delinq_2yrs and home_ownership as can be seen from the p -values that are less than 0.05.                      

step(Attrition_glm1)

#Using the both direction stepwise technique to select the variables for the logistic-Regression Model, we have the following model with the lowest AIC value: not_paid ~ installment + annual_inc + grade + delinq_2yrs



```


```{r}
#Model 2
Attrition_glm2 <- glm(formula = Attrition ~ Age + BusinessTravel + DistanceFromHome + 
    Education + EducationField + EnvironmentSatisfaction + Gender + 
    JobInvolvement + JobRole + JobSatisfaction + NumCompaniesWorked + 
    OverTime + RelationshipSatisfaction + StockOptionLevel + 
    TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
    YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
    YearsWithCurrManager, family = "binomial", data = train)



```


 

```{r}

# Assign the results of the probability-prediction of the entire dataset.test data set as a new column called "pred.Risk"
test$pred.Attrition.glm1 <- predict(Attrition_glm1, test, type = "response")

# Plot a histogram of "pred.Risk"
hist(test$pred.Attrition.glm1, breaks=20)
abline(v = 0.5, col = "blue", lwd = 3)

test$pred.Attrition2 <- predict(Attrition_glm2, test, type = "response")
hist(test$pred.Attrition2, breaks=20)
abline(v = 0.5, col = "red", lwd = 3)

```




### Confusion model for Logistic Regression Model 1 with all the variables

```{r}
# Assign the previous confusion matrix to "contab"
contab <- table("predicted"=as.numeric(test$pred.Attrition.glm1>=0.5), "actual"=test$Attrition)
a <- as.numeric(contab[1])
b <- as.numeric(contab[2])
c <- as.numeric(contab[3])
d <- as.numeric(contab[4])

accuracy_glm1 <- (a+c)/(a+b+c+d)*100
accuracy_glm1

```
Based on the above calculations, the accuracy of the prediction using the Logistic Regression Model 1 is `r accuracy_glm1`

### Confusion matrix for Logistic Regression Model 2
```{r}
# Assign the previous confusion matrix to "contab"
contab2 <- table("predicted"=as.numeric(test$pred.Attrition2>=0.5), "actual"=test$Attrition)

a2 <- as.numeric(contab2[1])
b2 <- as.numeric(contab2[2])
c2 <- as.numeric(contab2[3])
d2 <- as.numeric(contab2[4])

accuracy_glm2<- (a2+c2)/(a2+b2+c2+d2)*100
accuracy_glm2

table(prediction = test$pred.Attrition2, actual=test_labels)
```
Based on the above calculations, the accuracy of the prediction using the Logistic Regression Model 2 is `r accuracy_glm2`



### RoC for Logistic Regression Model 

```{r}

# for Logistic Regression Model 2

ROCRpred <- prediction(as.numeric(test$pred.Attrition2), as.numeric(test$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_lr, digits=5, scientific=FALSE)))

```

### Feature Importance
```{r}
summary(Attrition_glm2)
```
From the Logistic Regression model 2, we can see that those with the more significant features (Pr(>|z|) being very small, with  ***) are 
 BusinessTravelTravel_Frequently, DistanceFromHome, EnvironmentSatisfaction, JobRoleSales Representative  ,JobSatisfaction , NumCompaniesWorked , Overtime (Yes), StockOptionLevel ,YearsInCurrentRole, YearsSinceLastPromotion.
 


## Knn - K Nearest Neighbour

Let's try another classification method knn and compare our results with that of the logistic regression method.The advantage of Knn is that the code is shorter. But since there is no model built, and it can be computationally Expensive to rerun for new datasets, especially if the new dataset has a huge number of rows and columns.

### Re-scaling variables

```{r}
# Create a normalize() function, which takes a vector x and for each value in that vector, returns a value between 0 to 1 where 0 is the minimum value and 1 is maximum value in that vector.

normalize <-function(x){
  return ( 
    (x - min(x))/(max(x) - min(x)) 
           )
}
```


```{r}
# Re-scaling using scale() function.
# Note that using the scale() function is identical to standardizing the values into a standard bell curve (giving a z-score). All the means are set to zero and all standard deviations are "standardized". ie: All the bell curves of every variable look identical.

# Just like the normalize() function above, you would need to apply "scale()" to every variable.
```

### Prepare the dataset for knn method.

As Knn classification method only works for numerical values, we will prepare a dataset with only numerical values.
```{r}
str(train)
# Attrition, Business Travel, Department, Education Field, Gender, JobRole, Overtime
names(train)
dataset_num <- dataset[,-c(3,5,8,10,14,18)]
str(dataset_num)
names(dataset_num)

dataset_n <- as.data.frame(lapply(dataset_num[,c(1,3:22)], normalize))

head(dataset_n)
sum(is.na(dataset_n))
```



### Splitting into training and test set
```{r}

set.seed(98765)
split <- sample(nrow(dataset_n),nrow(dataset_n)*0.80)
str(split)

dataset_n_train <- dataset_n[split, ]
dataset_n_test <- dataset_n[-split, ]


sum(is.na(dataset_num))


# Remember we excluded our target variable (diagnosis) in wbcd_n (excluded column 1)?
# Give the respective train and test sets their labels in "wbcd_train_labels" and "wbcd_test_labels".
dataset_n_train_labels <- dataset_num[split,2]

dataset_n_train_labels <- ifelse(dataset_n_train_labels =="Yes",1,0)

dataset_n_test_labels <- dataset_num[-split,2]
dataset_n_test_labels <- ifelse(dataset_n_test_labels =="Yes",1,0)



```

```{r}
# We need to install and load library(class) for knn function.
library(class)

kvalue1 <- round(sqrt(nrow(dataset_n_train)),0)


#cl is the class, k can use square root of nrows of training set  
dataset_pred_knn1 <- knn(train = dataset_n_train, test =dataset_n_test, cl=dataset_n_train_labels, k=kvalue1)


```

### Confusion matrix for Knn 

```{r}


CrossTable(x=dataset_n_test_labels , y=dataset_pred_knn1,dnn=c("actual", "prediction"))
243/294


```

```{r}
# Compare accuracy of knn with that of baseline model

accuracy_knn <- 0.8265*100
ifelse(accuracy_knn > baseline*100, "knn is better that baseline model"," knn is worse than baseline model")
```


## Neural Network

```{r}
library(nnet)
set.seed(777)
modelNN<-nnet(Attrition~.,train,size=21,rang=0.07,Hess=FALSE,decay=15e-4,maxit=2000)

```

```{r}
predictionNN<-predict(modelNN,test,type=("class"))
table(predictionNN, test_labels)
accuracy_NN <- sum(predictionNN == test_labels)/length(test_labels)*100
accuracy_NN
```
Based on the above calculations, the accuracy of the prediction using the Neural Network classifier is `r accuracy_NN`




# Conclusion

To conclude, we have seen the entire process where we started with importing the dataset, getting to know the dataset at a high level, carrying out EDA and then moving on to data pre processing and then finally building models to predict the classification. The important features according to the analysis above are Overtime (Yes), Job Role, Total Working Years, Years at company, Years in Current Role, Job Satisfaction and Travel (Frequently).

Every model comes with parameters which can be used to tune the models to obtain higher accuracy and specific results.
Let's compare the accuracy of the different models used below.

```{r}
# Decision Tree
accuracy_DTree <- 84.35
#Accuracy <- c(accuracy_glm2,accuracy_glm1, accuracy_RF, accuracy_RF2,accuracy_NB, accuracy_NN, accuracy_DTree, accuracy_knn)
Accuracy <- c(accuracy_glm2,accuracy_glm1, accuracy_RF, accuracy_RF2,accuracy_DTree)
model <- c("Log Reg 2", "Log Reg 1", "Random Forest", "Random Forest 2","Decision Tree")

modelaccuracy <- data.frame(model, Accuracy)
baselineaccuracy <- baseline*100

graph <- plot(modelaccuracy)
abline(h=baselineaccuracy, col="red")

```

KNN and NN model fall below the baseline model accuracy of `r baselineaccuracy`. 

Logistic Regression Model 2 has the highest accuracy. This model can be used to predict new data of employees frequently (e.g. bi-yearly) to predict whether they will leave the company or not. After identifying those who will likely leave, we can decide if they are valuable employees that should be retained. The data of those who are predicted to leave and be sent to the Human Resource Department or Project Managers to see if any appropriate intervention is required. 


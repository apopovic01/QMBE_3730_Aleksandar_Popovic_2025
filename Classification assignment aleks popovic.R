install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
library(tidyverse)
library(dp)
loandata<-loan_default_data_set

#1
summary(loandata)
view(loandata)
#There are 20 000 rows and 21 columns 
#2
colSums(is.na(loandata))
# Percentantage of open credit cards with over 50% utilization have 1958 missing values
#Annual income has 1559 missing values
#Education has 1 missing value
loandata1<-na.omit(loandata)
#removed all NA from dataset

#3
duplicates<-loandata1%>%
  group_by_all()%>%
  filter(n()>1)%>%
  ungroup()
duplicates
#there are no duplicates in the dataset

#4
# if we were to have duplicates we would use the distinct() function from dplyr to remove all duplicates
# to correctly classify data we would use the as. functions such as as.factor or as.numeric

#5
plot(loandata1$credit_age,loandata1$tot_balance,main="Credit age vs Total balance",xlab="Credit Age", ylab="Total Balance",col="red")

#6
count(loandata1,rep_education)
#other is underrepresented 

#7
count(loandata1,Def_ind)
# the data is not balanced as there are 14956 that did not default and 1697 that did default
# when dealing with unbalanced data we would use an F1 score, AUC-ROC method, PR curve,
#log loss
#8
summary(loandata1$rep_income)
#It is technically slightly left skewed although it is by a difference of 100 meaning you could say
#it is approximately normal

#9
default_rates<-loandata1%>%
  group_by(rep_education)%>%
  summarize(
    total_count=n(),
    default_count=sum(Def_ind),
    default_rate=mean(Def_ind)*100)
default_rates

#People who's highest level of education is achieved have the most likely hood to end up defaulting on a loan at a 11.8% chance

### Seperate training and test sets
set.seed(2)
split <- sample.split(loandata1$Def_ind,SplitRatio = 0.8)
train <- subset(loandata1,split == TRUE)
test <- subset(loandata1,split == FALSE)



# Train and evaluate KNN
train$Def_ind<-as.factor(train$Def_ind)
test$Def_ind<-as.factor(test$Def_ind)
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5)
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$Def_ind))

#Accuracy, precision, recall
#accuracy:
2962+19/2962+339+10+19
2981/3330
#Accuracy=89.52%
#Precision:
19/29
#Precision=65.52%
#recall
19/(19+339)
#Recall is 5.3%
# the confusion matrix has 2962 true negatives, 19 true positives, 339 false negatives, and 10 false positives
#3 ROC

roc_curve <- roc(test$Def_ind, pred_knn)  
plot(roc_curve, col = "blue", main = "ROC Curve")
auc(roc_curve) 
#AUC 0.6434


# Train and evaluate Decision Tree


dt_model <- train(Def_ind~.,data=train,method='rpart')
pred_dt <- predict(dt_model, test)
print(confusionMatrix(pred_dt, test$Def_ind))
#Accuracy, precision, recall
#accuracy:
(2966+17)/(2962+335+12+17)

#Accuracy=89.69%
#Precision:
17/29
#Precision=58.62%
#recall
17/(17+335)
#Recall is 4.83%
#4 
#number of cards opened in the last 12 months is one of the most important variables as it is significiant at the 0.1% level

#5
#f1 score of KNN
2*(0.6551724*0.05307263)/(0.6551724+0.05307263)
#F1= 9.82%

#F1 of DT

2*(0.5862069*0.04829545)/(0.5862069+0.04829545)
#0.08923884

#The KNN model is better as it has a higher F1 score
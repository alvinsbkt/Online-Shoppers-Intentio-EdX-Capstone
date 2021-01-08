if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")


library(readr)
library(tidyverse)
library(caret)
library(data.table)
library(funModeling)
library(DMwR)
library(MLmetrics)
library(randomForest)
library(xgboost)
library(naivebayes)




# dataset from UCI Machine Learning Repository "https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv"
online_shoppers_intention <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv")

# checking that there are no missing value at all
sum(is.na(online_shoppers_intention))

# showing the first 6 rows of the dataset
head(online_shoppers_intention)



################################################################
###DATA PREPROCESSING###
################################################################

# changing month abbreviation into number representing the month of the year
# checking whether every month already have the proper abbreviation
unique(online_shoppers_intention$Month)

# changing "June" to its proper abbreviation which is "Jun"
online_shoppers_intention$Month[online_shoppers_intention$Month=='June']<-'Jun'
# changing abbreviation to numbers
online_shoppers_intention$Month<-match(online_shoppers_intention$Month,month.abb)


# encoding Weekend column to 0 (FALSE) and 1 (TRUE)
online_shoppers_intention$Weekend<-factor(online_shoppers_intention$Weekend,levels = c(FALSE,TRUE),labels=c(0,1))


# checking the levels in VisitorType
unique(online_shoppers_intention$VisitorType) 
# encode VisitorType 0 for returning visitors, 1 for new visitors, 2 for other
online_shoppers_intention$VisitorType<-factor(online_shoppers_intention$VisitorType,levels=c("Returning_Visitor","New_Visitor","Other"),labels=c(0,1,2))

# encode revenue(target label) with 0 (FALSE) and 1 (TRUE)
online_shoppers_intention$Revenue<-factor(online_shoppers_intention$Revenue,levels = c(FALSE,TRUE),labels=c(0,1))


# making sure all column is in its proper data type
# numeric for continuous or discrete data, factor for categorical data
# make sure its on the right data type
online_shoppers_intention$Administrative<-as.numeric(online_shoppers_intention$Administrative)
online_shoppers_intention$Administrative_Duration<-as.numeric(online_shoppers_intention$Administrative_Duration)
online_shoppers_intention$Informational<-as.numeric(online_shoppers_intention$Informational)
online_shoppers_intention$Informational_Duration<-as.numeric(online_shoppers_intention$Informational_Duration)
online_shoppers_intention$ProductRelated<-as.numeric(online_shoppers_intention$ProductRelated)
online_shoppers_intention$ProductRelated_Duration<-as.numeric(online_shoppers_intention$ProductRelated_Duration)
online_shoppers_intention$BounceRates<-as.numeric(online_shoppers_intention$BounceRates)
online_shoppers_intention$ExitRates<-as.numeric(online_shoppers_intention$ExitRates)
online_shoppers_intention$PageValues<-as.numeric(online_shoppers_intention$PageValues)
online_shoppers_intention$SpecialDay<-as.numeric(online_shoppers_intention$SpecialDay)
online_shoppers_intention$Month<-factor(online_shoppers_intention$Month)
online_shoppers_intention$OperatingSystems<-factor(online_shoppers_intention$OperatingSystems)
online_shoppers_intention$Browser<-factor(online_shoppers_intention$Browser)
online_shoppers_intention$Region<-factor(online_shoppers_intention$Region)
online_shoppers_intention$TrafficType<-factor(online_shoppers_intention$TrafficType)


# summary after preprocessing
summary(online_shoppers_intention) 
# still no missing values which means all data is properly transformed or encoded
sum(is.na(online_shoppers_intention))



################################################################
###DATA VISUALIZATION###
################################################################

# visualizing distributions of numeric columns
# there is a trend in most columns where the value of y axis will decrease exponentially as the x axis increases
plot_num(online_shoppers_intention%>%select(-Revenue),bins=10)


# log transformed plot for Administrative duration has a distribution relatively similar to normal distribution
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(Administrative_Duration,fill=Revenue))+
  geom_histogram(bins=15,color='black')+
  xlab('Log transformed Administrative Duration')+
  ggtitle('Log transformed Distribution of Administrative Duration')+
  scale_x_log10()+theme_light()


# log transformed plot for Informational duration has a distribution relatively similar to normal distribution
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(Informational_Duration,fill=Revenue))+
  geom_histogram(color='black',bins=15)+
  xlab('Log transformed Informational Duration')+
  ggtitle('Log transformed Distribution of Informational Duration')+
  scale_x_log10()+theme_light()


# log transformed plot for Product related duration has a distribution relatively similar to normal distribution
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(ProductRelated_Duration,fill=Revenue))+
  geom_histogram(bins=15,color='black')+
  xlab('Log transformed Product Related Duration')+
  ggtitle('Log transformed Distribution of Product Related Duration')+
  scale_x_log10()+theme_light()


# log transformed plot for Bounce Rate
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(BounceRates,fill=Revenue))+
  geom_histogram(color='black',bins=10)+
  xlab('Log transformed Bounce Rates')+
  ggtitle('Log transformed Distribution of Bounce Rates')+
  scale_x_log10()+theme_light()


# log transformed plot for Exit Rate
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(ExitRates,fill=Revenue))+
  geom_histogram(color='black',bins=10)+
  xlab('Log transformed Exit Rates')+
  ggtitle('Log transformed Distribution of Exit Rates')+
  scale_x_log10()+theme_light()


# plot for Regions
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(Region,fill=Revenue))+
  geom_histogram(color='black',stat='count')+
  ggtitle('Distribution of Region')+
  theme_light()


# plot for Traffic type
# shown below that there is a similar distribution of this column for both class of revenue
online_shoppers_intention %>%
  ggplot(aes(TrafficType,fill=Revenue))+
  geom_histogram(color='black',stat='count')+
  theme_light()


# after further inspection for other categorical variable, 
# every variable shows similar distribution between classes of revenue


# as shown below, there is a significant difference of amount in each class of revenue
# SMOTE will be used later to tackle this problem
online_shoppers_intention%>%
  ggplot(aes(Revenue))+geom_histogram(stat='count')
table(online_shoppers_intention$Revenue)

################################################################
###FEATURE ELIMINATIOAN AND SMOTE
################################################################

# Applying random forest elimination to eliminate redundant feature
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(online_shoppers_intention[,-which(colnames(online_shoppers_intention)=='Revenue')], online_shoppers_intention$Revenue, sizes=c(1:18), rfeControl=control)

#In case the code above run for too long, this is the feature obtained from the code
predictor<-c("PageValues","ExitRates", "BounceRates", "Month", 
             "ProductRelated_Duration", "ProductRelated", "VisitorType",
             "Administrative","TrafficType", "Administrative_Duration",
             "Informational_Duration", "Informational","OperatingSystems",
             "Browser","Weekend")

# summarize the results shows the importance of the 18 features
# we can see that the peak is located when there are 10 features
print(results)
plot(results, type=c("g", "o"))

# as seen below are the list of the 10 chosen features
predictors(results)


# creating train data and test data with the trimmed features
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = online_shoppers_intention$Revenue, times = 1, p = 0.2, list = FALSE)
train_set <- online_shoppers_intention[-test_index,c(predictors(results),'Revenue')]
test_set <- online_shoppers_intention[test_index,c(predictors(results),'Revenue')]

#alternative code
#train_set <- online_shoppers_intention[-test_index,c(predictor,'Revenue')]
#test_set <- online_shoppers_intention[test_index,c(predictor,'Revenue')]


# split to dataset with features and a vector of y for train set and test set
y_train<-as.factor(train_set$Revenue)
x_train<-train_set[,-which(colnames(train_set)=='Revenue')]
y_test<-as.factor(test_set$Revenue)
x_test<-test_set[,-which(colnames(test_set)=='Revenue')]


# current amount observation for each revenue class
table(train_set$Revenue)
# Performed SMOTE on the training set by generating additional twice the current amount of minority class (perc.over)
# undersampled majority class as many as twice the amount of newly generated observation (perc.under)
newdata<-SMOTE(Revenue~.,as.data.frame(train_set),perc.over=200,perc.under=200)
# new amount of observation for each revenue class, more balanced amount
table(newdata$Revenue)


# new x train and y train
y_train_smote<-as.factor(newdata$Revenue)
x_train_smote<-newdata[,-which(colnames(newdata)=='Revenue')]



################################################################
###MODELLING - TRAINING DATA COMPARISON
################################################################

# F1 will be used as our parameter since imbalance dataset could still have high accuracy but
# low performance in predicting minority class (low sensitiviy/specificity)
# since positive class is the minority, then it is likely to have low sensitivity

# First of all, comparing using normal training set and modified by SMOTE training set
RF_fit <- randomForest(x=x_train,y=y_train) 
plot(RF_fit)
confusionMatrix(predict(RF_fit, x_test), y_test,positive='1')
F1_Score(y_test,predict(RF_fit, x_test))

RF_fit_smote <- randomForest(x=x_train_smote,y=y_train_smote) 
plot(RF_fit_smote)
confusionMatrix(predict(RF_fit_smote, x_test), y_test,positive='1')
F1_Score(y_test,predict(RF_fit_smote, x_test))

#also comparing with naive prediction, predicting all as class 0 in revenue
naive_pred<-factor(rep(0,length(y_test)),levels=c(0,1))
confusionMatrix(naive_pred, y_test,positive='1')
F1_Score(y_test,naive_pred)

smote_results <- data_frame(Method = "Naive Predict '0'",F1 = F1_Score(y_test,naive_pred), 
                            Accuracy=mean(y_test==naive_pred), 
                            Sensitivity=sensitivity(naive_pred, y_test,positive='1'))
smote_results <- bind_rows(smote_results,
                           data_frame(Method="RF Normal train set",  
                                      F1=F1_Score(y_test,predict(RF_fit, x_test)),
                                      Accuracy=mean(y_test==predict(RF_fit, x_test)),
                                      Sensitivity=sensitivity(predict(RF_fit, x_test), y_test,positive='1')))
smote_results <- bind_rows(smote_results,
                           data_frame(Method="RF SMOTE train set",  
                                      F1=F1_Score(y_test,predict(RF_fit_smote, x_test)),
                                      Accuracy=mean(y_test==predict(RF_fit_smote, x_test)),
                                      Sensitivity=sensitivity(predict(RF_fit_smote, x_test), y_test,positive='1')))
smote_results %>% knitr::kable()


# Creating a new table for model comparison using training set modified SMOTE
model_results <- data_frame(Algorithm = "Random Forest",F1 = F1_Score(y_test,predict(RF_fit_smote, x_test)))



################################################################
###MODELLING - ALGORITHM COMPARISON
################################################################

# comparing with other Supervised learning models
# XGBoost
# creating XGBoost matrix for training using XGBoost model
xgb_train<-xgb.DMatrix(data=data.matrix(x_train_smote),label=y_train_smote)
xgb_test<-xgb.DMatrix(data=data.matrix(x_test),label=y_test)

# training and evaluating performance of XGBoost
xgb_fit<-xgboost(xgb_train,nrounds=50)

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="XGBoost",
                                      F1=F1_Score(y_test,as.factor(levels(y_test)[round(predict(xgb_fit, xgb_test))]))))


# Logistic Regression (GLM)
train_glm <- train(x=x_train_smote,y=y_train_smote, method = "glm")

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="Generalized Linear Model",
                                      F1=F1_Score(predict(train_glm, x_test), y_test)))


# k-Nearest Neighbour
train_knn <- train(x=x_train_smote,y=y_train_smote, method = "knn")

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="k-Nearest Neighbour",
                                      F1=F1_Score(predict(train_knn, x_test), y_test)))


# Naive Bayes
naive_fit<-naive_bayes(x=x_train_smote,y=y_train_smote)

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="Naive Bayes",
                                      F1=F1_Score(predict(naive_fit,x_test),y_test)))


#Decision Tree
DT_fit<-train(x=x_train_smote,y=y_train_smote,method='rpart')

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="Decision Tree",
                                      F1=F1_Score(y_test,predict(DT_fit, x_test))))


#Random Forest has the best F1 Score
model_results %>% knitr::kable()


#Tuning in Random Forest

#Define f1 function for the metric to be optimized in hyperparameter tuning
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

# define the hyperparameters that will be tuned (mtry)
# since from the previous plot we can see that with ntree>100 only increase runtime without any
# significant improvement, we are going to limit ntree by 100
control_rf <- trainControl(method="repeatedcv", number=10,
                           repeats=3,summaryFunction = f1)
tunegrid<-expand.grid(.mtry=seq(5,50,5))
set.seed(1,sample.kind = 'Rounding')
rf_gridsearch <- train(Revenue~.,data=newdata,
                       method="rf", metric='F1', tuneGrid=tunegrid, ntree=100,
                       trControl=control_rf)
print(rf_gridsearch)
plot(rf_gridsearch)

rf_gridsearch$results

model_results <- bind_rows(model_results,
                           data_frame(Algorithm="Tuned Random Forest",
                                      F1=F1_Score(y_test,predict(rf_gridsearch, x_test))))

model_results %>% knitr::kable()


################################################################
###MODELLING - FINAL MODEL PERFORMANCE
################################################################

confusionMatrix(predict(rf_gridsearch, x_test), y_test,positive='1')



################################################################
### Appendix
################################################################
print("Operating System:")
version

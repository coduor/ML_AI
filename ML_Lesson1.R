# # # # # # # # # # # # # # # # # # # # #
#INTRODUCTION TO MACHINE LEARNING IN R:DATA SCIENCE DOJO
#10102023
#CLIFFORD ODUOR,PhD 
# # # # ###################################
rm(list=ls())
#install.packages(c("e1071","doSNOW","ipred","xgboost","caret"))
library(caret)
train<-read.csv("C:/DATA SCIENCE/R/Titanic/train.csv",stringsAsFactors = FALSE)
View(train)
table(train$Embarked)
train$Embarked[train$Embarked==""]<-"S"
summary(train$Age)
#implement a tracking feature to take care of the missing values in age
train$missingAge<-ifelse(is.na(train$Age),"Y","N")
train$Familysize<-1+train$SibSp+train$Parch
#set up factors
train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)
train$Sex<-as.factor(train$Sex)
train$Embarked<-as.factor(train$Embarked)
train$missingAge<-as.factor(train$missingAge)
#subset data to features we wish to keep/use
str(train)
features=subset(train,select=-c(PassengerId,Name,Ticket,Cabin))
#Imputation
#First,transform all features to dummy variables
dummy.vars<-dummyVars(~.,data=features[,-1])
train.dummy<-predict(dummy.vars,features[,-1])
View(train.dummy)
#Now,impute
pre.process<-preProcess(train.dummy,method="bagImpute")
imputed.data<-predict(pre.process,train.dummy)
View(imputed.data)
features$Age<-imputed.data[,6]
View(features)
#Use caret to create a 70/30% split of the training data,
#keeping the proportion of the Survived class label the
#same across splits
set.seed(2345)
indexes<-createDataPartition(features$Survived,times=1,p=0.7,list =FALSE)

titanic.train<-features[indexes,]
titanic.test<-features[-indexes,]
###Examine the proportions of the survived class lable across the datasets
prop.table(table(train$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))
########################################################################
############BUILD A MODEL##############################################
#Set up caret to perform 10-fold cross validation repeated 3
#times and use a grid search for optimal model hyperparameter values
train.control<-trainControl(method ="repeatedcv",number=10,repeats=3,search="grid")
###Levarage a grid search of hyperparameters for xgboost

tune.grid<-expand.grid(eta=c(0.05,0.075,0.1),
                       nrounds=c(50,75,100),
                       max_depth=6:8,
                       min_child_weight=c(2.0,2.25,2.5),
                       colsample_bytree=c(0.3,0.4,0.5),
                       gamma=0,
                       subsample=1)
View(tune.grid)

#use the doSNOW package to enable caret to train in parallel
#NOTE:tune this number based on the number of core/threads
#available on your machine
library(doSNOW)

cl<-makeCluster(2,type="SOCK")
#Register cluster so that caret will know to train in parallel

registerDoSNOW(cl)

#train the xgboost model using 10-fold cross validation repeated 3
#times and use a grid search for optimal model hyperparameter values
caret.cv<-train(Survived ~.,
                data =titanic.train,
                method ="xgbTree",
                tuneGrid =tune.grid,
                trControl =train.control,
                na.action=na.exclude)
stopCluster(cl)

#Examine caret's processing results
caret.cv
#Make predictions on the test set using xgboost model
#trained on all 625 rows of the training set using the 
#found optimal hyper parameter values
preds<-predict(caret.cv,titanic.test)
#use caret's confusionMatrix()funtion to estimate the
#effectiveness of this model on unseen,new data

confusionMatrix(preds, titanic.test$Survived)



























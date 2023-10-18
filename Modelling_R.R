# # # # # # # # # # # # # # # # # # # # #
#MODELLING IN R
#25092023
#CLIFFORD ODUOR,PhD 
# # # # ###################################
#SUPERVISED LEARNING###

#CLASSICAL STATISTICAL METHODS
#1.LINEAR REGRESSION
rm(list=ls())
library(datasets)
library(ggplot2)
library(magrittr)


cars %>% ggplot(aes(x=speed,y=dist)) +geom_point()+ geom_smooth(method="lm")
cars %>% lm(dist~speed,data=.)%>% coefficients
cars %>% lm(dist~speed,data=.)%>% confint


#VALIDATING MODELS
line <-cars %>% lm(dist~speed,data=.)
poly <-cars %>% lm(dist~speed + I(speed^2),data=.)
predict(line,cars,interval='confidence') %>% head
predict(poly,cars) %>% head
#Evaluating regression models using Mean squared error
rmse<-function(x,t) sqrt(mean(sum((t-x)^2)))
rmse(predict(line,cars),cars$dist)
rmse(predict(poly,cars),cars$dist)

##SPLIT THE DATA INTO TWO SETS(TRAINING AND TESTING DATASET)

training_data <- cars[1:25,]
test_data <- cars[26:50,]
line <- training_data %>% lm(dist ~ speed, data = .)
poly <- training_data %>% lm(dist ~ speed + I(speed^2), data = .)
rmse(predict(line, test_data), test_data$dist)
## [1] 88.89189
rmse(predict(poly, test_data), test_data$dist)
## [1] 83.84263
##The second-degree polynomial is still better
#when you split your data into training and test data, sample data points randomly.


#2.LOGISTIC REGRESSION(Classification, Really)

#install.packages("caTools")
#install.packages("ROCR")

library(mlbench)
library(dplyr)
library(caTools)
library(ROCR)

data("BreastCancer")
str(BreastCancer)

BreastCancer %>% 
  ggplot(aes(x = Cl.thickness, y = Class))+
  geom_jitter(height = 0.05,width=0.2,alpha=0.4)


BreastCancer$Cl.thickness.numeric=as.numeric(BreastCancer$Cl.thickness)
BreastCancer$Cell.size.numeric=as.numeric(BreastCancer$Cell.size)
BreastCancer$IsMalignant=recode(BreastCancer$Class,benign=0,malignant=1)

plot(BreastCancer$Cl.thickness.numeric,BreastCancer$IsMalignant,xlab="Thickness",ylab="Probability of cancer")

#Splitting the Data
split<-sample.split(BreastCancer,SplitRatio = 0.8)
split
train_bre<-subset(BreastCancer,split=="TRUE")
test_bre<-subset(BreastCancer,split=="FALSE")
#Training model
fitted_model<-glm(IsMalignant~Cl.thickness.numeric + Cell.size.numeric,data=train_bre,family="binomial")
summary(fitted_model)

#Predict test data based on model
predict_reg<-predict(fitted_model,test_bre,type="response")
#Changing probabilities
predict_reg<-ifelse(predict_reg>0.5,1,0)
#Evaluating model accuracy
#using confusion matrix
table(test_bre$IsMalignant,predict_reg)
missing_classerr<-mean(predict_reg!=test_bre$IsMalignant)
print(paste('Accuracy=',1-missing_classerr))

#ROC-AUC Curve
ROCPred<-prediction(predict_reg,test_bre$IsMalignant)
ROCPer<-performance(ROCPred,measure = "tpr",x.measure="fpr")
auc<-performance(ROCPred,measure="auc")
auc <- auc@y.values[[1]]
auc
#Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

######################################################
#######################################################
#MACHINE LEARNING ALGORITHMS
###################################################
#####################################################
#1)DECISION TREE
#Decision trees work by building a tree from the input data, splitting on a parameter in each inner node
#according to a variable value. This can be splitting on whether a numerical value is above or below a certain
#threshold or which level a factor has.
library(rpart)
model <- cars %>% rpart(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)

model <- BreastCancer %>%
  rpart(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head
predicted_class <- predict(model, BreastCancer) %>%
  as.data.frame %$%
  ifelse(benign > 0.5, "benign", "malignant")
table(BreastCancer$Class, predicted_class)

#Another implementation of decision trees is the ctree() function from the party package:
#install.packages("party")
#install.packages("zoo")
#install.packages("libcoin")
#install.packages("TH.data")
library(dplyr)
library(party)

model <- cars %>% ctree(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)
## [1] 117.1626
model <- BreastCancer %>%
  ctree(Class ~ Cl.thickness, data = .)
predict(model, BreastCancer) %>% head
table(BreastCancer$Class, predict(model, BreastCancer))
## benign malignant
## benign 453 5
## malignant 94 147
cars %>% ctree(dist ~ speed, data = .) %>% plot


#2) RANDOM FORESTS
#Random forests generalize decision trees by building several of them and combining them.
#install.packages("randomForest")
library(ggplot2)
library(dplyr)
library(randomForest)
model <- cars %>% randomForest(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)
## [1] 83.5496
#For classification, the predictions are the actual classes as a factor, so no translation is needed to get a
#confusion matrix:
model <- BreastCancer %>%
  randomForest(Class ~ Cl.thickness, data = .)
predict(model, BreastCancer) %>% head
table(BreastCancer$Class, predict(model, BreastCancer))

#3)NEURAL NETWORKS
#You can use it for both classification and regression. 
#install.packages("nnet")
library(nnet)
model <- cars %>% nnet(dist ~ speed, data = ., size = 5)
rmse(predict(model, cars), cars$dist)
#For classification
model <- BreastCancer %>%
  nnet(Class ~ Cl.thickness, data = ., size = 5)
predict(model, BreastCancer) %>% head
#We need to translate it into classes and, for this, we can use a lambda expression:
predicted_class <- predict(model, BreastCancer) %>%
  { ifelse(. < 0.5, "benign", "malignant") }
table(BreastCancer$Class, predicted_class)


#4)SUPPORT VECTOR MACHINE
#install.packages("kernlab")
library(ggplot2)
library(kernlab)
model <- cars %>% ksvm(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)

#For classification
  model <- BreastCancer %>%
  ksvm(Class ~ Cl.thickness, data = .)
predict(model, BreastCancer) %>% head
table(BreastCancer$Class, predict(model, BreastCancer))


#5)NAIVE BAYES
#Naive Bayes essentially assumes that each explanatory variable is independent of the others and uses the
#distribution of these for each category of data to construct the distribution of the response variable given the
#explanatory variables.
#The package doesn't support regression analysis-after all, it needs to look at conditional distributions
#for each output variable value-but we can use it for classification.

#install.packages("e1071")
library(e1071)
model <- BreastCancer %>%
  naiveBayes(Class ~ Cl.thickness, data = .)
predict(model, BreastCancer) %>% head
table(BreastCancer$Class, predict(model, BreastCancer))

















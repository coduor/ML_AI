# # # # # # # # # # # # # # # # # # # # #
#STUDY:DATA SCIENCE
#20210301
#AUTHOR:CLIFFORD ODUOR
# # # # # # # # # # # # # # # # # # # #
version
rm(list=ls())

setwd("C:/DATA SCIENCE/R/data")
getwd()
#install.packages("epitools")
#install.packages("xlsReadWrite")
#install.packages("epidisplay")
#install.packages("plyr")
#install.packages("reshape")
#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("xlsx")
search()

library(plyr)
library(reshape)
library(tidyverse)
library(reshape)
library(foreign)
library(haven)
library(xlsx)

ind_data<-read.csv("C:/Kibera_studies/serosurvey/data/Errors/PBIDS_sero_survey_data.csv",header = TRUE,sep=",")
hh_data<-read.csv("C:/Kibera_studies/serosurvey/data/Errors/HH_survey_data.csv",header = TRUE,sep=",")


names(ind_data)
names(hh_data)

dim(ind_data)
dim(hh_data)


#WRITING TO CSV
#data<-read.csv(file.choose(),header=TRUE)
write.csv(ind_data,"ind_data1.csv")
write.csv(hh_data,"hh_data1.csv")
#READ YOUR DATA

ind_data1<-read.csv("ind_data1.csv")
hh_data1<-read.csv("hh_data1.csv")

#UON R CLASS PRACTICALS#
ideal1<-read.csv("ideal1.csv")
ideal2<-read.csv("ideal2.csv")


#Merging of datasets

ideal3<-merge(ideal1,ideal2,by="CalfID")
head(ideal3)

#DELETE COLUMN
ideal1<-ideal1[,-1]
ideal2<-ideal2[,-1]
head(ideal1)
head(ideal2)

dim(ideal1)
dim(ideal2)

#DATA MANIPULATION 
#Dereferencing
a1<-seq(from=11,to=53,by=3)
a1
#SELECT 3RD AND 6TH ELEMENT
a1[c(3,6)]
#MATRIX
A2<-matrix(a1,ncol=3,byrow = TRUE)
A2
#selecting the 2nd column of the matrix
A2[,2]
#selecting the 3rd row of the matrix
A2[3,]
#convert matrix to dataframe
a4<-data.frame(A2)
names(a4)

# dimensions number of rows and columns

dim(ideal1)
dim(ideal2)
# description of the variables
typeof(ideal1$CalfSex)
#change calf sex from integer to factor
is.factor(ideal1$CalfSex)
ideal1$CalfSex<-as.factor(ideal1$CalfSex)
is.factor(ideal1$CalfSex)
levels(ideal1$CalfSex)
#recode variable CalfSex
ideal1$CalfSex1<-ideal1$CalfSex
table(ideal1$CalfSex1)
ideal1$CalfSex1<-ifelse(ideal1$CalfSex==1,"Male","Female")
table(ideal1$CalfSex1)
# check consistency of the two columns with CalfSex data
table(ideal1$CalfSex,ideal1$CalfSex1)
#Merging data
ideal3<-merge(ideal1,ideal2,by="CalfID")
dim(ideal3)
#Subsetting data
# If we wanted data from only one sublocation - Kidera
ideal1kidera<-subset(ideal3,ideal3$sublocation=="Kidera")
idealkideraF<-subset(ideal3,ideal3$sublocation=="Kidera" & ideal3$CalfSex1=="Female")

#EXERCISE 2
ideal1<-read.csv("ideal1.csv")
ideal2<-read.csv("ideal2.csv")
names(ideal1)
ideal3<-merge(ideal1,ideal2,by="CalfID")
dim(ideal3)
ideal3$WeightMin<-min("Weight")
ideal3$WeightMax<-max("Weight")
ideal3$VisitWeek<-substring(ideal3$VisitID,4,5)
table(ideal3$VisitWeek)
ideal3$VisitWeek<-as.numeric(ideal3$VisitWeek)
ideal3$VisitTime<-ideal3$VisitWeek*7

#handling dates
View(ideal3)
ideal3$CADOB1<-as.Date(ideal3$CADOB, format=("%d/%m/%Y"))
ideal3$VisitDate1<-as.Date(ideal3$VisitDate, format=("%d/%m/%Y")) 
ideal3$age<-as.numeric(ideal3$VisitDate1-ideal3$CADOB1)
ideal3$timespent<-max(ideal3$age, na.rm=T)
ideal3$ReasonsLoss1<-ifelse(is.na(ideal3$ReasonsLoss),"survived","lost")
table(ideal3$ReasonsLoss1)
#SUBSET
View(ideal3)

#reshape and plyr packages
search()

exercise2<- function(df){
  
  df<- within(df,
              {
                WeightMin<- min(Weight, na.rm=T)
                WeightMax <-max(Weight, na.rm=T)
                VisitWeek<- as.numeric(substring(VisitID,4,5))
                VisitTime<- VisitWeek*7
               CADOB1<-as.Date(CADOB, format=("%d/%m/%Y"))
                VisitDate1<-as.Date(VisitDate, format=("%d/%m/%Y")) 
                Age<- as.numeric(VisitDate1 - CADOB1)
                timespent<- max(Age, na.rm=T)
                ADWG<-round(((WeightMax-WeightMin)/timespent),2)
                ReasonsLoss<- ifelse(is.na(ReasonsLoss),"survived","died")
              })
}
ADWG<-round(((WeightMax-WeightMin)/timespent),2)

exercise2
?ddply   
?tapply
?within
require(ddply)
ideal3a <- ddply(ideal3,c("CalfID"),exercise2)
head(ideal3a)
ideal4 <- subset(ideal3a, ideal3a$ADWG>mean(ideal3a$ADWG) & ideal3a$ADWG< 0.15)

ideal5 <- ideal4[,c("CalfID","VisitID","Theileria.spp.",
                    "ELISA_mutans","ELISA_parva","Q.Strongyle.eggs")]
#MELTING DATASET#
?melt
names(ideal5)
ideal6<-melt(ideal5,id.vars = c(1,2),variable_name ="Infections" )
ideal7<-cast(ideal6,CalfID+VisitID~Infections,value = "value")
View(ideal7)

#Data wrangling with dplyr
library(tidyverse)
require(dplyr)
library(dplyr)

outbreak<-read.csv("outbreak.csv")
names(outbreak)
outbreak$SEX1<-factor(outbreak$SEX)
levels(outbreak$SEX1)
levels(outbreak$SEX1)[levels(outbreak$SEX1)=="1"] <- "Male"
levels(outbreak$SEX1)[levels(outbreak$SEX1)=="2"] <- "Female"
range(outbreak$AGE)
dim(outbreak)

# identify the index of the row with the oldest person
oldest<-which(outbreak$AGE==86)
# create a copy of the outbreak database
new.outbreakdata <- outbreak 
# change age of oldest person to 120
new.outbreakdata$AGE[oldest] <- 120 
#Alternative way to recode a variable - supposing we wanted to code variable "nausea" into a "Yes" for 1, "No" for 2
outbreak$nausea1 <- outbreak$NAUSEA
outbreak$nausea1<-factor(outbreak$nausea1,levels = c(1,2),labels=c("Yes","No"))
table(outbreak$nausea1)
datetoday<-format(Sys.Date())
# dataset with only columns from IDNUM to VOMITING
head(select(outbreak,IDNUM:VOMITING),5)
# dataset without columns from IDNUM to VOMITING
head(select(outbreak, -(IDNUM:VOMITING)), 3)

out60<-filter(outbreak,AGE>60)

outbreak <- mutate(outbreak, ill = ifelse(is.na(ONSETDAT01),0,1))
outbreak[1:6,]
head(arrange(outbreak, ONSETTIM01),3)
#Elementary statistics in R
mean(ideal1$RecruitWeight)

## hypothetical recommended weight of 22kg
t.test(ideal1$RecruitWeight, mu=22)

## Visualise the distribution of recruitment weights
hist(ideal1$RecruitWeight,xlab="Recruitment weight",col="blue",main="")
qqnorm(ideal1$RecruitWeight)
qqline(ideal1$RecruitWeight,lty=2)
shapiro.test(ideal1$RecruitWeight)
## summary of the data
summary(ideal1$RecruitWeight)

boxplot(ideal1$RecruitWeight,col="green")
boxplot(RecruitWeight~as.factor(CalfSex),data=ideal1)

t.test(RecruitWeight~as.factor(CalfSex),data=ideal1)

#F test to compare two variances
var.test(RecruitWeight~as.factor(CalfSex), data=ideal1)

# assuming an equality of variances
t.test(RecruitWeight~as.factor(CalfSex), var.equal=TRUE,data=ideal1)
# compare the means of weights of calves at recruitment and at end of study

# compare the means of weights of calves at recruitment and at end of study
ideal2a <- ideal2
ideal2a$WeightMin <- tapply(ideal2a$Weight,ideal2a$CalfID,min,na.rm=T)[ideal2a$CalfID]
ideal2a$WeightMax <- tapply(ideal2a$Weight,ideal2a$CalfID,max,na.rm=T)[ideal2a$CalfID]
head(ideal2a,2)

ideal2b <- unique(ideal2a[,c(2,10:12)])
t.test(ideal2b$WeightMin,ideal2b$WeightMax, paired = TRUE)

boxplot(ideal2b$WeightMin,ideal2b$WeightMax, col="green")

#Comparing two proportions - binomial test
# assuming an equality of variances
prop.test(c(19,112),c(190,2240))

## Calf sex
ideal1a <- ideal1
## recode variables
ideal1a$CalfSex <- ifelse(ideal1a$CalfSex=="1","male","female")
ideal1a$ReasonsLoss <- ifelse(is.na(ideal1a$ReasonsLoss),"survived","died")
## tabulate mortality by sex
table(ideal1a$CalfSex,ideal1a$ReasonsLoss)

## conduct a chisq test
chisq.test(ideal1a$CalfSex,ideal1a$ReasonsLoss)


## conduct a Fisher's exact test
fisher.test(ideal1a$CalfSex,ideal1a$ReasonsLoss)
#One-way analysis of Variance (ANOVA)

## ANOVA
anova(lm(ideal1$RecruitWeight~as.factor(ideal1$Education)))

#Statistical modelling in R
#Linear regression examining relationship between Average Daily Weight Gain (ADWG)
#and weight at birth (RecruitWeight)
head(ideal3a,2)
model<-lm(ADWG~RecruitWeight,data=ideal3a)
summary(model)

model2 <- lm(ADWG ~ RecruitWeight,
             data=subset(ideal3a,ideal3a$VisitWeek==51))
summary(model2)
#Multiple linear regressions
#Logistic regressions
logmod1 <- glm(factor(Theileria.spp.) ~ ManualPCV, data=ideal3a, family="binomial")
summary(logmod1)


#####Casting
ideal6a<-cast(ideal6,VisitID+CalfID ~infection,value="value")
head(ideal6a)

favourite_team <-tribble(
  
  ~name, ~footballteam_a,~footballteam_b,
  
  "laurette Mhalanga",10,6,
  
  "joseph Sempa",5,5,
  
  "Elisha Are",9,3,
  
)

data.frame(favourite_team)

?gather

ft <-favourite_team %>%
  gather(footballteam,trophies,footballteam_a:footballteam_b)

view(ft)
ft

?pipe

?gsub
ft <-favourite_team %>%
  gather(footballteam,trophies,footballteam_a:footballteam_b)%>%
  mutate(footballteam=gsub("footballteam_","",footballteam)) %>%
  arrange(name,footballteam)%>%
  dplyr::group_by(name)%>%
  dplyr::mutate(meanof=mean(trophies,na.rm = FALSE))
ft

##GGPLOT###
ideal<-read.csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv")
write.csv(ideal3a,"ideal3a")
ideal<-read.csv("ideal3a")

View(ideal)
names(ideal)
attach(ideal)
table(ReasonsLoss1)
library(ggplot2)
ggplot(data=ideal,aes(x=ReasonsLoss1))+geom_bar()
prop.table(table(ReasonsLoss1))

ggplot(ideal,aes(x=ReasonsLoss1))+geom_bar()+theme_bw()+
  labs(x="",y="Number of calves",title ="Survival of calves")
names(ideal)
table(CalfSex)

ideal<-ideal%>%
mutate(CalfSex1=ifelse(CalfSex==1,"Male","Female"))
  ggplot(ideal,aes(x=CalfSex1,fill=ReasonsLoss1))+geom_bar()

ggplot(ideal,aes(x=sublocation,fill=ReasonsLoss1))+theme_bw()+geom_bar()+coord_flip()+ facet_wrap(~CalfSex1)+
labs(y="Number of calves",title ="Calve survival")

#continuos data
names(ideal)
table(ManualPCV)
ggplot(ideal,aes(x=ManualPCV,fill=ReasonsLoss1))+theme_bw()+geom_histogram()+
  labs(x="",y="Number of calves",title ="Survival of calves")

ggplot(ideal,aes(ReasonsLoss1,ManualPCV))+theme_bw()+geom_boxplot()+ facet_wrap(~CalfSex1)+
  labs(x="",y="ManualPCV",title ="Survival of calves")


ggplot(ideal,aes(x=ReasonsLoss1,y=ManualPCV))+theme_bw()+geom_density(alpha=0.5)+ facet_wrap(Education~ReasonsLoss1)+
  labs(x="",y="ManualPCV",title ="Survival of calves")

ggsave(niceplot1)


q()


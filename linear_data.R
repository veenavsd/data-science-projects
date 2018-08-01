#linear regression

#IMPORT DATA
LGD<-read.csv("D:/rdocs/machine learing algorithms/linear regression/data.csv.csv")
#EXPLORATORY DATA ANALYSIS
#REMOVING UNWANTED VARIABLES
LGD<-LGD[,-1] 
#checking  for missing values
summary(LGD)
#checking for outliers
boxplot(LGD[,-c(4,5)])

#splitting data test to training and test data
library(caTools) #for split
split=sample.split(LGD$Losses.in.Thousands,0.75)
Train=subset(LGD,split==T)
Test=subset(LGD,split==F)

#Building model using training set data
Model<-lm(Losses.in.Thousands~.-Number.of.Vehicles,data = Train)
summary(Model)

#Validation
predloss<-predict(Model,newdata=Test)
library(hydroGOF)#for RMSE fn
RMSETEST<-rmse(Test$Losses.in.Thousands,predloss)
RMSETEST    #RMSE=216.8096

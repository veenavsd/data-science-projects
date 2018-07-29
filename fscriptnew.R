
#reading dataset
empdata<-read.csv("C:/Users/Admin/Desktop/imarticus/finalproject/Attrition.csv", na.strings = c(""," ","NA") ) 
View(empdata)
#removing unwanted variables
empdata<-empdata[,-c(9,10,22,27)]
summary(empdata)
sapply(empdata,class)
#converting categorical variables to factors
empdata$Education <-factor(empdata$Education)
empdata$EnvironmentSatisfaction <-factor(empdata$EnvironmentSatisfaction)
empdata$JobInvolvement<-factor(empdata$JobInvolvement)
empdata$JobSatisfaction<-factor(empdata$JobSatisfaction)
empdata$PerformanceRating<-factor(empdata$PerformanceRating)
empdata$RelationshipSatisfaction<-factor(empdata$RelationshipSatisfaction)
empdata$WorkLifeBalance<-factor(empdata$WorkLifeBalance)
empdata$JobLevel<-factor(empdata$JobLevel)
empdata$NumCompaniesWorked<-factor(empdata$NumCompaniesWorked)
summary(empdata)
str(empdata)
# correlation between numeric variable
library(corrplot)
numeric.var <- sapply(empdata, is.numeric)
corr.matrix <- cor(empdata[,numeric.var])
corrplot(corr.matrix, main = '\n\n Correlation plot for Numeric Variables',
         method = "number")
boxplot(empdata[,c(1,4,6,11,17,18)])
boxplot(empdata[,c(21,24,25,26)])
boxplot(empdata[,c(28,29,30,31)])

#splitting the data set into train and test set

library(caTools) 
set.seed(123)
split=sample.split(empdata$Attrition,0.75)
Train=subset(empdata,split==T)
Test=subset(empdata,split==F)

#accuracy function
accuracy<-function(cm)
{
  totp<-cm[2,1]+cm[2,2]
  TP<-cm[2,2]
  acc=TP/totp
  return(acc)
}

#modelling

#1.logistic regression
model1 <-glm(Attrition~.,family = binomial(link = 'logit'), data =Train)
summary(model1)#aic=668.49
#vALIDATE
pred=predict(model1,newdata =Test,type='response')
pred
pred<-ifelse(pred>=0.5,1,0)
cm<-table(pred,Test$Attrition)    #frequency is obtained by table
cm
accuracy(cm) #accuracy=0.6590909
####################################################################################
#2.naive bayes
library(e1071)
model2<-naiveBayes(Attrition~.,data = Train)
predicted<-predict(model2,Test)

cm<-table(Test$Attrition,predicted)
cm
accuracy(cm)  # accuracy(cm)= 0.559322
####################################################################################
#3.decision tree
library(rpart)
model3=rpart(Attrition~.,data =Train)
#validating
predmodel<-predict(model3,newdata=Test,type = 'class')
cm<-table(predmodel,Test$Attrition)
accuracy(cm) #accuracy(cm)=0.5185185
dtreepr <- prune(model3, cp = 0.01666667)
predspr <- predict(dtreepr, Test, type = "class")
cm<-table(predspr,Test$Attrition)
accuracy(cm) #accuracy(cm)=0.5
####################################################################################
#4.random forest
library(randomForest)
set.seed(222)
model4=randomForest(Attrition~.,Train)
print(model4)
pred4=predict(model4,Test)
pred4
cm<-table(pred4,Test$Attrition)
accuracy(cm)  #accuracy(cm)=0.8461538
###################################################################################
#5.svm
library(e1071)
model5<-svm(Attrition~.,data=Train,type='C-classification',kernel='linear')
summary(model5)
#vALIDATE
pred=predict(model5,newdata =Test,type='class')
pred
cm<-table(pred,Test$Attrition)    #frequency is obtained by table
cm
accuracy(cm)  # accuracy(cm)(lin)= 0.7179487
###################################################################################
#random forest is champion among all models
























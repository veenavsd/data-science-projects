# LOGISTIC REGRESSION
#here 3 datasets are used for model building,validating and predicting
#all modifications done to model bulding dataset should be performed to other 2 datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#import dataset
dataset<-read.csv("D:/rdocs/machine learing algorithms/logistic regression/Credit_Risk_Train_data.csv", na.strings = c(""," ","NA") ) 
View(dataset)
dim(dataset)
#exploratory data analysis
#removing unwanted variables
dataset<-dataset[ ,-1]
View(dataset)
#checking  for NAs
summary(dataset)
#dealing with missing values
#1)impute missing value, or 
#2)delete the missing value
#we use imputing
sapply(dataset,class)
#mode function for replacing missing values
mode <-function(x)
{
  m <-x[max(table(x))]
  m
}
dataset$Gender[is.na(dataset$Gender)]<-mode(dataset$Gender)
dataset$Married[is.na(dataset$Married)]<-mode(dataset$Married)
dataset$Dependents[is.na(dataset$Dependents)]<-mode(dataset$Dependents)
dataset$Self_Employed[is.na(dataset$Self_Employed)]<-mode(dataset$Self_Employed)
dataset$LoanAmount[is.na(dataset$LoanAmount)]<-mean(dataset$LoanAmount,na.rm = T)
dataset$Loan_Amount_Term[is.na(dataset$Loan_Amount_Term)]<-median(dataset$Loan_Amount_Term,na.rm = T)
dataset$Credit_History <-factor(dataset$Credit_History)
dataset$Credit_History[is.na(dataset$Credit_History)]<-mode(dataset$Credit_History)
#checking again for missing value
summary(dataset)
#checking for outliers
boxplot(dataset[,-c(1,2,3,4,5,10,11,12)])

#model building
#check for aic value,it should be less
#model <-glm(Loan_Status~.,family = binomial(link = 'logit'), data = dataset)
#summary(model)
model1 <-glm(Loan_Status~Credit_History+Property_Area+Married, family =binomial(link ='logit'),data =dataset) 
summary(model1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#validation phase
#import dataset
vdataset<-read.csv("D:/rdocs/machine learing algorithms/logistic regression/Credit_Risk_Validate_data.csv", na.strings = c(""," ","NA") ) 
dim(vdataset)
#exploratory data analysis
#removing unwanted variables
vdataset<-vdataset[ ,-1]
#checking  for NAs
summary(vdataset)
#dealing with missing values
#1)impute missing value, or 
#2)delete the missing value
#we use imputing
sapply(vdataset,class)
vdataset$Gender[is.na(vdataset$Gender)]<-mode(vdataset$Gender)
vdataset$Dependents[is.na(vdataset$Dependents)]<-mode(vdataset$Dependents)
vdataset$Self_Employed[is.na(vdataset$Self_Employed)]<-mode(vdataset$Self_Employed)
vdataset$LoanAmount[is.na(vdataset$LoanAmount)]<-mean(vdataset$LoanAmount,na.rm = T)
vdataset$Loan_Amount_Term[is.na(vdataset$Loan_Amount_Term)]<-median(vdataset$Loan_Amount_Term,na.rm = T)
vdataset$Credit_History <-factor(vdataset$Credit_History)
vdataset$Credit_History[is.na(vdataset$Credit_History)]<-mode(vdataset$Credit_History)
#checking again for missing value
summary(vdataset)
#validating build model
pred=predict(model1,newdata = vdataset,type='response')
pred
pred<-ifelse(pred>=0.5,1,0)
#confusion matrix
cm<-table(pred,vdataset$Loan_Status)    #frequency is obtained by table
cm
accuracy<-function(cm)
{
  totp<-cm[2,1]+cm[2,2]
  TP<-cm[2,2]
  acc=TP/totp
  return(acc)
}
accuracy(cm) #accuracy(cm)= 0.9390681   
#Roc curve can also be used
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#predicting loan_status for new data set
pdataset<-read.csv("D:/rdocs/machine learing algorithms/logistic regression/Credit_Risk_Test_data.csv",na.strings = c(""," ","NA"))
summary(pdataset)
dim(pdataset)
#exploratory data analysis
#removing unwanted variables
pdataset<-pdataset[ ,-1]
#checking  for NAs
summary(pdataset)
#dealing with missing values
#1)impute missing value, or 
#2)delete the missing value
#we use imputing
sapply(pdataset,class)
pdataset$Gender[is.na(pdataset$Gender)]<-mode(pdataset$Gender)
pdataset$Dependents[is.na(pdataset$Dependents)]<-mode(pdataset$Dependents)
pdataset$Self_Employed[is.na(pdataset$Self_Employed)]<-mode(pdataset$Self_Employed)
pdataset$LoanAmount[is.na(pdataset$LoanAmount)]<-mean(pdataset$LoanAmount,na.rm = T)
pdataset$Loan_Amount_Term[is.na(pdataset$Loan_Amount_Term)]<-median(pdataset$Loan_Amount_Term,na.rm = T)
pdataset$Credit_History <-factor(pdataset$Credit_History)
pdataset$Credit_History[is.na(pdataset$Credit_History)]<-mode(pdataset$Credit_History)
#checking again for missing value
summary(pdataset)
#predicting 
pred1<-predict(model1,newdata = pdataset,type = 'response')
pred1
pred1<-ifelse(pred1>0.5,1,0)
finalpdataset<-cbind(pdataset,loan_status=pred1)
View(finalpdataset)

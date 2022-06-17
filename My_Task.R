library(class)
library(MASS)
library("e1071")
library(GGally)
library(kernlab)
library(mlbench)
library(reshape2)
library(ROCR)
library(ggplot2)
library(lattice)
library(caret)
library(ISLR)



# 1. Model1: KNN, -k-nearest neighbours
data<-read.csv("Prostate_Cancer.csv") # load the data
str(data)
head(data)
new_data<-data[-1] # remove the Id column
View(new_data)
table(new_data$diagnosis_result)
samp<-sample(1:nrow(new_data),0.5*nrow(new_data)) # taking the sample of 50% of all data set
Norm<-function(x){(x-min(x))/(max(x)-min(x))} # create the normalization fuction
data_norm<-as.data.frame(lapply(new_data[,c(2,3,4,5,6,7,8)],Norm))
summary(data_norm)
trained_data<-data_norm[samp,] # training sample
test_data<-data_norm[-samp,] # test sample
test_categ<-new_data[samp,1]
data_test_category <- new_data[-samp,1]
pr <- knn(trained_data,test_data,cl=test_categ,k=15)

##create confusion matrix
tab <- table(pr,data_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
tab


# 2. Model2: SVM, Support Vector Machine
new_data$diagnosis_result<-ifelse(new_data$diagnosis_result=="M",1,0)# change response to numeric
View(new_data)

x<-subset(new_data,select = -diagnosis_result)
y<-new_data$diagnosis_result

svm_model <- svm(diagnosis_result ~ ., data=new_data)
summary(svm_model)
svm_m<-svm(x,y)
summary(svm_m)


pred <- predict(svm_m,x)
system.time(pred <- predict(svm_m,x))
table(pred,y)










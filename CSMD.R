rm(list = ls())
setwd("C:/Piazza/internship/CSMD")
train<-read.csv("training.csv",heade=FALSE)
summary(train)####10546 obs 29 variables
test<-read.csv("testing.csv",header = FALSE)
summary(test) ##### 301 obs,29 variables
str(train)

sum(is.na(train))
sum(is.na(test))
plot(train$V1)

x<-c(2:29)

train[,x]<-lapply(train[,x], as.numeric)
hist(train$V29)
hist(train$V2)
plot(train$V2)

rm(list = ls(all=TRUE))
setwd("C:/Piazza/internship/CSMD")
train<-read.csv("training.csv",header = TRUE)
test<-read.csv("testing.csv",header = TRUE)
 
##### now analizing the data
#### combining the train and test data into one data set 

CSMD<-rbind(train,test)
summary(CSMD)
str(CSMD)


#### preprocessing the data 
library(caret)
target_variable<-CSMD$class
subset<-CSMD[,-1]
str(subset)
preProcess<-preProcess(CSMD[,-1],method=c("center","scale"))
preProcess
newdata<-predict(preProcess,CSMD)
str(newdata)
####outliers
install.packages("outliers")
library(outliers)
chisq.out.test(CSMD, variance=var(CSMD), opposite = FALSE)

#### ploting the graphs
### plots on class label  

plot(CSMD$class,CSMD$max_ndvi)
file<-jpeg("mygraph.jpg")
plot(CSMD$X20150720_N,CSMD$X20150602_N)
plot(CSMD$class,CSMD$X20140101_N)

### histograms
hist(CSMD$max_ndvi)
hist(CSMD$X20150720_N)
hist(CSMD$X20150602_N)
hist(CSMD$max_ndvi,breaks = 10,col ="red")
hist(CSMD$X20150720_N,breaks = 100,col= "blue")
### adding the normal curve to the histogram
x<-CSMD$max_ndvi
h<-hist(x,breaks=10,col="red", xlab = "MAX_NDVI",main = " hist of MAX_NDVI")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean = mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids)*length(x)
lines(xfit, yfit, col="blue", lwd=2)
##### kernal density plots 
d<-density(CSMD$max_ndvi)
plot(d)
plot(d,main="density of MAX_NDVI")
polygon(d, col="red",border = "blue")

#### comparing the different class to variables
install.packages("sm")
library(sm)
levels(CSMD$class)
mode(CSMD$class)
summary(CSMD)

#### dot plot 
dotchart(CSMD$max_ndvi,labels = row.names(CSMD$max_ndvi),cex=0.5,
         main = "dot chart plot on CSMD",
         xlab="MAX_NDVI")
##### bar plots

counts<-table(CSMD$max_ndvi)
barplot(counts,main="Barplot",xlab = "number of max_ndvi")

#### pie chart
slice<-c(1494,7509,482,1009,100,251)
labels<-c("farm","forest","grass","impervious","orchard","water")
pie(slice,labels = labels,main = "pie chart")

##### pie chart with percentanges
percentage<-round(slice/sum(slice)*100)
lbls<-paste(labels,percentage)
pie(slice,labels = lbls,col = rainbow(length(lbls)),main="pie chart with percentages")

#### for 3D pie chart 
install.packages("plotrix")
library("plotrix")
pie3D(slice,labels = lbls,explode = 0.2,main="pie chart with 3D")

#### box plot
boxplot(CSMD$max_ndvi,data = CSMD)
boxplot(CSMD$X20150720_N)
install.packages("aplpack")
library("aplpack")
library(tcltk)

##### scatter plot
plot(CSMD$class,CSMD$max_ndvi,pch=20)
plot(CSMD$class,CSMD$X20150720_N)

###### now we have to split the data into train and test sets
### these train and test sets divided from the newdata

train_rows<-createDataPartition(newdata$class,p=0.7,list = FALSE)
train1<-newdata[train_rows,]
test1<-newdata[-train_rows,]

#### now building the model
library(nnet)

glm_model<-multinom(class~.,family="binomial",data = train1)
summary(glm_model)
library(MASS)
stepAIC(glm_model,direction = "both")

#### now predicting the test set on model
predict_glm<-predict(glm_model,test1)
confusionMatrix(predict_glm,test1$class)

### acc=0.885
##### PCA 
PCA<-princomp(train1[,-1],cor = TRUE)
names(PCA)
summary(PCA)
plot(PCA)
PCA$loadings
PCA$scores

##### now compress the freatures
PCA_CF<-PCA$scores[,1:22]
PCA_CF
library(nnet)
multout<-multinom(train1$class~PCA_CF)
screeplot(PCA,type = "lines")
library(ggbiplot)
g<-ggbiplot(PCA,obs.scale = 1,var.scale = 1,groups = train1$class,ellipse = TRUE,cricle=TRUE)
g

###### CART or rpart
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
rpart<-rpart(train1$class~.,data=train1)
summary(rpart)

rpart.plot(rpart)
#### predict on test data

predict_rpart<-predict(rpart,test1,,type="class")
confusionMatrix(predict_rpart,test1$class)

###### Accuracy : 0.8557

library(C50)
C50_model<-C5.0(train1$class~.,data=train1)
summary(C50_model)
plot(C50_model)

predict_c50<-predict(C50_model,test1)
confusionMatrix(predict_c50,test1$class)

######Accuracy : 0.8871

### building the random forest
library(randomForest)
model_rf<-randomForest(train1$class~.,data = train1)
summary(model_rf)
predict_rf<-predict(model_rf,test1)

confusionMatrix(predict_rf,test1$class)

####### Accuracy : 0.9388

#### building the svm model
library(MASS)
library(caret)
library(e1071)
ctrl<-trainControl(method = "repeatedcv",repeats=5,classProbs = TRUE)










rm(list = ls(all=TRUE))
setwd("C:/Piazza/internship/CSMD")
train<-read.csv("training.csv",header = TRUE)
test<-read.csv("testing.csv",header = TRUE)
summary(train)
summary(test)

str(train)    #### 10545 obs. of  29 variables
str(test)     ##### 300 obs. of  29 variables
class(train$class)

levels(train$class)
levels(test$class)

### combined the train and test into one data frame
CSMD<-rbind(train,test)
str(CSMD)    ###10845 obs. of  29 variables

######################
## by using caret library doing  preprocessing train and test
#### the package contain tools:
# data splitting
# pre-processing
# feature selection
# model tuning using resampling
# variable importance estimation

##### removing the some observation values in data set 
set.seed(1234)
#idx = sample(nrow(CSMD), .1*nrow(CSMD))
#col_no = sample(ncol(CSMD), length(idx), replace = TRUE)

#del_cSMD = split(data.frame(idx, col_no), f = col_no)

#for (ii in seq_along(delDF)) 
  #CSMD[delDF[[ii]]$idx, delDF[[ii]][1L, 'col_no']] = NA

#CSMD[idx[10L], ]
install.packages("magrittr")
library(magrittr)
library(dplyr)
### box plot with original data 
x<-boxplot(CSMD$X20150720_N)
mean_x<-mean(CSMD$X20150720_N) #### 5694.503
sd_x<-sd(CSMD$X20150720_N)    #### 2306.414
sd_x
##### for negative values i replace with NA values
CSMD1<-CSMD[,-1] %>% mutate_each(funs(replace(., .<0, NA)))
sum(is.na(CSMD1))
colSums(is.na(CSMD1))
CSMD1
y<-boxplot(CSMD1$X20150720_N)
mean_y<-mean(CSMD1$X20150720_N)  #### NA
sd_y<-sd(CSMD1$X20150720_N)      #### NA
sd_y

##### now imputting the missing values
library(DMwR)
impute<-centralImputation(CSMD1)
sum(is.na(impute))


boxplot(CSMD1$X20150720_N~CSMD1$X20150602_N,data = CSMD1,col="lightgray")
boxplot(CSMD1$X20150720_N~CSMD1$X20150602_N,data = CSMD1)
boxplot(CSMD1$X20150720_N~CSMD1$X20150602_N,data = CSMD1,sample=0.5)
boxplot(CSMD1$X20150720_N,data = CSMD1)
boxplot(CSMD1)
z<-boxplot(impute$X20150720_N)
mean_z<-mean(impute$X20150720_N) #### 5715.787
mean_z
sd_z<-sd(impute$X20150720_N)  ####2275.781
sd_z
mean=c("mean_x","mean_y","mean_z")
value=c(5694.503,NA,5715.787)
boxplot(value)

sd<-c("sd_x","sd_y","sd_z")
value_sd<-c(2306.414,NA,2275.781)
boxplot(value_sd)
str(CSMD)
sum(is.na(CSMD))
####### now i am imputting the na  values with knn imputation
knn_impute<-knnImputation(CSMD1)
x1<-boxplot(knn_impute$X20150720_N)

##### plotting  the all plot in one


####### visualizing the data



str(CSMD) ####10845 obs. of  29 variables
#### scatter plot diagrams 
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
transparentTheme(trans = 0.3)
library(caret)
featurePlot(x=test[,-1],y=test$class,plot = "pairs",auto.key = list(columns = 1))
featurePlot(x=test[,-c(1:20)],y=test$class,plot = "ellipse",auto.key=list(columns=1))
featurePlot(x=test[,-c(1:25)],y=test$class,plot="density",auto.key=list(columns=1))
featurePlot(x=test[,-1],y=test$class,plot="density",auto.key=list(columns=3))
featurePlot(x=test[,-1],y=test$class,plot="box",auto.key=list(columns=2))
featurePlot(x=test[,-1],y=test$class,plot="scatter",layout=c(3,1))


#### preprocessing  the data
head(model.matrix(class~.,data=CSMD))
dummies<-dummyVars(class~.,data=CSMD)
head(predict(dummies,newdata=CSMD))
data.frame(table(CSMD$class))
data.frame((table(CSMD$max_ndvi)))

cor<-cor(CSMD[,-1])
plot(cor)
mean(CSMD$max_ndvi==0)
mean(CSMD$X20150720_N==0)
mean(CSMD$X20150602_N==0)
mean(CSMD$X20150501_N==0)

################ now spliting of data into tain and test set
train_rows<-createDataPartition(CSMD$class,p=0.8,list = F,time=1)
head(train_rows)
train_CSMD<-CSMD[train_rows,]
test_CSMD<-CSMD[-train_rows,]

####### parameter tunning
install.packages("gbm")
library(gbm)
par_tune<-trainControl(method="repeatedcv",number = 10,repeats = 10)
fit<-train(train$class~.,data=train,method="gbm",trcontrol=par_tune,verbose=FALSE)
library(corrplot)
corrplot(train[,-1])

#########
rm(list = ls(all=TRUE))
setwd("C:/Piazza/internship/CSMD")
train<-read.csv("training.csv")
test<-read.csv("testing.csv")
CSMD<-rbind(train,test)

########
dim(CSMD)
head(CSMD)
tail(CSMD)
length(CSMD)
mean(CSMD$max_ndvi)
sd(CSMD$max_ndvi)
str(CSMD)

library(lubridate)
library(class)
library(caret)
library(plyr)
library(glmnet)

#Read data into R
train <- read.csv("/kaggle/train.csv")
View(train)

#cange the format of the dates?gprep
strptime(train$Dates, format = "%Y-%m-%d %H:%M:%S")

table(train$Resolution)
train$Weekday <- ifelse(train$DayOfWeek == 'Monday',1,0)
train$Weekday <- ifelse(train$DayOfWeek == 'Tuesday',1,0)
train$Weekday <- ifelse(train$DayOfWeek == 'Wednesday',1,0)
train$Weekday <- ifelse(train$DayOfWeek == 'Thursday',1,0)
train$Weekday <- ifelse(train$DayOfWeek == 'Friday',1,0)
train$Weekend <- ifelse(train$DayOfWeek == 'Saturday',1,0)
train$Weekend <- ifelse(train$DayOfWeek == 'Sunday',1,0)

# See if the crime is at intersection
train$Intersection<-grepl("/", train$Address)
train$Intersection <- mapvalues(train$Intersection, from = c("TRUE", "FALSE"), to =c(1,0))
#Make a Category matrix
categoryMatrix<-data.frame(with(train,model.matrix(~Category+0))) 
names(categoryMatrix)<-sort(unique(train$Category))
train<- cbind(categoryMatrix, train)
#train$Resolution<-NULL
set.seed(9850)

#
train$Year <- year(train$Dates)
train$Month <- month(train$Dates)
train$Day <- day(train$Dates) 
train$Hour <- hour(train$Dates)
train$Minute<-minute(train$Dates) 
train$Second<-second(train$Dates)
train$Night<-ifelse(train$Hour > 22 | train$Hour < 6,1,0)



trainDSindex<-createDataPartition(train$Category, p=0.75, list= F)
trainDS<-train[trainDSindex,]
testDS<-train[-trainDSindex,]
matMod.tr<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+Hour+Minute+Intersection+Night,data=trainDS)
matMod.test<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+Hour+Minute+Intersection+Night,data=testDS)
m<-glmnet(matMod.tr,trainDS[,1],family="binomial")
pred<-as.data.frame(predict(m,matMod.test,s=1e-15,type="response"))
numCat<-length(unique(trainDS$Category))
pb <- txtProgressBar(min = 1, max = numCat, style = 3)

for (i in 2:numCat) {
  m<-glmnet(matMod.tr,trainDS[,i],family="binomial")
  pred<-cbind(pred,predict(m,matMod.test,s=1e-15,type="response"))
  setTxtProgressBar(pb, i)
}





#testDS$Category<-NULL

#k=10
#m1<-knn(train = trainDS, test = testDS, cl = trainDS$Category, k)



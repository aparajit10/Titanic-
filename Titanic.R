setwd("/Users/Aparajit/Desktop/Kaggle/")
#used to set working directory 
train <- read.csv("train.csv",header = TRUE,stringsAsFactors= TRUE)
testData <- read.csv("test.csv",header = TRUE,stringsAsFactors= TRUE)
# read.csv used to read CSV files into R studio 
head(train)

plot(density(train$Age,na.rm = TRUE))
# Most people between 20 to 40 years of age
plot(density(train$Pclass,na.rm = TRUE))
# 3rd class >1st class > 2nd class
## Survival rate by sex
plot(density(train$Fare,na.rm=TRUE))
counts <- table(train$Survived,train$Sex)
head(counts)
barplot(counts,xlab = "Sex",ylab = "No of Ppl",main = "Survival Rate by Sex")
counts[2] / (counts[1] + counts[2])
counts[3]/(counts[3]+counts[4])
109/(468+109)
##Survival rate by passenger class 
PcClassSurvival <- table(train$Survived,train$Pclass)
head(PcClassSurvival)
barplot(PcClassSurvival,xlab = "Class of Cabin",ylab = "No of people",main = "Survival by Class")
PcClassSurvival[2]/(PcClassSurvival[2]+PcClassSurvival[1])
PcClassSurvival[4]/(PcClassSurvival[4]+PcClassSurvival[3])
PcClassSurvival[6]/(PcClassSurvival[6]+PcClassSurvival[5])
## Survival rate by age 
SurviavlByAge <- table(train$Survived,train$Age)
head(SurviavlByAge)
#
### Part one ends before this comment 
### Part two starts below 

#removing non relevamt variables 
head(train)
traindata <- train[-c(1,9:12)]
head(traindata)
#
#Replacing Gender variable (Male/Female) with a Dummy Variable (0/1)
traindata$Sex=gsub("female",1,traindata$Sex,ignore.case = TRUE)
traindata$Sex=gsub("^male",0,traindata$Sex,ignore.case = TRUE)
#
head(traindata)
#trying to find percentage of missing values by attribute
colMeans(is.na(traindata))
#Age has 19 % missing values 
library("mice")
md.pattern(traindata)
test <- traindata[-c(3)]
#Using mice package we know that 714 records have zero missing values 
# 177 records have age missing 
methods(mice)
tempData <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData$imp$Age
imputeddata <- complete(tempData,1)
traindata <- cbind(imputeddata,traindata$Name)
colnames(traindata)[7] <- "Name"
md.pattern(traindata)
## train data has no more missing values 
### Creating new variables child , family and mother 
#CHILD
traindata["Child"]
for (i in 1:nrow(traindata)) {
  if (traindata$Age[i] <= 18) {
    traindata$Child[i] = 1
  } else {
    traindata$Child[i] = 2
  }
}
#FAMILY
traindata["Family"] = NA
for(i in 1:nrow(traindata)) {
  x = traindata$SibSp[i]
  y = traindata$Parch[i]
  traindata$Family[i] = x + y + 1
}
#MOTHER 
traindata["Mother"] 
for(i in 1:nrow(traindata)) {
  if(traindata$Name[i] == "Mrs" & traindata$Parch[i] > 0) {
    traindata$Mother[i] = 1
  } else {
    traindata$Mother[i] = 2
  }
}
### TRAINING DATASET COMPLETE 
### TEST Data processing begins below 
head(testData)
plot(density(testData$Age,na.rm = TRUE))
PassengerId = testData[1]
testData <- testData[-c(1,8:11)]
head(testData)
testData$Sex=gsub("female",1,testData$Sex,ignore.case = TRUE)
testData$Sex=gsub("^male",0,testData$Sex,ignore.case = TRUE)
head(testData)
colMeans(is.na(testData))
#Age variable has about 20 % Missing values 
md.pattern(testData)
# 86 Records have missing values 
X <- testData[-c(2)]
head(X)
X_temp <- mice(X,m=5,maxit=50,meth='pmm',seed=500)
summary(X_temp)
X_temp$imp$Age
imputeddata_test <- complete(X_temp,1)
testData <- cbind(imputeddata_test,testData$Name)
head(testData)
colnames(testData)[6] <- "Name"
md.pattern(testData)
### Creating new variables child , family and mother for test data 
#CHILD
testData["Child"]
for (i in 1:nrow(testData)) {
  if (testData$Age[i] <= 18) {
    testData$Child[i] = 1
  } else {
    testData$Child[i] = 2
  }
}
#FAMILY
testData["Family"] = NA
for(i in 1:nrow(testData)) {
  x = testData$SibSp[i]
  y = testData$Parch[i]
  testData$Family[i] = x + y + 1
}
#MOTHER 
testData["Mother"] 
for(i in 1:nrow(testData)) {
  if(testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
    testData$Mother[i] = 1
  } else {
    testData$Mother[i] = 2
  }
}
####
head(testData)
####TEST DATA PREPARATION COMPLETE 

train.glm <- glm(Survived~Pclass+Sex+Age+Child+Szs+Family+Mother,family=binomial,data = traindata)
summary(train.glm)

#family is the link funtion . dafault here is gaussian and data output ith binomial will be for logit regression 

p.hats <- predict.glm(train.glm, newdata = testData, type = "response")

survival <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

kaggle.sub <- cbind(PassengerId,survival)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "kaggle.csv", row.names = FALSE)





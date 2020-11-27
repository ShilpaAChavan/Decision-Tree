################################# Decision Tree #####################################

#Objective:A cloth manufacturing company is interested to know about the 
#segment or attributes causes high sale. 

#Data : Company_Data.csv
#####################################################################################
install.packages("C50")
library(C50)
install.packages("caret")
library(caret)


##Step1 : Data Exploration 
CompanyData  <- read.csv(file.choose()) #concrete.csv
View(CompanyData)

hist(CompanyData$Sales)

High <- ifelse(CompanyData$Sales<10, "No", "Yes")
High
CD <- data.frame(CompanyData, High)
View(CD)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(CD$High,p=.75, list=F)
training <- CD[inTraininglocal,]
View(training)
testing <- CD[-inTraininglocal,]

#model building
model <- C5.0(training$Sales~.,data = training,trails = 1000)
summary(model)

#Prediction
pred <- predict.C5.0(model,training[,-5])
pred
pred1 <- predict.C5.0(model,testing[,-5])
pred1

#Confusion Matrix
a <- table(training$Species,pred)
b <- table(testing$Species,pred1)
sum(diag(b)/sum(b))
plot(model)





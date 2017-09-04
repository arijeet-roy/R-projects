myData = as.data.frame(unclass(Letters_Train))
View(myData)
summary(myData)
train <- sample(1:nrow(myData), 16000)

#random forest
bagging.fit <- randomForest(X1~.,myData[train,],mtry=16,importance=TRUE)
bagging.fit
bagging.pred <- predict(bagging.fit,myData[-train,])
bagging.test.rate <- mean(bagging.pred == myData[-train, 1])
bagging.test.rate
length(bagging.pred)
myDataTest = as.data.frame(unclass(Letters_Test))
bagging.pred <- predict(bagging.fit,myDataTest)
bagging.pred
length(bagging.pred)
write.table(bagging.pred, file = "MyData.csv",row.names=FALSE,col.names=FALSE, sep=",")
MyData <- read_csv("~/R-Projects/MyData.csv", col_names = FALSE)
View(MyData)
predictedData = MyData
View(myDataTest)
myDataTest$X1 = predictedData
View(myDataTest)
myDataTest = as.data.frame(unclass(myDataTest))
write.table(myDataTest, file = "Letter_Test.csv",row.names = FALSE)







#SVM
svm.fit <- svm(X1~., myData[train,])
svm.pred <- predict(svm.fit, myData[-train,])
svm.test.rate <- mean(svm.pred == myData[-train,1])
svm.test.rate
svm.pred

# First we clean the dataset
myData = CommViolPredUnnormalizedData
myData[myData == '?'] <- NA
View(myData)
myData <- myData[,-c(130:145)]
myData <- myData[,-c(131)]
myData <- myData[-which(is.na(myData$RacialMatchCommPol)),]
myData$countyCode = NULL
myData$communityCode = NULL
myData$communityname = NULL
myData$state = NULL
myData$LemasGangUnitDeploy = NULL
myData <- na.omit(myData)
sum(is.na(myData))
myData[, c(13,27,100:116,120:122,124:125)] <- sapply(myData[, c(13,27,100:116,120:122,124:125)], as.numeric)

myData = as.data.frame(unclass(myData))

set.seed(1)
x<-model.matrix(ViolentCrimesPerPop~.,myData)[,-1]
y=myData$ViolentCrimesPerPop
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
names(ridge.mod)
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
# We need to find the optimal lambda - we can use CV
plot(ridge.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
# The cv function does 10-fold CV by default
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# sqrt(mean((ridge.pred-y.test)^2))/mean(y.test)
# Once we to CV find the best lambda, we can use all of the data
# to build our model using this lambda ...
ridge.model=glmnet(x,y,alpha=0)
predict(ridge.model,type="coefficients",s=bestlam)[1:20,]
# Note all coef are included, but some a weighted heavier than
# others.
# Now we apply the Lasso - note all we do is change the alpha
# option
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
# Note that Lasso takes certain coeff to zero for large enough
# lambda.We'll do CV to find the best lambda again ...
set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod,s=bestlam,newx=x[test,])
mean(( lasso.pred -y.test)^2)
# sqrt(mean((lasso.pred-y.test)^2))/mean(y.test)
# The MSE is similar to what we saw for Ridge
# Let's find the coefficients ...
out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict(out,type ="coefficients",s=bestlam )[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

pcr.fit = pcr(ViolentCrimesPerPop~., data=myData, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
?pcr
summary(pcr.fit)

dim(myData)
train = sample(1:319,200)
train.x = scale(x[train,])
test.x = scale(x[-train,])
train.y = y[train]
test.y = y[-train]
knn.fit = knn.reg(train.x, test.x, train.y, k=5)
mean((test.y - knn.fit$pred)^2)
errs = rep(0,15)
for(i in 1:15){
knn.fit = knn.reg(train.x, test.x, train.y, k=i)
errs[i] = mean((test.y - knn.fit$pred)^2)
}
errs
min(errs)
#removing NAs
myData[, c(13)] <- sapply(myData[, c(13)], as.numeric)
# What about doing k-fold CV?
bins = sample(1:10,319, replace = TRUE)
# Note this vector will assign every row in the data set to a bin
binErrs = rep(0,10)
for(k in 1:10){
train.x = scale(x[bins != k,])
test.x = scale(x[bins == k,])
train.y = y[bins != k]
test.y = y[bins == k]
knn.fit = knn.reg(train.x, test.x, train.y, k=8)
binErrs[k] = mean((test.y - knn.fit$pred)^2)
}
mean(binErrs)
# Let's combine...
errs = rep(0,15)
for(i in 1:15){
for(k in 1:10){
train.x = scale(x[bins != k,])
test.x = scale(x[bins == k,])
train.y = y[bins != k]
test.y = y[bins == k]
knn.fit = knn.reg(train.x, test.x, train.y, k=i)
binErrs[k] = mean((test.y - knn.fit$pred)^2)
}
errs[i] = mean(binErrs)
}
errs
# Better than simple CV, but it appears that Ridge and 13NN gave the
# best answer.





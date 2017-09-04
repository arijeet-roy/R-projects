
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)

library(corrplot)


train <- read.csv(file.choose(), header = TRUE, sep = ',',stringsAsFactors = F)
test <- read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = F)

##we bind the train and test set to implement data preprossing
Data <- bind_rows(train, test)
full <- bind_rows(train, test)


View( full)

full$class=NULL
summary(full)
myDataClean = na.omit(full)
summary(myDataClean)
newData=myDataClean[c("b1","b2","b3","b4","b5","b6","b7","b8","b9")]
View(newData)


kc <- kmeans(newData, 4)
kc



kc$totss
kc$withinss
kc$tot.withinss
kc$betweenss

# How well did we do? Let's compare the results of the clustering
# to the species value (which in this case, we know)

table(Data$class, kc$cluster)


Data <- bind_rows(train, test)
full <- bind_rows(train, test)
full2=full[,c("class","b1","b2","b3","b4","b5","b6","b7","b8","b9")]
full_scale = scale(full2[,-1])
head(full_scale)

km_full = kmeans(full_scale,4)
km_full



set.seed(200)
Cluster <- kmeans(full_scale[, 2:3], 4, nstart = 20)
Cluster
table(Data[[1]], Cluster$cluster)

km_full = kmeans(full_scale,4)
km_full
table(Data[[1]], km_full$cluster)

# This is strange - seems to have done a good job of clustering,
# but why is BetweenSS/TotalSS so low?

plot(full_scale[,c(1,2)], col=km_full$cluster)
points(km_full$centers[,c(1,2)], col=1:3, pch=8, cex=2)

# Here is how to do the Elbow Method directly ...

full_scale = scale(full2[,-1])
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.

k.max <- 15
data <- full_scale
wss <- sapply(2:k.max,
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

km_full = kmeans(full_scale,5)
km_full
table(Data[[1]], km_full$cluster)



###############################################################
#Hcluster

Data <- bind_rows(train, test)
full <- bind_rows(train, test)
full2=full[,c("class","b1","b2","b3","b4","b5","b6","b7","b8","b9")]
full_scale = scale(full2[,-1])
full.complete = hclust(dist(full_scale ), method="complete")
full.average = hclust(dist(full_scale ), method="average")
full.single = hclust(dist(full_scale ), method="single")


par(mfrow=c(1,3))
plot(full.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(full.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(full.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)

cutree(full.complete,4)
cutree(full.average,5)

# Let's see how the method worked ...

table(cutree(full.complete,4), Data[,1])
table(cutree(full.average,5), Data[,1])
table(cutree(full.single,1), Data[,1])

# Answer: Not well. But interestingly, if we use fewer
# variables ...

full.average.restr = hclust(dist(full_scale[,3:4]), method="average")
table(cutree(full.average.restr,3), Data[,1])

# Note the final result does not tell us much

full.average

 # Here we use correlation between predictors for our clustering




# attach package "tidyr"

Data <- bind_rows(train, test)
full <- bind_rows(train, test)
full2=full[,c("class","b1","b2","b3","b4","b5","b6","b7","b8","b9")]
dim(full2)

myData3 = spread(full2, class, b2, fill = NA, convert = FALSE, drop = TRUE,
                 sep = NULL)
View(myData3)
dim(myData3)
myData4 = na.omit(myData3)
dim(myData4)

# We will need to be more careful on removing NAs!
# First, we need to do more cleanup - we can remove
# the date column, and and then we need to transpose
# the data to get the stocks as rows

myData4 = myData3

myData5 = t(myData4)
dim(myData5)

# Not all rows are complete - some stocks joined and others 
# left the SP500 during the time period. We correct for this

myData6 = myData5[complete.cases(myData5),]
dim(myData6)

# There are 379 stocks that were in the index for the entire
# year. Next we convert the data to a percentage daily
# change, rather than a closing price.

myData6[1,1]
myData7 = myData6
for(i in 1:244){
  myData7[,i] = (myData6[,i+1] - myData6[,i])/myData6[,i]}
dim(myData7)
myData7[1,1]

# We can now compute a distance matrix for myData7 using
# correlation ...

full_corr=as.dist(1-cor(t(myData7)))
full.Ward = hclust(full_corr, method="ward.D2")
plot(full.Ward, labels = FALSE)

# Maybe 5 groups of stocks? Let's try to find the groups

full.Groups=cutree(full.Ward,5)
full.Groups==1
x =as.vector(names(full.Groups))
x[1]
GP_1 = x[full.Groups==1]
GP_1

# We can try to remove outliers and re-cluster ...

myData8 = myData7[GP_1,]
dim(myData8)
full_corr=as.dist(1-cor(t(myData8)))
full.Ward = hclust(full_corr, method="ward.D2")
plot(full.Ward, labels = FALSE)







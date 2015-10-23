library(caret); library(dplyr); library(randomForest); library(rpart); library(rpart.plot); library(rattle); library(RColorBrewer)

set.seed(1015)
training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))

testing <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!", ""))

##Cleaning data
cleanTrain <- training[,colSums(is.na(training))==0]
cleanTest <- testing[,colSums(is.na(testing))==0]

##Remove columns 1-7
cleanTrain <- cleanTrain[, -c(1:7)]
cleanTest <- cleanTest[, -c(1:7)]

##inspect datasets
dim(cleanTrain); dim(cleanTest)
head(cleanTrain); head(cleanTest)

##Partitioning to 60/40 split
inTrain <- createDataPartition(y=cleanTrain$classe, p=0.6, list=FALSE)
train <- cleanTrain[inTrain, ]; subTrain <- cleanTrain[-inTrain, ]
dim(train); dim(subTrain)

##view data
plot(train$classe, col="blue", main="bar plot", xlab="classe levels", ylab="frequency")

mod1 <- rpart(classe ~., data=train, method="class")
pred1 <- predict(mod1, subTrain, type="class")

fancyRpartPlot(mod1)
##rpart.plot(mod1, main="classification tree", extra=102, under=TRUE, faclen=0)
confusionMatrix(pred1, subTrain$classe)
mod2 <- randomForest(classe ~., data=train, method="class")
pred2 <- predict(mod2, subTrain, type="class")
> confusionMatrix(pred2, subTrain$classe)
fpred <- predict(mod2, cleanTest, type="class")
fpred

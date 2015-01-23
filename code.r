remove(list = ls())
library(caret)
library(randomForest)
library(corrplot)
library(kernlab)
library(RCurl)
setInternet2(use = F)
# set URLs and destinations
Url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
file1 <- "./data/pml-training.csv"
file2 <- "./data/pml-testing.csv"

# download the files
download.file(Url1, destfile = file1)
download.file(Url2, destfile = file2)

# read the trainings file
data_training <- read.csv("./data/pml-training.csv", na.strings= c("NA",""," "))

# usable the data by removing columns with NAs etc
data_training_NAs <- apply(data_training, 2, function(x) sum(is.na(x)))
data_training_usable <- data_training[,which(data_training_NAs == 0)]

# remove not needed columns
data_training_usable <- data_training_usable[8:length(data_training_usable)]

# split the data to trainings and validation sets 75/25
inTrain <- createDataPartition(y = data_training_usable$classe, p = 0.75, list = FALSE)
training <- data_training_usable[inTrain, ]
validation <- data_training_usable[-inTrain, ]

# plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.7,  tl.col = rgb(0, 0, 0))

# fit a model to predict the classe using every other variable as a predictor
#Random forest method
model.rm     <- randomForest(classe ~ ., data = training)
#Linear discriminant analysis method
model.lda    <- train(classe ~ ., data = training, method="lda")
#Recursive Partitioning and Regression Trees method
model.rpart  <- train(classe ~ ., data = training, method="rpart")

# use cross validation for these methods using "validation" set
predictvalidation.rm    <- predict(model.rm, newdata = validation)
confusionMatrix(validation$classe, predictvalidation.rm)

predictvalidation.lda   <- predict(model.lda, newdata = validation)
confusionMatrix(validation$classe, predictvalidation.lda)

predictvalidation.rpart <- predict(model.rpart, newdata = validation)
confusionMatrix(validation$classe, predictvalidation.rpart)

predictvalidation.gbm <- predict(model.gbm, newdata = validation)
confusionMatrix(validation$classe, predictvalidation.gbm)

# use the best model (random forest) to forecast for the test set
data_test <- read.csv("./data/pml-testing.csv", na.strings= c("NA",""," "))
data_test_NAs <- apply(data_test, 2, function(x) sum(is.na(x)))
data_test_usable <- data_test[,which(data_test_NAs == 0)]
data_test_usable <- data_test_usable[8:length(data_test_usable)]

# predict the classes of the test set
predictTest <- predict(model.rm, data_test_usable)

print(predictTest)

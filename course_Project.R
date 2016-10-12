library(RCurl)
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot
library(caret)

#Download the data
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainingset <- read.csv(text=getURL(train_url), na.strings=c("", "NA"))
testingset <- read.csv(text=getURL(test_url), na.strings=c("", "NA"))

# Check dimensions for number of variables and number of observations
dim(trainingset)
dim(testingset)

# Delete columns with all missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]

# Some variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). We can delete these variables.
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]

# and have a look at our new datasets:
dim(trainingset)
dim(testingset)
head(trainingset)
head(testingset)

subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subsamples, ] 
subTesting <- trainingset[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)

plot(subTraining$classe, col="blue", main="Bar Plot of levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")

model1 <- rpart(classe ~ ., data=subTraining, method="class")
# Predicting:
prediction1 <- predict(model1, subTesting, type = "class")
# Plot of the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

model1 <- rpart(classe ~ ., data=subTraining, method="class")
# Predicting:
prediction1 <- predict(model1, subTesting, type = "class")
# Plot of the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(prediction1, subTesting$classe)


model2 <- randomForest(classe ~. , data=subTraining, method="class")

# Predicting:
prediction2 <- predict(model2, subTesting, type = "class")

# Test results on subTesting data set:
confusionMatrix(prediction2, subTesting$classe)


predictfinal <- predict(model2, testingset, type="class")
predictfinal



# #Remove the index column
# train_data$X <- NULL
# 
# #Remove the unused column 
# cols_to_remove <- c("user_name", "raw_timestamp_part_1",
#                     "raw_timestamp_part_2", "cvtd_timestamp")
# for (col in cols_to_remove) {
#   train_data[, col] <- NULL
# }
# 
# #Remove the column with too many missing value
# NAs <- apply(train_data,2,function(x) {sum(is.na(x))})
# train_data <- train_data[,which(NAs == 0)]
# 
# 
# #Remove near zero variance value
# library(caret)
# nsv <- nearZeroVar(train_data)
# train_data <- train_data[-nsv]
# test_data <- test_data[-nsv]
# 
# 
# #FinalSet of Predictor
# names(train_data)
# 
# #The Model
# library(randomForest)
# set.seed(1)
# obs <- c()
# preds <- c()
# for(i in 1:10) {
#   intrain = sample(1:dim(train_data)[1], size=dim(train_data)[1] * 0.8, replace=F)
#   train_cross = train_data[intrain,]
#   test_cross = train_data[-intrain,]
#   rf <- randomForest(classe ~ ., data=train_cross)
#   obs <- c(obs, test_cross$classe)
#   preds <- c(preds, predict(rf, test_cross))
# }
# 
# #COnfusion Matrix
# conf_mat <- confusionMatrix(table(preds, obs))
# conf_mat$table
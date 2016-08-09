# Balancing the dataset for equal groups

nrow(subset(file5, file5$VALID == "Yes"))
file50 <- subset(file5, file5$VALID == "Yes")
nrow(subset(file5, file5$VALID == "No"))
file51 <- subset(file5, file5$VALID == "No")

dim(file50)
dim(file51[51:120,])

#sampleids <- sample(file51$VALID,70)
#newdata <- subset(file51, file51$VALID %in% sampleids)

file52 <- rbind(file50, file51[51:120,])
nrow(file52)

# Creating model - Naive Bayes

train_rows <- sample.split(file52$VALID, SplitRatio=0.8)
train_rows
file52_train <- file52[train_rows,]
file52_test <- file52[!train_rows,]
nrow(file52_train)
nrow(file52_test)

library(e1071)
model <- naiveBayes(file52_train$VALID ~ ., data = file52_train)
model
#pred <- predict(model, file5_test[,-1])
pred <- predict(model, file52_test[,c(1:4)])
pred
table(pred, file52_test$VALID)
table(pred, file52_test$VALID, dnn = c("Predicted", "Actual"))

# NB Model accuracy
model_accuracy <- sum(diag(d))/sum(d)
model_accuracy
model_incorrect_class <- 1-sum(diag(d))/sum(d)
model_incorrect_class

# Type I error = 14%
# Type II error = 28%


# Creating model - SVM

library(e1071)
model <- svm(VALID ~ ., data = file52_train)
model
pred <- predict(model, file52_test[,c(1:4)])
pred
table(pred, file52_test$VALID)
table(pred, file52_test$VALID, dnn = c("Predicted", "Actual"))
d <- table(pred, file52_test$VALID, dnn = c("Predicted", "Actual"))

# Model accuracy
model_accuracy <- sum(diag(d))/sum(d)
model_accuracy
model_incorrect_class <- 1-sum(diag(d))/sum(d)
model_incorrect_class

# Tuning SVM

svm_tune <- tune.svm(VALID ~ ., data = file52_train, cost=10^(-1:2), gamma=c(.5,1,2), kernel = "radial")
print(svm_tune)
#svm_tune <- tune.svm(file5_train$VALID ~ ., data = file5_train, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2))) 

model_after_tune <- svm(VALID ~ ., data = file52_train, kernel="radial", cost=10, gamma=0.5)
model_after_tune
pred <- predict(model_after_tune, file52_test[,c(1:4)])
pred
table(pred, file52_test$VALID)
table(pred, file52_test$VALID, dnn = c("Predicted", "Actual"))
d <- table(pred, file52_test$VALID, dnn = c("Predicted", "Actual"))

# Model accuracy
model_accuracy <- sum(diag(d))/sum(d)
model_accuracy

# Type I error = 2/14 = 14%
# Type II error = 3/11 = 21%



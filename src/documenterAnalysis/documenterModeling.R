## documenter Modeling
library(caret)

df <- read.csv('./data/business_innovation/working/DocumenterModelData/termDocumentResponseMatrix.csv')

# create train and test test with these data
set.seed(2019)
intrain <- createDataPartition(y = df$y, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]

## brief trying of insample data

## SVM IN SAMPLE
set.seed(2019)

svm_is <- train(as.factor(y) ~., data = df, method = "svmRadial")

svmis_pred <- predict(svm_is, newdata = df)

confusionMatrix(svmis_pred, df$y)

## support vector machine
set.seed(2019)

svm_ctrl <- trainControl(method = "cv", savePred=T)

svm_Linear <- train(as.factor(y) ~., data = training, trControl = svm_ctrl, method = "svmRadial")

svm_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(svm_pred, testing$y)

## NEURAL NETWORK
set.seed(2019)

nn_ctrl <- trainControl(method = "cv", savePred=T)

nn <- train(as.factor(y) ~., data = training, trControl = nn_ctrl, method = "nnet")

nn_pred <- predict(nn, newdata = testing)

confusionMatrix(nn_pred, testing$y)

nnet

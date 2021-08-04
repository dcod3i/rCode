library(ISLR)
library(tidyverse)
library(ggthemes)
library(caret)
library(e1071)
set.seed(1)
data('OJ')

#a
inTrain<- sample(nrow(OJ), 800, replace = FALSE)
training <- OJ[inTrain,]
testing <- OJ[-inTrain,]

# b cost = 0.01 purchase as a response
svm_linear<- svm(Purchase~., data = training, kernel = 'linear',cost = 0.01)
summary(svm_linear)

#c) -- training and test error
postResample(predict(svm_linear, training), training$Purchase)
postResample(predict(svm_linear, testing), testing$Purchase)

#d tune() function is used to select the optimal cost, values ranging from 0.01 to 10
svm_linear_tune<- train(Purchase~., data = training, method = 'svmLinear2',
                        trControl = trainControl(method = 'cv', number = 10),
                        preProcess = c('center', 'scale'),
                        tuneGrid = expand.grid(cost = seq(0.01, 10, length.out = 20)))
svm_linear_tune

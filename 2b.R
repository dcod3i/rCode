#a #BOSTON
library(MASS)
library(tree)
train = sample(1:nrow(Boston), nrow(Boston)/2)

#b
tree.boston=tree(medv~., Boston , subset=train)
summary(tree.boston)
yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

#c
library(randomForest)
bag.boston= randomForest( medv~., data=Boston , subset=train , mtry=13, importance =TRUE)
bag.boston

yhat.bag = predict(bag.boston , newdata=Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag-boston.test)^2)
importance(bag.boston)

#d
set.seed(1)
rf.boston= randomForest(medv~., data=Boston , subset=train , mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)

#e
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train ,], distribution="gaussian",n.trees=5000, interaction.depth=4)
summary(boost.boston)
boost.boston=gbm(medv~.,data=Boston[train ,], distribution="gaussian", n.trees =1000, interaction.depth =4, shrinkage =0.01,verbose=F)
yhat.boost=predict(boost.boston ,newdata =Boston[-train ,], n.trees=1000)
plot(yhat.boost, boston.test)
abline(0,1)
mean((yhat.boost - boston.test)^2)

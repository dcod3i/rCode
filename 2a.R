#Part-a
library(MASS)
set.seed(1)
library(ISLR)
train=sample(1: nrow(Carseats), nrow(Carseats)/2)

#Part-b
library(tree)
tree.carseats=tree(Sales~., Carseats, subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
yhat=predict (tree.carseats ,newdata=Carseats[-train,])
carseats.test=Carseats[-train ,"Sales"]
plot(yhat ,carseats.test)
abline (0,1)
mean( (yhat-carseats.test) ^2)

#Part-C
#Tree pruning using cv.tree 
cv.carseats=cv.tree(tree.carseats) 
plot(cv.carseats$size,cv.carseats$dev, type='b')
#Tree pruning using prune.tree
prune.carseats=prune.tree(tree.carseats, best=5)
plot(prune.carseats)
text(prune.carseats, pretty=0)
#Prediction using unpruned tree
prune.carseats=prune.tree(tree.carseats, best=5)
plot(prune.carseats)
text(prune.carseats, pretty=5)
yhat=predict (tree.carseats ,newdata=Carseats[-train,])
carseats.test=Carseats[-train ,"Sales"]
plot(yhat ,carseats.test)
abline (0,1)
mean((yhat -carseats.test)^2)

#Part-E
#Random Forest
library(randomForest)
set.seed(1)
bag.carseats=randomForest(Sales~., data=Carseats, subset = train, mtry=10, importance= TRUE)
bag.carseats
yhat.bag= predict(bag.carseats, newdata = Carseats[-train,])
plot(carseats.test)
abline(0,1)
mean((yhat.bag-carseats.test)^2)

#Part-D
#Bagging with Random forest
rf.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=6, importance=TRUE)
yhat.rf=predict(rf.carseats, newdata = Carseats[-train,])
mean((yhat.rf-carseats.test)^2)
importance(rf.carseats)

#Boosting
library(gbm)
set.seed(1)
boost.carseats=gbm(Sales~., data=Carseats[train,], distribution = "gaussian", n.tree=5000, interaction.depth = 4)
summary(boost.carseats)
par(mfrow=c(1,2))
plot(boost.carseats,i="Price")
plot(boost.carseats,i="ShelveLoc")
yhat.boost=predict(boost.carseats, newdata = Carseats[-train,], n.trees = 5000)
mean((yhat.boost-carseats.test)^2)


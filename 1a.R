#Part-A
require(ISLR)
data(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)

#Part-B
weekly.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = "binomial")
summary(weekly.fit)

#Part-C
weekly.probs = predict(weekly.fit, type = "response")
weekly.preds = ifelse(weekly.probs > 0.5, "Up", "Down")
cmat = table(Weekly$Direction, weekly.preds)
cmat
corre_pred = (cmat["Down", "Down"] + cmat["Up", "Up"]) / sum(cmat)
corre_pred

#Part-D
weekly.train = (Weekly$Year <= 2008)
weekly.test = Weekly[!weekly.train,]
weekly.fit = glm(Direction ~ Lag2, data = Weekly, subset = weekly.train, family = "binomial")
weekly.probs.d = predict(weekly.fit, type = "response", newdata = weekly.test)
weekly.preds.d = ifelse(weekly.probs.d > 0.5, "Up", "Down")
cmat.d = table(weekly.test$Direction, weekly.preds.d)
cmat.d
corre_pred.d = (cmat.d["Down", "Down"] + cmat.d["Up", "Up"]) / sum(cmat.d)
corre_pred.d

#Part-E
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = weekly.train)
lda.preds = predict(lda.fit, newdata = weekly.test)
cmat.e = table(weekly.test$Direction, lda.preds$class)
cmat.e
corre_pred.e = (cmat.e["Down", "Down"] + cmat.e["Up", "Up"]) / sum(cmat.e)
corre_pred.e

#Part-F
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = weekly.train)
qda.preds = predict(qda.fit, newdata = weekly.test)
cmat.f = table(weekly.test$Direction, qda.preds$class)
cmat.f
corre_pred.f = (cmat.f["Down", "Down"] + cmat.f["Up", "Up"]) / sum(cmat.f)
corre_pred.f

#Part-G
library(class)
train.X = cbind(Lag1 ,Lag2)[weekly.train,]
test.X = cbind (Lag1 ,Lag2)[!weekly.train,]
train.Direction = Direction [weekly.train]
set.seed (1)
knn.preds = knn (train.X ,test.X, train.Direction ,k=1)
cmat.g = table(weekly.test$Direction, knn.preds)
cmat.g
corre_pred.g = (cmat.g["Down", "Down"] + cmat.g["Up", "Up"]) / sum(cmat.g)
corre_pred.g

#Part-H
rbind(corre_pred.d, corre_pred.e, corre_pred.f, corre_pred.g)


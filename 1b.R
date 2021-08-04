#Part-a
Auto$mpg01 <- with(ifelse(mpg>median(mpg), "1", "0"), data=Auto)

#Part-b
attach(Auto)
par(mfrow=c(2,3))
for(i in names(Auto)){
# excluding the own mpgs variables and others categorical variables 
if( grepl(i, pattern="^mpg|cylinders|origin|name")){ next}
boxplot(eval(parse(text=i)) ~ mpg01, ylab=i, col=c("red", "blue")) 
}
plot(mpg)

attach(Auto)
colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
par(mfrow=c(1,2))
for(i in c("cylinders", "origin")){
  aux <- table(eval(parse(text=i)), mpg01)
  cols <- colors[1:nrow(aux)]
barplot(aux, xlab="mpg01", ylab=i, beside=T, legend=rownames(aux), col=cols)
}

#Part-c
set.seed(1)
rows <- sample(x=nrow(Auto), size=.75*nrow(Auto))
trainset <- Auto[rows, ]
testset<- Auto[-rows, ]

#Part-d
library(MASS)
lda.fit<- lda(mpg01 ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=trainset)
lda.pred<- predict(lda.fit, testset)
table(testset$mpg01, lda.pred$class)
round(sum(lda.pred$class!=testset$mpg01)/nrow(testset)*100,2)

#Part-e
qda.fit<- qda(mpg01 ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=trainset)
qda.pred<- predict(qda.fit, testset)
table(testset$mpg01, qda.pred$class)
round(sum(qda.pred$class!=testset$mpg01)/nrow(testset)*100,2)

#Part-f
lr.fit<- glm(as.factor(mpg01) ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=trainset, family="binomial")
lr.probs<- predict(lr.fit, testset, type="response")
lr.pred<- ifelse(lr.probs>0.5, "1", "0")
table(testset$mpg01, lr.pred)
round(sum(lr.pred!=testset$mpg01)/nrow(testset)*100,2)

#Part-g
library(class)
sel.variables<- which(names(trainset)%in%c("mpg01", "displacement", "horsepower", "weight", "acceleration", "year", "cylinders", "origin"))
set.seed(1)
accuracies <- data.frame("k"=1:10, acc=NA)
for(k in 1:10){
  knn.pred<- knn(train=trainset[, sel.variables], test=testset[, sel.variables], cl=trainset$mpg01, k=k)
  # test-error
accuracies$acc[k]= round(sum(knn.pred!=testset$mpg01)/nrow(testset)*100,2) 
}

accuracies


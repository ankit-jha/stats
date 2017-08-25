
require(MASS)
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300)

require(gbm)
boost.boston=gbm(medv~.,data=Boston[train,],distribution = "gaussian", n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)

boost.boston
summary(boost.boston)
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

# Check boosting error performance as one increases number of trees
n.trees=seq(from=100,to=10000,by=10)
predmat=predict(boost.boston,newdata = Boston[-train,], n.trees = n.trees)
dim(predmat)
boostingerr=with(Boston[-train],apply( (predmat-medv)^2,2,mean))
plot(n.trees,boostingerr,pch = 19, ylab = "Mean Squared Error", xlab = "# Trees",
     main = "Boosting Test Error")

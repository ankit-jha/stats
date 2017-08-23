# Random Forests

require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300)
?Boston

rf.boston=randomForest(medv~.,data=Boston,subset=train)
rf.boston

# The MSR and % variance explained are based on OOB (Out Of Bag Estimates)
# Tuning parameter is 'mtry' which is number of variables randomly chosen at each split


oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,Boston[-train,])
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
  cat(mtry," ")
}  

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab = "Mean Squred Error")
legend("topright",legend = c("OOB","Test"), pch = 19, col = c("red","blue"))

# Decision Trees

require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)

# Fit the tree
tree.carseats = tree(High~.-Sales,data = Carseats)
summary(tree.carseats)
plot(tree.carseats)

#Annotate text
text(tree.carseats,pretty = 0)

#Prints the tree in text format
tree.carseats
#Create sample training and test set
set.seed(1011)
train=sample(1:nrow(Carseats),250)
tree.carseats=tree(High~.-Sales,data = Carseats,subset = train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty = 0)

#Predict on test set
tree.pred=predict(tree.carseats,Carseats[-train,],type = "class")

#Classification table
with(Carseats[-train,],table(tree.pred, High))

#Cross validation to prune the tree
cv.carseats=cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats
plot(cv.carseats)

prune.carseats=prune.misclass(tree.carseats,best = 13)

#Re evaluate the pruned tree
tree.pred=predict(prune.carseats,Carseats[-train,],type = "class")
#Classification table
with(Carseats[-train],table(tree.pred, High))

library(tree)
library(randomForest)
library(MASS)
library(tree)
library(gbm)
library(e1071)
library(ggplot2)
library(fpc)
library(cluster)

#read data
setwd("C:/Users/Andrew Foresi/Documents/A Masters Year 1/CSE 780/Assignment 3/Ecoli")
ecoli = read.csv("ecoli.txt")

ecoli$Sequence <- NULL
dataset <- ecoli[,-c(1,9)]
head(dataset)
boxplot(scale(dataset))
boxplot(scale(ecoli[,-c(9)]))

#fit the data to the tree

set.seed(3)
train = sample (1: nrow(ecoli), nrow(ecoli)/2)
ecoli$class<-factor(ecoli$class)
tree.ecoli <- tree(class ~ mcv + gvh + lip + chg + aac + alm1 + alm2, data = ecoli)
summary(tree.ecoli)
plot(tree.ecoli)
text(tree.ecoli)
ecoli.test=ecoli[-train,"class"]
ecoli.pred=predict(tree.ecoli,ecoli[-train,],type="class")
tab<-table(ecoli.test,ecoli.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand

classAgreement(tab)


#pruning

set.seed(3)
cv.ecoli = cv.tree(tree.ecoli, FUN = prune.misclass)
help(cv.tree)


names(cv.ecoli)
cv.ecoli  #best deviance is 55 which corresponds to a tree size of 10

prune.ecoli = prune.misclass(tree.ecoli, best = 9)
plot(prune.ecoli)
text(prune.ecoli, pretty = 0)

ecoli.pred = predict(prune.ecoli, type="class")


#bagging




set.seed(3)
formula("class~alm1 + mcv + gvh + aac + alm2")
ecoli$class <- factor(ecoli$class)
bag.ecoli=randomForest(class~alm1 + mcv + gvh + aac + alm2, data = ecoli, subset=train,mtry=5,importance=TRUE,type="class")
bag.ecoli
ecoli.pred=predict(bag.ecoli,ecoli[-train,],type="class")
tab<-table(ecoli.test,ecoli.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand

importance(bag.ecoli)
varImpPlot(bag.ecoli)

?importance
#random forests
set.seed(3)
rf.ecoli=randomForest(class ~mcv + gvh + lip + chg + aac + alm1 + alm2,data=new.dat,subset=train,mtry=4,importance=TRUE,type="class")
rf.ecoli
ecoli.pred=predict(rf.ecoli,ecoli[-train,],type="class")
tab<-table(ecoli.test,ecoli.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand

importance(rf.ecoli)
varImpPlot(rf.ecoli)


### Boosting
set.seed(1)
boost.ecoli=gbm(class ~mcv + gvh + lip + chg + aac + alm1 + alm2,data=ecoli[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
# We get a plot with summary
summary(boost.ecoli)
# Using type="response" means that class membership probabilities are returned for each observation 
yhat.boost=predict(boost.ecoli,newdata=ecoli[-train,],n.trees=5000,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
class.pred<-rep(0,89)
for(i in 1:89){
  which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(ecoli.test,class.pred)
tab
1-classAgreement(tab)$diag # I get 0.056
classAgreement(tab)$crand

###################################################################
###################################################################
###################################################################
# Increase lambda to 0.01 (default is 0.001), keep d=4
set.seed(1)
boost.ecoli=gbm(class ~mcv + gvh + lip + chg + aac + alm1 + alm2,data=ecoli[train,],distribution="multinomial",n.trees=5000,interaction.depth=4,shrinkage=0.01)
par(mfrow=c(1,2))
summary(boost.ecoli)
# The following is a suggestion about how many trees to use for the test set
# (I get 928 with a warning that it may be an underestimate) as well as a nice plot
# We did *not* do this in class, but it is important (unlike random forests, "overfitting" is a concern here)
gbm.perf(boost.ecoli) 
# Using type="response" means that class membership probabilities are returned for each observation 
yhat.boost=predict(boost.ecoli,newdata=ecoli[-train,],n.trees=928,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
  which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(ecoli.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ....this is very good (as good as the random forest from above)
classAgreement(tab)$crand
# What if 928 was an underestimate... 
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1200,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
  which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ...no change
classAgreement(tab)$crand

# Reduce lambda to 0.01 (default is 0.001), put d=1
set.seed(1)
boost.wine=gbm(Class~.,data=wine[train,],distribution="multinomial",n.trees=5000,interaction.depth=1,shrinkage=0.01)
summary(boost.wine)
# This is a suggestion about how many trees to use for the test set (I get 1098 with a warning)
gbm.perf(boost.wine) 
# Predict class memebrship proabailities
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1098,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
  which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.056 ...not as good
classAgreement(tab)$crand
# Again, suppose 1098 is an underestimate
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1250,distribution="multinomial",type="response")
for(i in 1:89){
  which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag 
classAgreement(tab)$crand

library(corrplot)
library(car)



#create a correlation matrix
correlationmatrix = cor(ecoli[, 2:8])
corrplot.mixed(correlationmatrix, type="upper")



head(ecoli)
boxplot(ecoli$aac~ecoli$class, main="Classes", ylab="aac", xlab="class")




mycolor <- c("red","green3","blue", "yellow", "black", "orange", "purple", "grey")[as.factor(ecoli$class)]
plot(ecoli$alm1, ecoli$mcv, pch = 8, col = mycolor,
     main = "Density plot", xlab = "mcv", ylab = "alm1")
?plot

dat <- ecoli[, -5] # without known classification 
# Kmeans clustre analysis
clus <- kmeans(ecoli, centers=3)
# Fig 01
plotcluster(dat, clus$cluster)


# Load the data
library("MASS")
data("geyser")
# Scatter plot
library("ggpubr")
install.packages("ggpubr")
ggscatter(ecoli, x = "alm1", y = "mcv")+
  geom_density2d() # Add 2D density


# Week 6 - Classification Trees Assignment
ec <- read.csv("C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-6/Wk-6-EC-RPART/wk-6-EC-RPART/ec.csv")
attach(ec)
#Creation of Training and Test Data
s<-c(sample(1:500,400), sample(501:1000,400), sample(1001:10000,8000))
trn<-ec[s,]
tst<-ec[-s,]
library(rpart)
library(caret)
# Building the Classification Tree. As Dependent Variable is Binomial.
# Churned ==0 / Stayed ==1
# Model - mT created from Training Data Sample. 
# Same model tested with "Test" data for Prediction Accuracy . 
mT<-train(st~.,method="rpart",data =trn)

library(rattle)
fancyRpartPlot(mT$finalModel)

#Prediction for Training Data
TrnPred<-predict(mT, newdata=trn)
head(TrnPred)
# Transferring the Data Type - TrnPred from Numeric Vector to FAACTOR 
# As seen below we do not get the 0 and 1 classification as we did with the "iris" data. 
# The table() seen below is the Confusion Matrix ...
TrnPred<-as.factor(TrnPred)
head(TrnPred)
table(trn$st,TrnPred)

TstPred<-predict(mT, newdata=tst)
head(TstPred)
table(tst$st,TstPred)

#Building the Tree with Function - rpart and method ="class"
mRp<-rpart(st~.,data=trn, method="class")
fancyRpartPlot(mRp)
# using Control -- minsplit==500 and cp==0.001
mRp_minsplit500cp0.001<-rpart(st~.,data=trn, method="class",control=rpart.control(minsplit=500, cp=0.001))
fancyRpartPlot(mRp_minsplit500cp0.001)
# using Control -- minsplit==1000 and cp==0.05
mRp_minsplit1000cp0.05<-rpart(st~.,data=trn, method="class",control=rpart.control(minsplit=1000, cp=0.05))
fancyRpartPlot(mRp_minsplit1000cp0.05)
# using Control -- minsplit==1000 and cp==0.05
mRp_minsplit100cp0.005<-rpart(st~.,data=trn, method="class",control=rpart.control(minsplit=500, cp=0.005))
fancyRpartPlot(mRp_minsplit100cp0.005)
# using Control -- minsplit==200 and cp==0.0005
mRp_minsplit200cp0.0005<-rpart(st~.,data=trn, method="class",control=rpart.control(minsplit=200, cp=0.0005))
fancyRpartPlot(mRp_minsplit200cp0.0005)

#Display Results
print(mRp)
print(mRp_minsplit200cp0.0005)

# plot cp() - complexity parameter value
plotcp(mRp)
plotcp(mRp_minsplit200cp0.0005)

printcp(mRp)
printcp(mRp_minsplit200cp0.0005)

mRp$cptable[which.min(mRp$cptable[,"xerror"]),"CP"]
# as seen from the CP table and the "which.min" formula...
# CP ==0.01  is the Min CP corresponding to the Minimum "xerror".
# Combining the Prune command and the which.min commands...


## Testing if cptable can be created for a model - made by "caret" in place of "rpart" 
pmRp<- prune(mRp, cp= mRp$cptable[which.min(mRp$cptable[,"xerror"]),"CP"])
pmRp_minsplit200cp0.0005<- prune(mRp_minsplit200cp0.0005, cp= mRp_minsplit200cp0.0005$cptable[which.min(mRp_minsplit200cp0.0005$cptable[,"xerror"]),"CP"])
# plot the Pruned tree using - #fancyRpartPlot(pmRp)
fancyRpartPlot(pmRp)
fancyRpartPlot(pmRp_minsplit200cp0.0005)

# Bagging -- Bagging will have bias similar to the individual models
# but a reduced variance as we are averaging over individual models.

library(caret)
m_bag<-train(st~.,method="treebag",data =trn)
print(m_bag)


# Creating a Random Forest ...

# Sir Code - mFor<-train(st~.,method="rf",data =trn,prox=TRUE)
# own rf_model<-train(st~.,data=trn,method="rf",trControl=trainControl(method='cv',number=5),prox=TRUE,allowParallel=TRUE)
# Both above code not running thus trying - alternatve code ....
library(randomForest)
rf_model_1<-randomForest(st~.,data =trn,ntree = 5)
rf_model_1<-randomForest(st~.,data =trn,ntree = 10)
rf_model_1
# This code - has created a Random Forest -- but its a Type :- REGRESSION we need a Type :- Classification . 
# Also we gort the below mentioned warning message ....
# Warning message:
#   In randomForest.default(m, y, ...) :
#   The response has five or fewer unique values.  Are you sure you want to do regression?

# Checking the head - of the 2nd Tree in the Random Forest 
head(getTree(rf_model_1,k=2))

# Checking the head - of the 3rd Tree in the Random Forest 
head(getTree(rf_model_1,k=3))

# From the Two - Head , values as seen here 
# - the "left daughter" and  "right daughter" , values
# are the same for both the Trees . 
# Values of "split var" , "split point" and "prediction" are different. 

# Relative Importance ....
# importance(mRp) ## - doesnt work as it cant be applied to an object from - rpart - mRp is a Large rpart ( 14 elements , 1.5Mb)
# importance(mT) ## Error in UseMethod("importance") : no applicable method for 'importance' applied to an object of class "c('train', 'train.formula')"

# Exporting data samples - trn and tst to .csv for analysis ...
getwd()
write.table(trn,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-6/Wk-6-EC-RPART/wk-6-EC-RPART/trn.csv",sep=",")
write.table(tst,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-6/Wk-6-EC-RPART/wk-6-EC-RPART/tst.csv",sep=",")

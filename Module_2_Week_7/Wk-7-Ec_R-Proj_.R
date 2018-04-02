# Wk-7 EC - R- Project 
ec <- read.csv("C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-7/Wk-7-Ec_R-Proj/ec.csv")
attach(ec)
summary(ec)
library(caret)
library(rpart)
library(rattle)
inTrain <- createDataPartition(y=ec$spec,p=0.7, list=FALSE)
trn <- ec[inTrain,]
tst <- ec[-inTrain,]
dim(trn); dim(tst)
m<-rpart(spec~.,data=trn,method="anova")
par(mai=c(0.1,0.1,0.1,0.1))
plot(m,main="Regression Tree -1",col=3,compress=TRUE,branch=0.2,uniform="TRUE")
text(m,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))
fancyRpartPlot(m)

m_minsplit500cp0.001<-rpart(spec~.,data=trn, method="anova",control=rpart.control(minsplit=500, cp=0.001))
library(rattle)
fancyRpartPlot(m_minsplit500cp0.001)
m_minsplit100cp0.001<-rpart(spec~.,data=trn, method="anova",control=rpart.control(minsplit=100, cp=0.001))
fancyRpartPlot(m_minsplit100cp0.001)

## INTERACTIONS ## 
## As seen from plot Interaction is as defined below --
# Root Node - Split of variable eco < 1610 here n=7001.  
# Child Node -9 , where n=1075 -- sco < 88,eco < 368 and qcec >= 22 is the result of Interaction between - sco , eco and qcec. 

# Predicting from data Trainig ..
TrnPred_m<-predict(m, newdata=trn)
head(TrnPred_m)
x<-as.matrix(TrnPred_m)
# As its a Regression problem - we need to Minimize Sum of Squared Errors 
# 
# Bagging - Bagging will have bias similar to the individual models
# but a reduced variance as we are averaging over individual models.

library(caret)
mBAG<-train(spec~.,method="treebag",data =trn)
print(mBAG$finalModel)

## Random Forests 

library(randomForest)
rf <- randomForest(spec~., data=trn,ntree=10)
yhat_tst <- predict(rf, tst)
yhat_trn <- predict(rf, trn)
mean((yhat_tst - tst$spec)^2)
mean((yhat_trn - trn$spec)^2)
rf
#View a Tree specified by k in the Forest
# getTree(rf,k=2)
#Predictions
pred<-predict(rf,trn)
table(pred,trn$spec)
#Relative Importance
importance(rf)
# varImp(rf)
varImpPlot(rf)
# start a plot, and plot the variable importance plot
varImpPlot(rf, type=2, pch=11, col=4, cex=2, main="")

## Boosting ----
library(gbm)
boost <- gbm(spec~. , data=trn,distribution = 'gaussian', n.trees = 10, interaction.depth = 4)
summary(boost)
# as seen from Summary - eco   eco 60.626854 and sco   sco 10.632023 have high Relative Influence . 
# We plot them separately ...
par(mfrow=c(1,2))
plot(boost, i='eco')
plot(boost, i='sco')

boost.pred <- predict (boost, tst, n.trees=10)
mean((boost.pred - tst$spec)^2)

ctr <- trainControl(method = "cv", number = 10)
boost.caret <- train(spec~., trn,method='bstTree',preProc=c('center','scale'),trControl=ctr)
# yes
boost.caret
plot(boost.caret)

## Here, with some better tuned parameters, we compare prediction - accuracy with random forests.??
boost.caret.pred <- predict(boost.caret, tst)
mean((boost.caret.pred - tst$spec)^2)

## RF - > mean((boost.pred - tst$spec)^2) == [1] 373.8108
## boost.caret.pred - mean((boost.caret.pred - tst$spec)^2) == [1] 311.0449

## boost.caret.pred  is better as - mean == 311.0449 is lower . 

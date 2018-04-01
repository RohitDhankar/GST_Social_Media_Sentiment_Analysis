# Week 5 - Logistic Regression Assignment
# For the E-commerce data set, perform regression on "Churn/Stay" using the other numeric variables as the independent variables, in R. Use glm(...family=binomial(link=logit)).
# a. Generate the equation.
# b. Use the summary to view the diagnostics, and write a summary of the results
# c . Do you like the model or not? Why?
# d. How would you determine what is the "best" linear model?
# e. How will you add interactions? Which ones?
# f. Print results and graphs/charts, and submit Assignment 5.

#Reading Data
e<-read.csv("C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/ec.csv")
# First model -all independent variables. 
attach(e)
summary(e)
m1<-glm(e$st~e$sls+e$sco+e$eco+e$csec+e$osec+e$qcec+e$spec+e$ad, data =e, family = "binomial")
summary(m1)
# From Summary- FOUR undermentioned variables, not Significant to Churn Status:-
# e$sco - p-Value==0.775
# e$csec- p-Value==0.102 
# e$osec- p-Value==0.388 
# e$spec- p-Value==0.118
## Remove FOUR variables - check if model is more Significant.
# 
plot(predict(m1, type="response"),residuals(m1, type= "deviance"))
plot(hatvalues(m1))
plot(rstudent(m1))
plot(cooks.distance(m1))
## Diagnostic plots helpful when response variable takes on many values. 
# Here response variable is Binomial - thus ignoring Diagnostic Plots. 

# Create Confusion Matrix for this glm().
# Compare Classification capability with other glm()'s. 
ModelFit_m1<-m1$fit
ProbabModelFit_m1<-m1$fit>0.5
P1<-as.data.frame(ProbabModelFit_m1)
# "if" loop to convert TRUE and FALSE into 1 and 0. 
v1<-vector()
for (i in 1:nrow(P1))
{
  if(P1[i,1]=="TRUE")
  {
    v1[i]<-1
  }
  if(P1[i,1]=="FALSE")
  {
    v1[i]<-0
  }
}
V1<-as.data.frame(cbind(e$st,v1))
colnames(V1)<-c("yhat","m1")
# write.table(V1,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/V1.csv", sep=",")


# Second glm()-as seen below - 
m2<-glm(e$st~e$sls+e$eco+e$qcec+e$ad, data =e, family = "binomial")
summary(m2)
# Independent Variables - all FOUR have Significant p-Values. 
# Comparison between models- AIC - AIC of 2nd Model -m2 Higher at- AIC: 10873
# Prefer a glm() - with Smaller Mean Deviance / Residual Deviance - m1==Residual deviance: 10789 # Whereas - m2==Residual deviance: 10863
# Confusion Matrix created in Excel as seen below - 
ModelFit_m2<-m2$fit
ProbabModelFit_m2<-m2$fit>0.5
P2<-as.data.frame(ProbabModelFit_m2)
# "if" loop to convert TRUE and FALSE into 1 and 0. 

v2<-vector()
for (i in 1:nrow(P2))
{
  if(P2[i,1]=="TRUE")
  {
    v2[i]<-1
  }
  if(P2[i,1]=="FALSE")
  {
    v2[i]<-0
  }
}

V2<-as.data.frame(cbind(e$st,v2))
colnames(V2)<-c("yhat","m2")
# write.table(V2,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/V2.csv", sep=",")
## CIs using profiled log-likelihood
confint(m2)

# Analysis of the Residual Deviance from the ANOVA summary ...Analysis of Deviance Table
anova(object=m1, test="Chisq")
# Null- Resid deviance ==13842 at Df==9999 
# e$ad- Resid deviance ==10789 at Df==9991
# The deviance was reduced by points on 8 degrees of freedom, for a p-value of... 
x<-13842-10789
x
1 - pchisq(3053, df=8)
plot(m2$fitted)
abline(v=30.5,col="red")
abline(h=.3,col="green")
abline(h=.5,col="green")
text(15,.9,"Churned = 0")
text(40,.9,"Stayed = 1")
# Hosmer - The Hosmer-Lemeshow test...
library("ResourceSelection", lib.loc="~/R/win-library/3.1")
hl <- hoslem.test(m1$y, fitted(m1), g=10)
hl
cbind(hl$observed,hl$expected)
pihat1 <- m1$fitted
pihat1cat <- cut(pihat1, breaks=c(0,quantile(pihat1, probs=seq(0.1,0.9,0.1)),1), labels=FALSE)

m3<-glm(e$st~e$sls*e$sco+e$eco*e$csec+e$osec*e$qcec+e$spec*e$ad, data =e, family = "binomial")
summary(m3)
# Confusion Matrix created in Excel as seen below - 
ModelFit_m3<-m3$fit
ProbabModelFit_m3<-m3$fit>0.5
P3<-as.data.frame(ProbabModelFit_m3)
# "if" loop to convert TRUE and FALSE into 1 and 0. 

v3<-vector()
for (i in 1:nrow(P3))
{
  if(P3[i,1]=="TRUE")
  {
    v3[i]<-1
  }
  if(P3[i,1]=="FALSE")
  {
    v3[i]<-0
  }
}

V3<-as.data.frame(cbind(e$st,v3))
colnames(V3)<-c("yhat","m2")
# write.table(V3,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/V3.csv", sep=",")



m4<-glm(e$st~e$sls+e$sco*e$eco*e$csec+e$qcec*e$spec*e$ad, data =e, family = "binomial")
summary(m4)
# Model - glm(m3) - AIC: 10803
# Model - glm(m4) -AIC: 10761
ModelFit_m4<-m4$fit
ProbabModelFit_m4<-m4$fit>0.5
P4<-as.data.frame(ProbabModelFit_m4)
# "if" loop to convert TRUE and FALSE into 1 and 0. 

v4<-vector()
for (i in 1:nrow(P4))
{
  if(P4[i,1]=="TRUE")
  {
    v4[i]<-1
  }
  if(P4[i,1]=="FALSE")
  {
    v4[i]<-0
  }
}

V4<-as.data.frame(cbind(e$st,v4))
colnames(V4)<-c("yhat","m4")
# write.table(V4,"C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/V4.csv", sep=",")


# Analysis of the Residual Deviance from the ANOVA summary ...Analysis of Deviance Table
anova(object=m2, test="Chisq")
# Null- Resid deviance ==13842 at Df==9999 
# e$ad- Resid deviance ==10863 at Df==9995

anova(object=m3, test="Chisq")
# Null- Resid deviance ==13842 at Df==9999 
# e$spec:e$ad- Resid deviance ==10777 at Df==9987

anova(object=m4, test="Chisq")
# Null- Resid deviance ==13842 at Df==9999 
# e$qcec:e$spec:e$ad- Resid deviance ==10729 at Df==9984


# Tried to Combine Binomial variables of EComm data and glm()- create Confusion Matrix within R. Could not be done.
# install.packages('caret', dependencies = TRUE)
## The R inbuilt caret Package - Confusion Matrix is not used as the code is not clear...
# library("caret", lib.loc="~/R/win-library/3.1")
# lvs <- c("Stayed", "Churned")
# truth <- factor(
#   c(
#     rep(lvs, times = c(4199,5801))),
#                 levels = rev(lvs))
# pred <- factor(
#   c(
#     rep(lvs, times = c(5227,4773))),               
#   levels = rev(lvs))
# 
# xtab <- table(pred, truth)
# xtab
# confusionMatrix(xtab)
# confusionMatrix(pred, truth)
# confusionMatrix(xtab, prevalence = 0.25)
# 
# 
# 
# lvs <- c("normal", "abnormal")
# truth <- factor(rep(lvs, times = c(86, 258)),
#                 levels = rev(lvs))
# pred <- factor(
#   c(
#     rep(lvs, times = c(54, 32)),
#     rep(lvs, times = c(27, 231))),               
#   levels = rev(lvs))
# 
# xtab <- table(pred, truth)
# 
# confusionMatrix(xtab)
# confusionMatrix(pred, truth)
# confusionMatrix(xtab, prevalence = 0.25)   
#Reading Data
e<-read.csv("C:/STAT/____________STATISTICS_OWN/__________bridge_school/CANVAS-Pdf's/__Module_2/Week-5/Week-5-R-Ec/Wk-5-EC-Logistic/ec.csv")
# First model with all independent variables. 
attach(e)
summary(e)

#Converting Churn in E-Commerce data set to Numeric Ordinal - Churned==0 and Stayed==1
Churn1<-vector()
for (i in 1:nrow(e))
{
  if(e[i,1]=="Churned")
  {
    yy[i]<-1
  }
  if(e[i,1]=="Stayed")
  {
    yy[i]<-0
  }
}
#Converting Churn in S2 to 0 and 1
Churn2<-vector()
for (i in 1:nrow(S1))
{
  if(S2[i,1]=="Churned")
  {
    Churn2[i]<-1
  }
  if(S2[i,1]=="Stayed")
  {
    Churn2[i]<-0
  }
}
data1<-Churn1
data2<-Churn2
##
m<-glm(e$st~e$sls+e$sco+e$eco+e$csec+e$osec+e$qcec+e$spec+e$ad, data =e, family = "binomial")
summary(m)
# Checking Normality of the Deviance Residuals
# QQ Plot 
r<-m$resid
head(r)
qqnorm(r,main="Normal-QQ-Plot_Deviance Residuals-Model1 -'m'")
qqline(r,col = 2)
# KS Test on Deviance Residuals for Normality - seen p-value < 2.2e-16 , thus Deviance Residuals Normal. 
k<-r
ks<-(k-mean(k))/sqrt(var(k))
ks.test(ks,rnorm(length(ks)))
# Histogram of Deviance Residuals
hist(r)
TBD === m$fit
m$fit>0.5


#Logistic Regression
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
model<-glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(model)
model$fit
model$fit>0.5




#Deviance is -2lnL
#Null Deviance is -2ln(Null L) #Null L means when insginificant regression

#AIC = Model Deviance + 2*(No. of Parameters)

#Null-ModelDeviance ~ ChiSquare with 1
#H0: Model is insignificant
#H1: Model is significant



install.packages("ResourceSelection")
library(ResourceSelection)
set.seed(43657)
n <- 100
x <- rnorm(n)
xb <- x
pr <- exp(xb)/(1+exp(xb))
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family="binomial")

hoslem.test(mod$y, fitted(mod), g=10)

summary(lm(mpg~hp+drat+wt+qsec+vs+gear+carb,data=mtcars)
)

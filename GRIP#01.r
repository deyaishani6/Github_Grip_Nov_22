
rm(list=ls())
setwd("D:/")
d=read.csv("student_scores.csv");d
attach(d)

library(ggplot2)
g1=qplot(Hours,data=d,geom="boxplot",fill=I("red"),xlab="Hours of Study",main="Boxplot on hours of Study",alpha=I(0.4));g1
 g2= qplot(Scores,data=d,geom="boxplot",fill=I("yellow"),alpha=I(0.7),main="Boxplot of Percentage of marks obtained");g2

 ggplot(d,aes(Hours,Scores))+geom_point(col=2,lwd=1)+labs(title=" Scatterplot of Scores vs Hours") 

## *[Making *Train* and *Test* data:]{.underline}*We will fit the regression model on Train data and check whether the fit is well or not based on the test data.

n=nrow(d)
set.seed(12345)
sam=sample(c(1,0),n,replace=T,prob=c(0.8,0.2))
d1=cbind(d,sam)
##extracting the train data
attach(d1)
train_d=d1[sam==1,]
test_d=d1[sam==0,]
reg=lm(Scores~Hours,train_d)
summary(reg)
paste("Scores=",round(reg$coefficients[[1]],2),"+",round(reg$coefficient[[2]],2),"*(Hours)")
predicted_val=as.numeric(predict(reg))
resi_error=as.numeric(resid(reg))
ggplot(NULL,aes(train_d$Hours,resi_error))+geom_point(col=3,lwd=1)+labs(title="Residual plot",x="Hours of Study",y="residuals")
g=ggplot(NULL,aes(train_d$Hours))+
  geom_point(aes(y=train_d$Scores,col="actual"))+geom_line(aes(y=predicted_val,col="predicted"))
g+scale_color_manual(name="Scores",             breaks=c("actual","predicted"),           values=c("actual"="blue","predicted"="red"))+labs(title="Hours vs Scores",x="Hours",y="Scores")
prediction=as.numeric(predict(reg,data.frame(Hours=test_d$Hours)))
attach(test_d)
coefficient_determination=1-(sum((Scores-prediction)^2)/sum((Scores-mean(Scores))^2))
intercept=reg$coefficients[[1]]
estimate=reg$coefficients[[2]]
predicted_score=intercept+(estimate*(9.25));predicted_score

library(dplyr)
library(tidyr)
library(MASS)
library(caTools)
library(ISLR)
library(car)
library(ggplot2)
library(plotly)
library(gridExtra)
fix(Prestige)

presData<-as.data.frame(Prestige)

dim(presData)

str(presData)
glimpse(presData)

colnames(presData)

head(presData, 3)
detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(presData, detectNAs)

presData<-na.omit(presData)

summary(presData)





p1<-ggplot(presData, aes(x=education)) + geom_histogram( binwidth = 1)+
  labs(title="Histogram of Average years of Education") +
  labs(x="average years of Education") +
  labs(y="Frequency Count")

p2<-ggplot(presData, aes(x=income)) + geom_histogram( binwidth = 1000)+
  labs(title="Histogram of Income") +
  labs(x="Income") +
  labs(y="Frequency Count")

p3<-ggplot(presData, aes(x=education, y=income)) + geom_point( position="jitter")+
  labs(title="Relationship between Education and Income") +
  labs(x="Education") +
  labs(y="Income")

p4<-ggplot(presData, aes(x=education, y=income)) + geom_point() + geom_smooth( method="lm", se=FALSE)+
  labs(title="Linear Relationship between Education and Income") +
  labs(x="Education") +
  labs(y="Income")
grid.arrange(p1,p2, p3, p4, nrow=2, ncol=2)
  
  
set.seed(100)
split<-sample.split(presData$income, SplitRatio=0.8)
trainSet<-subset(presData, split==TRUE)
testSet<-subset(presData, split==FALSE)  

cat("\014")
lm.fit1<-lm(income~., data=trainSet)
lm.fit1
summary(lm.fit1)

lm.fit2<-lm(income~education, data=trainSet)
lm.fit2
summary(lm.fit2)

lm.fit3<-lm(income~(prestige+women), data=trainSet)
lm.fit3
summary(lm.fit3)


predict1<-predict(lm.fit2, newdata = testSet)
predict1_df<-as.data.frame(predict1)


cat("\014")
predict2<-predict(lm.fit3, newdata = testSet)
predict2_df<-as.data.frame(predict2)

names<-row.names(testSet)
lm._df<-data.frame(ActualIncome=testSet$income, 
                  PredictedIncome_1=predict1_df$predict1, PredictedIncome_2=predict2_df$predict2)

lm._df

library(tidyverse)
library(ggcorrplot)
wine<-read.csv("wine.csv")
str(wine)
summary(wine)
model1<-lm(Price~AGST,data=wine)
summary(model1)
model1$residuals
SSE1<-sum(model1$residuals^2)
SSE1
model2<-lm(Price~AGST + HarvestRain,data=wine)
model2
summary(model2)
SSE2<-sum(model2$residuals^2)
SSE2
model3<-lm(Price~AGST + HarvestRain +WinterRain +Age +FrancePop,data=wine)
model3
summary(model3)
SSE3<-sum(model3$residuals^2)
SSE3
model3<-lm(Price~HarvestRain +WinterRain,data=wine)
summary(model3)

ggplot(wine,aes(x=FrancePop,y=Age)) + geom_point()+geom_smooth(method="lm",se=FALSE)
cor(wine$FrancePop,wine$Age)
ans<-cor(wine)

model4<-lm(Price~AGST + HarvestRain +WinterRain +Age,data=wine)
model4
summary(model4)
SSE4<-sum(model4$residuals^2)
SSE4

wine_test<-read.csv("Wine_test.csv")
str(wine_test)
predict_price<-predict(model4,newdata = wine_test)
predict_price
SSE<-sum((wine_test$Price-predict_price)^2)
SST<-sum((wine_test$Price-mean(wine$Price))^2)
Rsq<- 1- SSE/SST
Rsq
ggcorrplot(ans)
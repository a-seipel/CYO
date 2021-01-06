
#load packages and libraries 
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")

library(readxl)
library(tidyverse)
library(gridExtra)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(caret)
library(psych)

#getwd()
#setwd("C:/Users/Aljoscha/Documents/R/homework-0/ownproject")

crimedata <- read_excel("Boston Crime.xlsx")

#substituting "Y" (Yes) by 1 (Yes)
crimedata$SHOOTING[crimedata$SHOOTING=="Y"] <- 1
crimedata$SHOOTING[is.na(crimedata$SHOOTING)==T] <- 0
crimedata$SHOOTING <- as.numeric(crimedata$SHOOTING)

#Exploration

head(crimedata)
names(crimedata)
nrow(crimedata)
ncol(crimedata)
unique(crimedata$OFFENSE_DESCRIPTION)

min(crimedata$OCCURRED_ON_DATE)
max(crimedata$OCCURRED_ON_DATE)


#TIME ANALYSIS

time <- seq.Date(from=as.Date("2015-06-15"),to=as.Date("2020-07-30"),by="days")
time <- as.data.frame(time)

crimedata <- crimedata%>%mutate(time=.$OCCURRED_ON_DATE)
crimedata2 <- time %>% left_join(crimedata,by="time")
crimedata2 <- crimedata2%>%group_by(time)%>%summarise(n=n())

#plotting time series (daily)
crimedata2 %>% ggplot(aes(time,n))+
  geom_line()+
  geom_vline(xintercept = as.numeric(crimedata2$time[1570]),color="red")

#
weeklyT <- crimedata %>% group_by(DAY_OF_WEEK) %>% 
  summarise(n=n())%>%.$n%>%
  ts(start=c(2015,6),end=c(2020,7),frequency = 7)%>%
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

monthlyT <- crimedata %>%  group_by(MONTH)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2020,1),end=c(2020,12),frequency = 12)%>% 
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

weeklyY <- crimedata %>%  group_by(YEAR,DAY_OF_WEEK)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2015,6),end=c(2020,7),frequency = 7)%>% 
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

monthlyY <- crimedata %>%  group_by(YEAR,MONTH)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2015,6),end=c(2020,7),frequency = 12)%>% 
  ggseasonplot()+
  ggtitle("")+
  geom_line()

hourlyT <- crimedata %>% 
  group_by(HOUR)%>%
  summarise(n=n())%>% 
  ggplot(aes(HOUR,n))+
  geom_line()

hourlyY <- crimedata %>% 
  group_by(YEAR,HOUR)%>%
  summarise(n=n())%>% 
  ggplot(aes(HOUR,n,group=YEAR,color=YEAR))+
  geom_line()

#plot results
grid.arrange(weeklyT,monthlyT,weeklyY,monthlyY,hourlyT,hourlyY)


#SHOOTINGS and time
###
weeklyT_shots <- crimedata %>% 
  filter(SHOOTING==1)%>%
  group_by(DAY_OF_WEEK) %>% 
  summarise(n=n())%>%.$n%>%
  ts(start=c(2015,6),end=c(2020,7),frequency = 7)%>%
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

monthlyT_shots <- crimedata %>%  
  filter(SHOOTING==1)%>%
  group_by(MONTH)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2020,1),end=c(2020,12),frequency = 12)%>% 
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

weeklyY_shots <- crimedata %>%  
  filter(SHOOTING==1)%>%
  group_by(YEAR,DAY_OF_WEEK)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2015,6),end=c(2020,7),frequency = 7)%>% 
  ggseasonplot()+
  geom_line()+
  ggtitle("")+
  theme(legend.position = "none")

monthlyY_shots <- crimedata %>%  
  filter(SHOOTING==1)%>%
  group_by(YEAR,MONTH)%>%
  summarise(n=n())%>%.$n%>% 
  ts(start=c(2015,6),end=c(2020,7),frequency = 12)%>% 
  ggseasonplot()+
  ggtitle("")+
  geom_line()

hourlyT_shots <- crimedata %>% 
  filter(SHOOTING==1)%>%
  group_by(HOUR)%>%
  summarise(n=n())%>% 
  ggplot(aes(HOUR,n))+
  geom_line()

hourlyY_shots <- crimedata %>% 
  filter(SHOOTING==1)%>%
  group_by(YEAR,HOUR)%>%
  summarise(n=n())%>% 
  ggplot(aes(HOUR,n,group=YEAR,color=YEAR))+
  geom_line()

#plot results
grid.arrange(weeklyT_shots,monthlyT_shots,weeklyY_shots,monthlyY_shots,hourlyT_shots,hourlyY_shots)


#######################
##geographical location

 #crimescenes - coordinates

#per year
crime_pYD <- crimedata %>% group_by(YEAR,DISTRICT) %>% summarise(n=n())
cpy <- crime_pYD %>% ggplot(aes(YEAR,n,color=DISTRICT))+
  geom_line()
scpy <- crimedata%>% 
  filter(SHOOTING==1)%>%
  group_by(YEAR,DISTRICT) %>% 
  summarise(n=n())%>%
  ggplot(aes(YEAR,n,color=DISTRICT))+
  geom_line()
grid.arrange(cpy,scpy,nrow=1)

#CRIMES RELATED TO SHOOTINGS
#crime involving shooting

crimedata %>% 
  filter(SHOOTING==1)%>%
  summarise(n=n(),p=n/nrow(crimedata))
#total=2570

shootingstotal <- crimedata %>% 
  filter(SHOOTING==1 & is.na(OFFENSE_CODE_GROUP)==F)%>%
  group_by(OFFENSE_CODE_GROUP)%>%
  summarise(n=n(),p=n/2570)%>%
  arrange(desc(n))
#only first five over 5 %

mainshootingcrimes <- shootingstotal %>% top_n(n=5)

shootings_geo <- crimedata %>% 
  filter(OFFENSE_CODE_GROUP %in% mainshootingcrimes$OFFENSE_CODE_GROUP & is.na(OFFENSE_CODE_GROUP)==F)%>%
  filter(SHOOTING==1 & Lat>0 & Long < 0 & is.na(Lat)==F & is.na(Long)==F)%>%
  select(Lat,Long,OFFENSE_CODE_GROUP)%>%
  arrange(desc(Lat))


mainshootingcrimes
shootings_geo %>% ggplot(aes(Lat,Long,color=OFFENSE_CODE_GROUP))+
  geom_point()

##########################################
#knn

crime <- crimedata %>% 
  select(OFFENSE_CODE,Long,Lat,HOUR,YEAR,SHOOTING)%>%
  filter(is.na(Long)==F & is.na(Lat)==F & Lat > 40 & Long < -60) 

set.seed(10)
index <- createDataPartition(crime$Lat,p=0.9,times=1,list=F)

train <- crime[index,]
test <- crime[-index,]

#training of models 
set.seed(10)

train_knn <- train(SHOOTING ~ ., method = "knn", 
                   data = train,
                   tuneGrid = data.frame(k = seq(5, 15, 2)))


set.seed(10)

knn_fit1 <- knn3(SHOOTING~.,data=train,k=1)
knn_fit2 <- knn3(SHOOTING~.,data=train,k=2)
knn_fit3 <- knn3(SHOOTING~.,data=train,k=3)
knn_fit4 <- knn3(SHOOTING~.,data=train,k=4)
knn_fit5 <- knn3(SHOOTING~.,data=train,k=5)
knn_fit6 <- knn3(SHOOTING~.,data=train,k=6)
knn_fit7 <- knn3(SHOOTING~.,data=train,k=7)
knn_fit8 <- knn3(SHOOTING~.,data=train,k=8)
knn_fit9 <- knn3(SHOOTING~.,data=train,k=9)
knn_fit10 <- knn3(SHOOTING~.,data=train,k=10)


#prediction of models 
#View(knn_fit$learn$X)

knn_pred1 <- predict(knn_fit1,test,type="prob")
knn_pred2 <- predict(knn_fit2,test,type="prob")
knn_pred3 <- predict(knn_fit3,test,type="prob")
knn_pred4 <- predict(knn_fit4,test,type="prob")
knn_pred5 <- predict(knn_fit5,test,type="prob")
knn_pred6 <- predict(knn_fit6,test,type="prob")
knn_pred7 <- predict(knn_fit7,test,type="prob")
knn_pred8 <- predict(knn_fit8,test,type="prob")
knn_pred9 <- predict(knn_fit9,test,type="prob")
knn_pred10 <- predict(knn_fit10,test,type="prob")

#conversion of prob values to binary
knn_pred1[knn_pred1[,2]>0.5,] <- 1
knn_pred1[knn_pred1[,2]<=0.5,] <- 0
knn_pred1 <- knn_pred1[,-1]

knn_pred2[knn_pred2[,2]>0.5,] <- 1
knn_pred2[knn_pred2[,2]<=0.5,] <- 0
knn_pred2 <- knn_pred2[,-1]

knn_pred3[knn_pred3[,2]>0.5,] <- 1
knn_pred3[knn_pred3[,2]<=0.5,] <- 0
knn_pred3 <- knn_pred3[,-1]

knn_pred4[knn_pred4[,2]>0.5,] <- 1
knn_pred4[knn_pred4[,2]<=0.5,] <- 0
knn_pred4 <- knn_pred4[,-1]

knn_pred5[knn_pred5[,2]>0.5,] <- 1
knn_pred5[knn_pred5[,2]<=0.5,] <- 0
knn_pred5 <- knn_pred5[,-1]

knn_pred6[knn_pred6[,2]>0.5,] <- 1
knn_pred6[knn_pred6[,2]<=0.5,] <- 0
knn_pred6 <- knn_pred6[,-1]

knn_pred7[knn_pred7[,2]>0.5,] <- 1
knn_pred7[knn_pred7[,2]<=0.5,] <- 0
knn_pred7 <- knn_pred7[,-1]

knn_pred8[knn_pred8[,2]>0.5,] <- 1
knn_pred8[knn_pred8[,2]<=0.5,] <- 0
knn_pred8 <- knn_pred8[,-1]

knn_pred9[knn_pred9[,2]>0.5,] <- 1
knn_pred9[knn_pred9[,2]<=0.5,] <- 0
knn_pred9 <- knn_pred9[,-1]

knn_pred10[knn_pred10[,2]>0.5,] <- 1
knn_pred10[knn_pred10[,2]<=0.5,] <- 0
knn_pred10 <- knn_pred10[,-1]

#confusion matrices
u <- union(knn_pred1, test$SHOOTING)
t <- table(factor(knn_pred1, u), factor(test$SHOOTING, u))
res1 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred2, test$SHOOTING)
t <- table(factor(knn_pred2, u), factor(test$SHOOTING, u))
res2 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred3, test$SHOOTING)
t <- table(factor(knn_pred3, u), factor(test$SHOOTING, u))
res3 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred4, test$SHOOTING)
t <- table(factor(knn_pred4, u), factor(test$SHOOTING, u))
res4 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred5, test$SHOOTING)
t <- table(factor(knn_pred5, u), factor(test$SHOOTING, u))
res5 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred6, test$SHOOTING)
t <- table(factor(knn_pred6, u), factor(test$SHOOTING, u))
res6 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred7, test$SHOOTING)
t <- table(factor(knn_pred7, u), factor(test$SHOOTING, u))
res7 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred8, test$SHOOTING)
t <- table(factor(knn_pred8, u), factor(test$SHOOTING, u))
res8 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred9, test$SHOOTING)
t <- table(factor(knn_pred9, u), factor(test$SHOOTING, u))
res9 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])

u <- union(knn_pred10, test$SHOOTING)
t <- table(factor(knn_pred10, u), factor(test$SHOOTING, u))
res10 <- c(confusionMatrix(t)$overall["Accuracy"],confusionMatrix(t)$byClass["Balanced Accuracy"], confusionMatrix(t)$overall["McnemarPValue"],confusionMatrix(t)$byClass["Specificity"])


#visualization of accuracy

accresult <- data.frame()
accresult <- rbind(accresult,res1,res2,res3,res4,res5,res6,res7,res8,res9,res10)
names(accresult) <- c("accuracy","balanced","McNemar","specificity")
accresult <- cbind(accresult,k=c(1,2,3,4,5,6,7,8,9,10))

#plot the results

p1 <- accresult %>% ggplot(aes(k,accuracy))+
  geom_line()+
  geom_point(aes(k[2],accuracy[2]))
p2 <- accresult %>% ggplot(aes(k,balanced))+
  geom_line()+
  geom_point(aes(k[2],balanced[2]))
grid.arrange(p1, p2, ncol = 2)


#final model

u <- union(knn_pred2, test$SHOOTING)
t <- table(factor(knn_pred2, u), factor(test$SHOOTING, u))
finalm <- confusionMatrix(t)
finalm$byClass

finalm

#plot specificity 
accresult %>% ggplot(aes(k,specificity))+
  geom_line()







setwd("S://R Studio Study//Projects//Kaggle//Spaceship Titanic")

Train=read.csv("train.csv",stringsAsFactors = F)
Test=read.csv("test.csv",stringsAsFactors = F)

Test1=Test
Train1=Train

Test1$Transported=NA
Test1$Data='Test'
Train1$Data='Train'

All=rbind(Train1,Test1)

library(dplyr)

glimpse(All)

library(ggplot2)

ggplot(All,aes(x=HomePlanet))+geom_bar()

ggplot(All,aes(x=VIP))+geom_bar()

ggplot(All,aes(x=HomePlanet))+geom_bar()

# HomePlanet:

unique(All$HomePlanet)

round(prop.table(table(All$HomePlanet,All$Transported),1),2)

sum(is.na(All$HomePlanet))

POS5=which(All$HomePlanet=="")

table(All$HomePlanet)

#Replacing blank with 'Earth'

All$HomePlanet[POS5]='Earth'

round(prop.table(table(All$HomePlanet,All$Transported),1),2)

All=All%>%
  mutate(HMP_Earth=as.numeric(HomePlanet=='Earth'),
         HMP_Europa=as.numeric(HomePlanet=='Europa'),
         HMP_Mars=as.numeric(HomePlanet=='Mars'))

All=All%>%select(-HomePlanet)

# CryoSleep

unique(All$CryoSleep)

round(prop.table(table(All$CryoSleep,All$Transported),1),2)

All=All%>%
  mutate(CryoSleep_T=as.numeric(CryoSleep=='True'))

All=All%>%select(-CryoSleep)

# Destination

unique(All$Destination)

round(prop.table(table(All$Destination,All$Transported),1),2)

table(All$Destination) #Trappist is occuring max times

POS6=which(All$Destination=="")

All$Destination[POS6]="TRAPPIST-1e"

All=All%>%
  mutate(Dest_TRAPP=as.numeric(Destination=='TRAPPIST-1e'),
         Dest_PSO=as.numeric(Destination=='PSO J318.5-22'),
         Dest_55_Cancri=as.numeric(Destination=='55 Cancri e'))

All=All%>%select(-Destination)

# VIP

unique(All$VIP)

All=All%>%mutate(VIP_T=as.numeric(VIP=='True'))

All=All%>%select(-VIP)

#cabin

unique(All$Cabin)

All=All%>%select(-Cabin)

glimpse(All)

#Age

unique(All$Age)

summary(All$Age)

hist(All$Age)

Pos=which(is.na(All$Age))

All$Age[Pos]=29

#RoomService

unique(All$RoomService)

summary(All$RoomService)

hist(All$RoomService)

Pos=which(is.na(All$RoomService))

All$RoomService[Pos]=0

# outlier test

boxplot(x=All$RoomService,horizontal = T)

#FoodCourt

sum(is.na(All$FoodCourt))

summary(All$FoodCourt)

hist(All$FoodCourt)

boxplot(x=All$FoodCourt,horizontal = T)

Pos=which(is.na(All$FoodCourt))

All$FoodCourt[Pos]=0

#Shopping mall:

unique(All$ShoppingMall)

sum(is.na(All$ShoppingMall))

boxplot(x=All$ShoppingMall,horizontal = T)

hist(All$ShoppingMall)


#Removing Nas:

summary(All$ShoppingMall)

pos3=which(is.na(All$ShoppingMall))

All$ShoppingMall[pos3]=0


#Spa

hist(All$Spa)

Pos=which(is.na(All$Spa))

All$Spa[Pos]=0

summary(All$Spa)

boxplot(x=All$Spa,horizontal = T)

glimpse(All)

All=All%>%select(-Name)

All=All%>%select(-PassengerId)

#VR Deck

unique(All$VRDeck)

pos4=which(is.na(All$VRDeck))

summary(All$VRDeck)

hist(All$VRDeck)

All$VRDeck[pos4]=0

sum(is.na(All$VRDeck))
glimpse(All)

#Transported

All=All%>%
  mutate(Transported=as.numeric(Transported=='True'))


sum(is.na(All))

#separating Test and Train Data

Test2=All%>%filter(Data=='Test')
Test2=Test2%>%select(-Transported)
Train2=All%>%filter(Data=='Train')

Test2=Test2%>%select(-Data)
Train2=Train2%>%select(-Data)

sum(is.na(Train2)) #0 NA
sum(is.na(Test2)) #0 NA

glimpse(Train2)

sum(is.na(Test2))

set.seed(2)
s=sample(1:nrow(Train2), 0.8*nrow(Train2))

Train3=Train2[s,]
Test3=Train2[-s,]

library(car)

for_vif=lm(Transported~.-HMP_Mars-Dest_55_Cancri,data = Train3)

#Correlation Matrix

correlation=cor(Train3)

barchart(correlation) #Visualizing the correlation with the Response variable

sort(vif(for_vif),decreasing =  T)

alias(for_vif)

#Multi colinearity has been eliminated

library(gbm)
library(cvTools)

paramsGBM=list(interaction.depth=c(1:5),
               n.trees=c(100,500,700,200),
               shrinkage=c(0.1,.01,.001),
               n.minobsinnode=c(5,10,15))


Expanded.DBMParam=expand.grid(paramsGBM)


G=sample(1:nrow(Expanded.DBMParam),36)

subset3=Expanded.DBMParam[G,]

#Finding best parameters:

mycost_GBMauc=function(Transported,Transportedhat){
  roccurve=pROC::roc(Transported,Transportedhat)
  score=pROC::auc(roccurve)
  
  return(score)}

myauc_GBM=0

for(i in 1:36){
  
  print(paste0("iteration",i))
  
  
  parameters=subset3[i,]
  
  k=cvTuning(gbm,Transported~.-HMP_Mars-Dest_55_Cancri,data = Train3,
             
             tuning = parameters,
             
             args = list(distribution='bernoulli'),
             
             folds=cvFolds(nrow(Train3),K=10,type = 'random'),
             
             cost=mycost_GBMauc, seed = 2,
             
             predictArgs = list(type='response',n.trees=parameters$n.trees)
  )
  score.this=k$cv[,2]
  if(score.this>myauc_GBM)
    
  {
    myauc_GBM=score.this
    
    best_params=parameters
    print(best_params)}}

#Auc=87.549, Interaction.depth=4, n.trees=200,shrinkage=0.1,n.minobsinnode=10

Model=gbm(Transported~.-HMP_Mars-Dest_55_Cancri,data = Train3,
          interaction.depth=4,n.trees=200,shrinkage=0.1,n.minobsinnode = 10,distribution = 'bernoulli')

In_Sample=predict(Model,newdata = Train3,type = 'response')

roc(Train3$Transported,In_Sample) #Auc=89.89

OutSample=predict(Model,newdata = Test3,type = 'response')

roc(Test3$Transported,OutSample) #Auc=87.9

#Model on entire training set

GBMModel=gbm(Transported~.-HMP_Mars-Dest_55_Cancri,data = Train2,
             interaction.depth=4,n.trees=200,shrinkage=0.1,n.minobsinnode = 10,distribution = 'bernoulli')

In_SampleGBM=predict(GBMModel,newdata = Train2,type = 'response')

roc(Train2$Transported,In_SampleGBM) #Auc=89.74




# Have taken the cutoff as 0.5

Transported=ifelse(OutSampleGBM>0.5,"TRUE","FALSE") #Final Prediction

length(OutSampleGBM)

Solution=cbind(Test$PassengerId,Transported)

colnames(Solution)=c("PassengerId","Transported")

write.csv(Solution,"Passengers_GBM.csv",row.names = F)


#Determining cutoff using KS

Cutoff=round(seq(0,1,length=100),3)

Cutoff_Data=data.frame(Cutoff=0,TP=0,TN=0,FP=0,FN=0)

for (i in Cutoff) {
  
  Predicted=as.numeric(In_SampleGBM>i)
  
  TP=sum(Train2$Transported==1&Predicted==1)
  TN=sum(Train2$Transported==0&Predicted==0)
  FP=sum(Train2$Transported==0&Predicted==1)
  FN=sum(Train2$Transported==1&Predicted==0)
  
  
  Cutoff_Data=rbind(Cutoff_Data,c(i,TP,TN,FP,FN))
}

Cutoff_Data=Cutoff_Data[-1,] 

Cutoff_Data=Cutoff_Data%>%
  mutate(P=TP+FN,
         N=TN+FP,
         KS=abs((TP/P)-(FP/N)))

W=which.max(Cutoff_Data$KS) #max KS is 0.628, cutoff=0.556

OutSampleGBM=predict(GBMModel,newdata = Test2,type = 'response')

Transported_KS=ifelse(OutSampleGBM>0.556,"TRUE","FALSE")

Solution=cbind(Test$PassengerId,Transported_KS)

colnames(Solution)=c("PassengerId","Transported")

write.csv(Solution,"Passengers_GBM_KS.csv",row.names = F)


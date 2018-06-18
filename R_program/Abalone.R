
# EXERCISE 1

df<-read.csv("abalone.data.csv",header=TRUE)

degree<-c(1,2,3)
cost<-c(0.1,1,10,100)
result<-data.frame("Degree"=double(),"Cost"=double(),"Accuracy"=double())
bestD<-0
bestC<-0
bestAccuracy<-0
count<-1
for(i in 1:length(degree))
{
  for(j in length(cost):1)
  {
    model<-svm(Rings~.,df,kernel='polynomial',degree=degree[i],type='C-classification',cost=cost[j],cross=5)
    currAccuracy<-model$tot.accuracy
    result[count,]<-list(degree[i],cost[j],currAccuracy)
    count<-count+1
    if(currAccuracy>bestAccuracy)
    {
      bestD<-degree[i]
      bestC<-cost[j]
      bestAccuracy<-currAccuracy
    }
  }
}
df1<-subset(df,select=Rings)
df2<-subset(df,select=-Rings)
myModel<-svm(Rings~.,df,kernel='polynomial',degree=bestD,type='C-classification',cost=bestC,cross=5)
pred<-as.integer(as.vector(predict(myModel,df2)))
sum<-0
Difference<-vector()
for(i in 1:nrow(df))
{
  diff<-abs(pred[i]-df1[i,])
  sum<-sum+diff
  Difference<-rbind(Difference,diff)
}
print(result)
average<-sum/nrow(df)
print(sprintf("Average distance=%f",average))
hist(Difference,breaks=max(as.integer(unique(df$Rings)))+1,labels=TRUE,freq=TRUE,ylim=c(0,max(histinfo$counts)+100))

# EXERCISE 2

best_SVM<-function(df)
{
  d<-c(1,2,3)
  c<-c(0.1,1,10,100)
  bestD<-0
  bestC<-0
  bestAccuracy<-0
  for(i in 1:length(d))
  {
    for(j in 1:length(c))
    {
      model<-svm(Rings~.,df,kernel='polynomial',degree=d[i],type='C-classification',cost=c[j],cross=5)
      currAccuracy<-model$tot.accuracy
      if(currAccuracy>bestAccuracy)
      {
        bestD<-d[i]
        bestC<-c[j]
        bestAccuracy<-currAccuracy
      }
    }
  }
  myModel<-svm(Rings~.,df,kernel='polynomial',degree=bestD,type='C-classification',cost=bestC,cross=5)
  pred<-(sum(df$Rings==fitted(myModel))/nrow(df))*100
  res<-list(bestD,bestC,bestAccuracy,pred,myModel)
  return(res)
}

result<-data.frame("Description"=character(),"Size"=double(),"Degree"=double(),"Cost"=double(),"AverageAcc"=double(),"TrainingAcc"=double(),stringsAsFactors=FALSE)
rowLen<-nrow(df)
colLen<-ncol(df)
df9Vs10<-subset(df,Rings<=9 | Rings>=10)
for(i in 1:nrow(df9Vs10))
{
  if(df9Vs10[i,colLen]<=9)
    df9Vs10[i,colLen]=-1
  else
    df9Vs10[i,colLen]=+1
}
r<-best_SVM(df9Vs10)
model9Vs10<-r[[5]]
result[1,]<-c("9Vs10",nrow(df9Vs10),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df7Vs89<-subset(df,Rings<=7 | Rings==8 | Rings==9)
for(i in 1:nrow(df7Vs89))
{
  if(df7Vs89[i,colLen]<=7)
    df7Vs89[i,colLen]=-1
  else
    df7Vs89[i,colLen]=+1
}
r<-best_SVM(df7Vs89)
model7Vs89<-r[[5]]
result[2,]<-c("7Vs8-9",nrow(df7Vs89),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df5Vs67<-subset(df,Rings<=5 | Rings==6 | Rings==7)
for(i in 1:nrow(df5Vs67))
{
  if(df5Vs67[i,colLen]<=5)
    df5Vs67[i,colLen]=-1
  else
    df5Vs67[i,colLen]=+1
}
r<-best_SVM(df5Vs67)
model5Vs67<-r[[5]]
result[3,]<-c("5Vs6-7",nrow(df5Vs67),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df8Vs9<-subset(df,Rings==8 | Rings==9)
for(i in 1:nrow(df8Vs9))
{
  if(df8Vs9[i,colLen]==8)
    df8Vs9[i,colLen]=-1
  else
    df8Vs9[i,colLen]=+1
}
r<-best_SVM(df8Vs9)
model8Vs9<-r[[5]]
result[4,]<-c("8Vs9",nrow(df8Vs9),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df6Vs7<-subset(df,Rings==6 | Rings==7)
for(i in 1:nrow(df6Vs7))
{
  if(df6Vs7[i,colLen]==6)
    df6Vs7[i,colLen]=-1
  else
    df6Vs7[i,colLen]=+1
}
r<-best_SVM(df6Vs7)
model6Vs7<-r[[5]]
result[5,]<-c("6Vs7",nrow(df6Vs7),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df1011Vs12<-subset(df,Rings==10 | Rings==11 | Rings>=12)
for(i in 1:nrow(df1011Vs12))
{
  if(df1011Vs12[i,colLen]>=12)
    df1011Vs12[i,colLen]=+1
  else
    df1011Vs12[i,colLen]=-1
}
r<-best_SVM(df1011Vs12)
model1011Vs12<-r[[5]]
result[6,]<-c("10-11Vs12",nrow(df1011Vs12),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df1213Vs14<-subset(df,Rings==12 | Rings==13 | Rings>=14)
for(i in 1:nrow(df1213Vs14))
{
  if(df1213Vs14[i,colLen]>=14)
    df1213Vs14[i,colLen]=+1
  else
    df1213Vs14[i,colLen]=-1
}
r<-best_SVM(df1213Vs14)
model1213Vs14<-r[[5]]
result[7,]<-c("12-13Vs14",nrow(df1213Vs14),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df10Vs11<-subset(df,Rings==10 | Rings==11)
for(i in 1:nrow(df10Vs11))
{
  if(df10Vs11[i,colLen]==10)
    df10Vs11[i,colLen]=-1
  else
    df10Vs11[i,colLen]=+1
}
r<-best_SVM(df10Vs11)
model10Vs11<-r[[5]]
result[8,]<-c("10Vs11",nrow(df10Vs11),list(r[[1]],r[[2]],r[[3]],r[[4]]))
df12Vs13<-subset(df,Rings==12 | Rings==13)
for(i in 1:nrow(df12Vs13))
{
  if(df12Vs13[i,colLen]==12)
    df12Vs13[i,colLen]=-1
  else
    df12Vs13[i,colLen]=+1
}
r<-best_SVM(df12Vs13)
model12Vs13<-r[[5]]
result[9,]<-c("12Vs13",nrow(df12Vs13),list(r[[1]],r[[2]],r[[3]],r[[4]]))
print(result)

# EXERCISE 3

df1<-subset(df,select=Rings)
df2<-subset(df,select=-Rings)
Difference<-vector()
sum<-0
count<-0
for(i in 1:rowLen)
{
  if(as.integer(as.vector(predict(model9Vs10,df2[i,]))==-1))
  {
    if(as.integer(as.vector(predict(model7Vs89,df2[i,]))==-1))
    {
      if(as.integer(as.vector(predict(model5Vs67,df2[i,]))==-1))
        value<-5
      else
      {
        if(as.integer(as.vector(predict(model6Vs7,df2[i,]))==-1))
          value<-6
        else
          value<-7
      }
    }
    else
    {
      if(as.integer(as.vector(predict(model8Vs9,df2[i,]))==-1))
        value<-8
      else
        value<-9
    }
  }
  else
  {
    if(as.integer(as.vector(predict(model1011Vs12,df2[i,]))==-1))
    {
      if(as.integer(as.vector(predict(model10Vs11,df2[i,]))==-1))
        value<-10
      else
        value<-11
    }
    else
    {
      if(as.integer(as.vector(predict(model1213Vs14,df2[i,]))==-1))
      {
        if(as.integer(as.vector(predict(model12Vs13,df2[i,]))==-1))
          value<-12
        else
          value<-13
      }
      else
        value<-14
    }
  }
  diff<-abs(value-df1[i,])
  if(diff==0)
    count<-count+1
  sum<-sum+diff
  Difference<-rbind(Difference,diff)
}
Accuracy<-(count/rowLen)*100
average<-(sum/rowLen)
print(sprintf("Training Accuracy=%f Average distance=%f",Accuracy,average))
hist(Difference,breaks=max(as.integer(unique(df$Rings)))+1,labels=TRUE,freq=TRUE,ylim=c(0,max(histinfo$counts)+50))

# EXERCISE 4

df<-read.csv("Exercise-4.data.csv",header=TRUE)

plot(df,col="red",pch=16)

e<-c(0.1,0.3,0.75,1,1.3)
c<-c(0.1,1.0,10.0,100.0)
result<-data.frame("Epsilon"=double(),"Cost"=double(),"MSE"=double())
count<-1
bestE<-0
bestC<-0
bestMSE<-9999
for(i in length(e):1)
{
  for(j in length(c):1)
  {
    model<-svm(Y~X,df,kernel='polynomial',degree=2,type='eps-regression',cost=c[j],cross=10,epsilon=e[i])
    pred<-predict(model,df)
    error<-(df$Y-pred)
    mse<-mean(error^2)
    result[count,]<-c(e[i],c[j],mse)
    count<-count+1
    if(mse<bestMSE)
    {
      bestE<-e[i]
      bestC<-c[j]
      bestMSE<-mse
    }
  }
}
print(result)

# EXERCISE 5

myModel<-svm(Y~X,df,kernel='polynomial',degree=2,type='eps-regression',cost=bestC,cross=10,epsilon=bestE)
predY<-predict(myModel,df)
points(df$X,predY,col="red",pch=20)

myVectorX<-seq(from=0.01,to=10,by=0.01)
myDf<-as.data.frame(myVectorX)
colnames(myDf)<-c("X")
predY<-predict(myModel,myDf)
points(myVectorX,predY,col="blue")

# EXERCISE 6

df<-read.csv("abalone.data.csv",header=TRUE)

degree<-c(1,2,3)
cost<-c(0.1,1,10,100)
epsilon<-c(0.1,0.3,0.75,1,1.3)
best_d<-0
best_c<-0
best_e<-0
best_mse<-9999
for(i in 1:length(degree))
{
  for(j in 1:length(cost))
  {
    for(k in 1:length(epsilon))
    {
      model<-svm(Rings~.,df,kernel='polynomial',degree=degree[i],type='eps-regression',cost=cost[j],cross=10,epsilon=epsilon[k])
      mse<-model$tot.MSE
      if(mse<best_mse)
      {
        best_d<-d[i]
        best_c<-c[j]
        best_e<-e[k]
        best_mse<-mse
      }
    }
  }
}
df1<-subset(df,select=-Rings)
df2<-subset(df,select=Rings)
bestModel<-model<-svm(Rings~.,df,kernel='polynomial',degree=best_d,type='eps-regression',cost=best_c,cross=10,epsilon=best_e)
pred<-as.integer(as.vector(predict(bestModel,df1)))
sum<-0
Difference<-vector()
count<-0
for(i in 1:nrow(df))
{
  diff<-abs(pred[i]-df2[i,])
  if(diff==0)
    count<-count+1
  sum<-sum+diff
  Difference<-rbind(Difference,diff)
}
average<-sum/nrow(df)
print(sprintf("Average distance=%f",average))
hist(Difference,breaks=max(as.integer(unique(df$Rings)))+1,labels=TRUE,freq=TRUE,ylim=c(0,max(histinfo$counts)+100))

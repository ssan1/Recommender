# Programming - Sanchita S

# Exercise 1
best_accuracy <- function(df){
  
  l <- nrow(df)
  d <- c(1,2,3) 
  a = NULL
  # c <- sample(0.1:100, replace=T, 5) 
  co <- c(0.1,1,10,100)
  
  best_acc = 0
  best_d =0
  best_c = 0
  
  l <- nrow(df)
  
  for(i in d) {
    for(j in co) {
      
      mymodelK <- svm(Rings~. , data =df , kernel = 'polynomial', type='C-classification',degree = i, cost = j, cross =5)
      
      accu = mymodelK$tot.accuracy
      
      if(accu > best_acc)
      {
        best_d = i
        best_c = j
        best_acc = accu
      }
      
      a = rbind(a, data.frame(i, j, accu))
      
    }
  }
  
  sort_accu <- a[order(a$accu),]
  
  print(sort_accu)
  x <- subset(df, select = -Rings)
  y <- subset(df,select=Rings)
  
  model <- svm(Rings~. , data =df,type='C-classification',degree = best_d, cost = best_c, kernel= 'polynomial', cross =5)
  
  pred_model <- as.integer(as.vector(predict(model,x)))
  
  
        h_vec <- vector()
        distance<-0
        
        h_vec <- vector()
        for(j in 1:nrow(df))
          
        {
          dist  <- abs(y[j,]-pred_model[j])
          h_vec <- rbind(h_vec,dist)
          distance <- dist + distance
        }
        
        
      ave_dist = (mean(distance)/l)
      print(ave_dist)
      
      hist(h_vec) #creates histogram
      
}



# ________________ End of Exercise 1


# Exercise 2

binfunction<- function(dfbin, d, c) {
  
  #initilizing parameters
  dval = 0
  cval = 0
  
  acc=0
  accuracy = 0
  
  mean_acc=0
  T_acc=0
  count=0
  
  for(i in d){
    for(j in c){
      
      mymodel=svm(Rings ~ ., data = dfbin, kernel="polynomial", degree = i,type = "C-classification",cost=j,cross=5)
      mymodel$accuracies
      
      acc=mymodel$tot.accuracy
      
      T_acc = T_acc + acc
      
      count=count+1
      if(acc > accuracy)
      {
        accuracy = acc
        dval=i
        cval=j
      }
    }
  }
  mean_acc = T_acc/count  #calculating mean value
  
  out_final = list(nrow(dfbin),dval,cval,accuracy,mean_acc)
  
  return(out_final)
}

      bin <- function(dfbin,lw,hg)
      {
        
        count1 = ncol(dfbin)
        for (i in 1:nrow(dfbin))
        { 
          if(dfbin[i,count1] <= lw)
            dfbin[i,count1]=0
          if(dfbin[i,count1] >= hg)
            dfbin[i,count1]=1
        }
        return(dfbin)
      }



binary_classifier <- function(df)
{
  output <- matrix(ncol=5, nrow=9)
  df9_10 =subset(df,Rings<=9|Rings>=10)
  df9_10 =bin(df9_10 ,9,10)
  output[1,]=unlist(binfunction(df9_10,d,c))
  
  df7_8_9 = subset(df, Rings <= 7 | Rings ==8 | Rings ==9)
  df7_8_9=bin(df7_8_9,7,8)
  output[2,]=unlist(binfunction(df7_8_9,d,c))
  
  df5_6_7 = subset(df, Rings <= 5 | Rings ==6 | Rings ==7)
  df5_6_7=bin(df5_6_7,5,6)
  output[3,]=unlist(binfunction(df5_6_7,d,c))
  
  df8_9 = subset(df, Rings ==8 | Rings ==9)
  df8_9=bin(df8_9,8,9)
  output[4,]=unlist(binfunction(df8_9,d,c))
  
  df6_7 = subset(df, Rings ==6 | Rings ==7)
  df6_7=bin(df6_7,6,7)
  output[5,]=unlist(binfunction(df6_7,d,c))
  
  df10_11_12 = subset(df, Rings == 10 | Rings ==11 | Rings >=12)
  df10_11_12=bin(df10_11_12,11,12)
  output[6,]=unlist(binfunction(df10_11_12,d,c))
  
  df12_13_14 = subset(df, Rings ==12 | Rings ==13 | Rings >=14)
  df12_13_14=bin(df12_13_14,13,14)
  output[7,]=unlist(binfunction(df12_13_14,d,c))
  
  f10_11 = subset(df, Rings ==10 | Rings ==11)
  f10_11=bin(f10_11,10,11)
  output[8,]=unlist(binfunction(f10_11,d,c))
  
  df12_13 = subset(df, Rings ==12 | Rings ==13)
  df12_13=bin(df12_13,12,13)
  final_val[9,]=unlist(binfunction(df12_13,d,c))
  return(final_val)
}



# End of Exercise 2


# ______________Exercise 4 and 5

alldata.df <- read.csv("Ex4_5_data.csv")
plot(alldata.df)

best_accuracy <- function(df){
  
  l <- nrow(df)
  
  d <- c(1,2,3) 
  a = NULL
  # c <- sample(0.1:100, replace=T, 5) 
  co <- c(0.1,1,10,100)
  
  best_acc = 0
  best_d =0
  best_c = 0
  
  l <- nrow(df)
  
  for(i in d) {
    for(j in co) {
      
      
      mymodelK <- svm(Rings~. , data =df , kernel = 'polynomial', degree = i, cost = j, cross =5)
      
      accu = mymodelK$tot.accuracy
      
      if(accu > best_acc)
      {
        best_d = i
        best_c = j
        best_acc = accu
      }
      
      a = rbind(a, data.frame(i, j, accu))
      
    }
  }
  
  sort_accu <- a[order(a$accu),]
  
  print(sort_accu)
  x <- subset(df, select = -Rings)
  y <- subset(df,select=Rings)
  best_model <- svm(Rings~. , data =df,type='C-classification',degree = best_d, cost = best_c, kernel= 'polynomial', cross =5)
  
  pred_model <- as.integer(as.vector(predict(best_model,x)))
  
  h_vec <- vector()
  distance<-0
  
  # Part of question 5 to plot the graph 
  h_vec <- vector()
  for(j in 1:nrow(df))
    
  {
    dist  <- abs(y[j,]-pred_model[j])
    h_vec <- rbind(h_vec,dist)
    distance <- dist + distance
  }
  
  
  ave_dist = (mean(distance)/l)
  print(ave_dist)
  
  hist(h_vec) #creates histogram
  
  # to plot the data points on the graph
  final <- predict(best_model,df[,])
  points(df$X, final, col = "red", pch=6)
  
}


# ______________End of Exercise 4 and 5


# Exercise 6
best_machine <- function(df){
  l <- nrow(df)
  
  a = NULL
  co <- c(0.1,1,10,100)
  ep <- c(0.1,0.5,1.1)
  d <- c(1,2,3) 
  accu=0
  best_mse = 1000
  best_d =0
  best_c = 0
  best_e = 0
  
  l <- nrow(df)
  
  for(i in d) {
    for(j in co) {
      for(k in ep) {
        mymodelK <- svm( Rings~. , data =df ,type = 'eps-regression',kernel = 'polynomial', degree = i,cost =j,epsilon =k, cross=5)
        
        mse = NULL
        mse = mymodelK$tot.MSE
        
        if(mse < best_mse)
        {
          best_d = i
          best_c = j
          best_e = k
          best_mse = mse
        }
        
        a = rbind(a, data.frame(i,j,k, mse))
      }
    }
  }
  
  #print(best_d)
  #print(best_c)
  #print(best_e)
  #print(best_mse)
  x <- subset(df, select = -Rings)
  y <- subset(df,select=Rings)
  model <- svm(Rings~. , data =df,type='eps-regression',degree = best_d, cost = best_c, epsilon = best_e, kernel= 'polynomial',cross=5)
  
  #pred_model <- as.integer(as.vector(predict(model,x)))
  pred_model <- predict(model, df[,])
  print(pred_model)
  # to create histogram
  h_vec <- vector()
  distance<-0
  
  for(j in 1:nrow(df))
  {
    dist  <- abs(y[j,]- pred_model[j])
    h_vec <- rbind(h_vec,dist)
    distance <- dist + distance
  }
  ave_dist = (distance)/l
  print(ave_dist)
  hist(h_vec)
}

# _____________ End of Exercise 6

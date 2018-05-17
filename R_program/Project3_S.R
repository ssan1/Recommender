# Programming - 3  - Sanchita S

# Exercise 1

library(e1071)

  partition <- function(df,a) {
  
      r <- nrow(df)
      s <- a*r
      df1 = df[sample(r,size = s, replace=FALSE ),]
      
      all <- rbind(df,df1)
      df2 <- all[!duplicated(all,fromLast = FALSE)&!duplicated(all,fromLast = TRUE),] 
      
      v <- c(list(df1,df2))
 
  return(v)
  
}

#________________End of Exercise 1 ___________________________

# Exercise 2

  best_svm <- function(df,a,d,c){
  
      alldata <- partition(df,a)
      df1 <- as.data.frame(alldata[1])
      df2 <- as.data.frame(alldata[2])
     
     # Model is designed to train the data using training set
      tempA = 0
      tempd = 0
      tempc = 0
  
          for(i in d) {
                for(j in c) {
          mymodel <- svm(Class~., data = df1, kernel = "polynomial", degree = i, type = "C-classification", cost = j )
         
          # The designed model is fitted and applied on the test data df2 as below 
          
          svm.pred <- predict(mymodel, df2[,])
          
          accu = (mean(svm.pred == df2$Class))*100
 
                 if(accu > tempA){
                   tempA = accu
                   tempd = i
                   tempc = j
                }
        
                  if(tempA == 100) {break}

            }
      }
      
              out <- list(tempd,tempc,tempA)
              return(out)
  
  }
  
#________________End of Exercise 2 ___________________________

# Exercise 3


#	Degree	Cost	  Accuracy
#   1	    0.1	    0.8092486
#   1	    100	    0.9421965
#   2	    1	      0.7196532
#   2	    100	    0.982659
#   2	    1000	  0.9884393
#   2	    10000	  0.9942197
#   3	    10	    0.8439306
#   3	    1000	  1.00        #--- Highest Accuracy :- 100 % @ Cost = 1000 & Degree = 3 
#   4	    0.1	    0.7052023
#   4	    1000	  0.9855491
#   4	    10000	  0.9913295

#________________End of Exercise 3 ___________________________

# Exercise 4

# By setting the C-classification cost to 10^5 (meaning very little to no slack allowance), 
# determine the least degree value d that can produce a (nonlinear) model that can attain 100% accuracy
#for the "entire data set".



const_cost <- function(df,d){
  
  mymodel <- svm(Class~., data = df, kernel = "polynomial", degree = d, type = "C-classification", cost = 100000 )
  mymodel$index
  summary(mymodel)
  print(mymodel)
  
  
  svm.pred <- predict(mymodel, df)
  
  mean(svm.pred == df[,7])

}

# GETTING 100 % ACCURACY AT DEGREE = 2 SUBJECTED TO CONSTANT COST = 100000 
# Number of Support Vectors:  351


# ________________End of Exercise 4 ___________________________



# Exercise 5

best.svm.cross <- function(df, d, c, n) {

  tempA = 0
  tempd = 0
  tempc = 0
  
  for(i in d) {
    
    for(j in c) {
      
      crossmodel <- svm(Class~., data = df, kernel = "polynomial", degree = i, type = "C-classification", cost = j, cross = n )
  
      crossmodel$acurracies
  
      tot = crossmodel$tot.accuracy
 
          if(tot > tempA){
             tempA = tot
            tempd = i
            tempc = j
               }
        }
    
  }
          out <- list(tempd,tempc,tempA)
          return(out)
     
}


# ________________End of Exercise 5 ___________________________


# Exercise 6

# best.svm.cross(wscn.df,c(1,2,3,4),c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0),10) 

#Answer - One of the execution result shows that maximum accuracy achieved is 96.42346 where cost =1 , degree = 0.1
  

# ________________End of Exercise 6 ___________________________


#Exercise 7

  
  #out1 <- best.svm.cross(df,d,c,n)
  #mymodel <- svm(Class~., data = df, kernel = "polynomial", degree = out1[[1]], type = "C-classification", cost = out1[[2]], cross = 10)
  
  bootstrap <- function(df,mymodel,p, n) {
  
  
    
  accu <- vector()
 
  for( i in 1:n) {
    x <- nrow(df)
    S <- df[sample(x, size = x, replace = TRUE),]
   
    pred <- predict(mymodel,S)
    cm <- table(S$Class,pred)
   
    accu <- c(accu,((cm[1,1] + cm[2,2])/x)* 100)
    
  }
  sorted <- sort(accu)

  
  upb <- sorted[((1+p)/2)*100]
  lob <- sorted[((1-p)/2)*100]
  return(list(upb,lob))
}

# __________________ End of Exercise 7 ________________


# Exercise 8
  
  # Executing below at console
  
 # > out1 <- best.svm.cross(wscn.df,c(1,2,3,4),c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0),10) 
# > mymodel <- svm(Class~., data = wscn.df, kernel = "polynomial", degree = out1[[1]], type = "C-classification", cost = out1[[2]], cross = 10)
#> bootstrap(wscn.df,mymodel,0.9,100)
  
  # Out put -------
  
  #[[1]]
  #[1] 98.42632
  
  #[[2]]
  #[1] 96.42346
  

# __________________ End of Exercise 8 ________________

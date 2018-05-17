# Programming - Sanchita S

#Exercise 1

simple_learner <- function(df){
  
  plus_row <- nrow(subset(df, X6 == 1))  # calculate num of row for which y=+1 and save in  #the vector
  minus_row <- nrow(subset(df, X6 == -1))  # calculate num of row for which y=-1
  cplus_set <- subset(df, X6 == 1, -X6)  #creates subspace and save in vector cplus_set
  cminus_set <- subset(df, X6 == -1, -X6) # creates subspace and save in vector cminus_set
  
  cplus <- colSums(cplus_set)/plus_row 
  cminus <- colSums(cminus_set)/minus_row
  
  
  c_val <- 0.5*(cplus + cminus)
  
  w_val <-  cplus - cminus
  
  b_val <- sum(w_val*c_val)
 
  
  return(list(w_val,b_val))
  
}
# ____ End of simple_learner ___________________________


# Exercise 2

# Evaluation function - this function evaluate the necessary condition for convergence

evale <- function(df,w,b){
  
  for (i in 1:nrow(df))
  {
    x <- as.numeric(df[i,1:ncol(df)-1])
    
    cond <- (sum(w*x) -b) # use to check necessary condition for convergence
    if(sign(cond) != df[i,ncol(df)]) 
    {
      return(1)
    }
    
  }
  return(0)
}

#---------------------------
perceptron_learner <- function(df){
  
  w <- c(0,0,0,0,0)
  b <- 0
  #r <- c(1)
  #eta <- c(1)
  validate <- 1
  
  while(validate) {   # repeats until the condition is met
    for (i in 1:nrow(df))
    {
      x <- as.numeric(df[i,1:ncol(df)-1])
      
      cond <- (sum(w*x) -b)
      if(sign(cond) != df[i,ncol(df)]) 
      {
       
        w = w + ((df[i,ncol(df)])*x) # adjusts the value of w (w=w+x) as per algo
        b=b - df[i,ncol(df)] # adjusts the b value , assuming eta and r =1
      }
      
    }
  
  validate <- evale(df,w,b)  # this is written separately to validate the condition for convergence
  }
       v <- c(w,b)
       print(v)
  
  
}



#_______ End of perceptron_learner


# Exercise 3 - classify function

classify <- function(df,w,b) {
  v <- c()
  for(i in 1:nrow(df))
  {
    x <- as.numeric(df[i,1:ncol(df)-1])
    
    thres <- sum(w*x)  # classifying condition
    if(thres-b < 0)  # condition to classify the data, sum(wx)-b < 0 , classify as -1
    {
      v <- c(v,-1) #classify as -1 and show the -1 for classified vector in v
      
    } 
    else
    {    
      v <- c(v,1)   # sum(wx)-b >= 0 , classify as +1 and show the +1 for classified vector in v
      
    }
  }
  print(v)
}

# _________ End of classify function ____________________


# Exercise 4


#data_input.df <- read.csv("Exercise-4.csv")
#simple_learner(data_input.df)
# W is
        #      X1        X2        X3        X4        X5 
        #   11.87250  20.36818 -16.60066  27.87608 -22.29279 
# b is -70.39144

#_________ End of Exercise 4 _____________________________

# Exercise 5

#v <- c(w,b)
#[1]  409.2977  757.0583 -722.5130  908.0482 -929.8089 -278.0000

# b= -278

#_________ End of Exercise 5 _____________________________


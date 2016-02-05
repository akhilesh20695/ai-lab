#Program to implement Pocket Algorithm

#function to calculate output
calculate_output<- function(weight,z)     #function to calculate the output
{
  sum<-0
  for(k in 1:(cols-1))
  {
    sum <- sum +(weight[k]*z[k])  
  }
  sum<-sum+weight[k]
  
  if(sum>0)
  {
    error<-1
  }
  else
  {
    error<--1
  }
  return(error)       #returning the error
}


#Perceptron
X=as.matrix(read.table("Iris.txt",sep=",")) # reading the training file
rows<-nrow(X)     #calculating rows
cols<-ncol(X)     #calculating columns

#seperating the feature matrix and the target matrix
x<-X[1:rows,1:cols-1]
y<-X[1:rows,cols]


#percentage for data to taken for training
perc=30;


train<-(perc/100)*rows


learning_rate <- 0.02
iteration<-0
max_iteration<-1000 #total iterations

#wieghts and epochs vector
weight<-vector(mode='numeric',length=0)
epochs<-vector(mode='numeric',length=0)

#initialising the weights vector
for(i in 1:cols)
{
  weight <- c(weight,0.5)  
}

#cloning weights vector
temp_weights<-weight


#error vector
e_temp_in <- vector(mode='numeric',length = 0)
min_error<-1000000  #min error is initialised at 1000000


repeat
{
  
  iteration <- iteration+1
  temp_weights<-weight  #updating the weights
  e_in <- 0
  for(p in 1:train)
  {
   #calculating the output with updated weights and pprevious iteration's weights  
    error<- calculate_output(temp_weights,x[p,])
    error_updated<-calculate_output(weight,x[p,])
    #checking for misclassification
    if(error==1 && y[p]==-1)
    {
      local_error<--1
    }
    else if(error==-1 && y[p]==1)
    {
      local_error<-1
    }
    else if(error==y[p])
    {
      local_error<-0
    }
    
    #updating error
    e_in<-e_in+abs(local_error)
    
    if(error_updated != y[p]){
      if(error_updated == -1 && y[p] == 1){
        correction <- 1*learning_rate
      }
      if(error_updated == 1 && y[p] == -1){
        correction <- -1*learning_rate
      }
      #updating weights in case of misclassification
      for(l in 1:cols-1)
      {
        weight[l]<-weight[l]+ x[p,l]*correction
      }
      l<-l+1
      weight[l]<-weight[l]+correction
    }
    
    #checking for minimum error and weights
    if(e_in <= min_error){
      min_error <- e_in
      min_weights <- weight
    }
    
  }
  
  e_temp_in <- c(e_temp_in, e_in)
  
  
  if(iteration==max_iteration)
  {
    break
  }
 
  
}
cat("Minimum error is: ",min_error)
cat("\nMinimum weights are: ",min_weights)













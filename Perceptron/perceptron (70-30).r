#function
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
X=as.matrix(read.table("Iris.txt",sep=","))     #reading the file here. Taking the values sepaerated by ','
rows<-nrow(X)                                   #Finding the rows of the matrix obtained
cols<-ncol(X)                                   #Finding the cols of the matric obtained

# splitting the matrix into two matrices, x for the feature matrix and y for the target function  matrix
x<-X[1:rows,1:cols-1]
y<-X[1:rows,cols]



perc=70;    #Percentage of the data to be taken
train<-(perc/100)*rows



learning_rate <- 0.02
iteration<-0
max_iteration<-1000   #total Itertions for the perceptron

weight<-vector(mode='numeric',length=0)   #Weight vector
epochs<-vector(mode='numeric',length=0)   #Epochs vector

for(i in 1:cols)
{
  weight <- c(weight,500)  #initializing weight vector
}

temp_weights<-weight    #temporary copy of weight vector for calculation in a iteration

e_temp_in <- vector(mode='numeric',length = 0)  #in error vector
e_temp_out <- vector(mode='numeric', length = 0)#out error vector




repeat
{
  
  iteration <- iteration+1
  temp_weights<-weight #initialising temporary weights with updated weights
  e_in <- 0
  e_out <- 0
  for(p in 1:train)
  {
    
    error<- calculate_output(temp_weights,x[p,])  #Checking for class output with previous iteration's weight 
    
    if(error==1 && y[p]==-1)      #checking for missclassification
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
    
    correction<-local_error
    e_in<-e_in+abs(local_error)
    
    #checking for output with updated weights
    
    error_updated<- calculate_output(weight,x[p,])
    
    if(error_updated==1 && y[p]==-1)
    {
      local_error<--1
    }
    else if(error_updated==-1 && y[p]==1)
    {
      local_error<-1
    }
    else if(error_updated==y[p])
    {
      local_error<-0
    }
    
    #computing error
    correction<-local_error*learning_rate
    
    #updating weights
    for(l in 1:cols-1)
    {
      weight[l]<-weight[l]+ x[p,l]*correction
    }
    l<-l+1
    weight[l]<-weight[l]+correction
  }
  
  e_temp_in <- c(e_temp_in, e_in)
  
  #Checking for errors in test data
  for(j in train:150){
    output <- calculate_output(temp_weights,x[j,])
    
    #checkong for misclassification
    if(output == -1 && y[j] == 1){
      local_error <- 1
    }
    if(output == 1 && y[j] == -1){
      local_error <- -1
    }
    if(output == y[j]){
      local_error <- 0
    }
    e_out <- e_out + abs(local_error)
  }
  e_temp_out <- c(e_temp_out, e_out)
  
  if(iteration==max_iteration)
  {
    epochs<-c(epochs,iteration)
    break
  }
  else
  {
    epochs<-c(epochs,iteration)
  }
  
}


#plotting graphs. Note: Two graphs will be obtained
plot( epochs,e_temp_in,col="red",pch=20 )
lines(e_temp_in)
plot( epochs,e_temp_out, col="green", pch=20 )
lines(e_temp_out)











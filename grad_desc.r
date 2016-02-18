#function
calculate_output<- function(weight,z)     #function to calculate the output
{
  sum<-0
  for(l in 1:(cols-1))
  {
    sum <- sum +(weight[l]*z[l])  
  }
  sum<-sum+weight[l]
  
  if(sum>0)
  {
    error<-1
  }
  else
  {
    error<-0
  }
  return(error)       #returning the error
}


X=as.matrix(read.table("ex2data2.txt",sep=","))
rows<-nrow(X)
cols<-ncol(X)

w = runif(cols,0,1)

result = vector(mode="numeric",length = cols-1)
e_temp_in <- vector(mode='numeric',length = 0)

et=0.3


for(i in 1:50){
  
  result<-0
  #calculate E_in
  
  for(j in 1:rows){
    x= X[j,1:cols-1]
    x=c(x,1)
    y= X[j,cols]
    wt<-t(w)
    result = result+(y*x)/(1+exp(y*(wt*x)))
  }
  
  result<-t(result)
  E_in = -(result)/rows
  w = w + et*E_in
  miss<-0
  
  
  
  for(k in 1:rows)
  {
    error<-calculate_output(w,X[k,1:cols-1]) 
    y<-X[k,cols]
    if(error==1 && y==0)      #checking for missclassification
    {
      local_error<-1
    }
    else if(error==0 && y==1)
    {
      local_error<-1
    }
    else if(error==y)
    {
      local_error<-0
    }
    miss<-miss+local_error
  }
  print(miss)
  e_temp_in<-c(e_temp_in,miss)
  print(w)
}
plot( e_temp_in,col="red",pch=10 )
lines(e_temp_in)

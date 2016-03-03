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


e_temp_in <- vector(mode='numeric',length = 0)

e_in<-vector(mode='numeric',length = 0)

line<-vector(mode='numeric',length=0)

misses=vector(mode='numeric',length = 0)


et=0.7

iteration<-vector(mode='numeric',length=0)

for(i in 1:100){
  
  #calculate E_in
  result = vector(mode="numeric",length = cols-1)
  
  for(j in 1:rows){
    x= X[j,1:cols-1]
    x= c(x,1)
    y= X[j,cols]
    result = result+(y*x)/(1+exp(y*w%*%x))
  }
  
  E_in = -(result)/rows
  w = w - et*E_in
  miss<-0
  
  result1=vector(mode='numeric',length = cols-1)
  
  for(k in 1:rows)
  {
   
    x<-X[k,1:cols-1]
    y<-X[k,cols]
    x= c(x,1)
    
    result1 <- result1+log(1+exp(-y*w%*%x),base=exp(1))
   
    
    h= 1/(1+exp(-w%*%x))
  
    
   if(h>0.5 && y==0)
   {
     local_error<-1
   }
  else if(h>0.5 && y==1)
  {
    local_error=1
  }
  else
  {
    local_error=0
  }
    
    miss<-miss+local_error
  }
  
  result1<-result1/rows
  
  misses<-c(misses,miss)
  e_in<-c(e_in,result1)
}

plot(misses,col="green",pch=10)
lines(misses)
#plot(e_in,col="red",pch=10)
#lines(e_in)
#class=as.factor(X[,cols])
#plot(X,col=class,pch=10)
#par(new=T)



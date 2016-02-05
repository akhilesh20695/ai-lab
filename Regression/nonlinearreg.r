#Program to implement non linear regression
#Note: program is not generic. Will word only for Iris DataSet or similar datasets with 4 features

X=as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
cols<-ncol(X)
#x<-X[1:110,1:4]
y<-X[1:110,5]

x<-cbind(X[1:110,1]*X[1:110,1],X[1:110,2]*X[1:110,2],X[1:110,3]*X[1:110,3],X[1:110,4]*X[1:110,4],X[1:110,1]*X[1:110,2],X[1:110,2]*X[1:110,3],X[1:110,3]*X[1:110,4],X[1:110,4]*X[1:110,1])

xt<-t(x) #taking transpose of x
a<-solve(xt%*%x)
w<-a%*%xt%*%y

error<-0

Y<-as.matrix(read.table("iris_data_norm_test.txt",sep=","))
testrows<-nrow(Y)

for(i in 1:testrows)
{
  output<-0
  for(j in 1:(cols-1))
  {
    output<-output+ w[j]*Y[i,j]  
  }
  if(output<0)
  {
    if(Y[i,5]>0)
      error<-error+1
  }
  else if(output>0)
  {
    if(Y[i,5]<0)
      error<-error+1
  }
  
}

cat("Error Measure is: ",error)
cat("\nWeights are: ",w)



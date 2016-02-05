#Program to implement regression and display error measure and new weights
#reading the training file
X=as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
#calculating rows and columns
rows<-nrow(X)
cols<-ncol(X)
#separating the features matrix and target function matrix
x<-X[1:rows,1:cols-1]
y<-X[1:rows,cols]

xt<-t(x) #taking transpose of x
a<-solve(xt%*%x)
#calculating w
w<-a%*%xt%*%y

error<-0

#reading test file
Y<-as.matrix(read.table("iris_data_norm_test.txt",sep=","))
#calculating it's rows
testrows<-nrow(Y)

#Calculating errors and computing the new weights
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


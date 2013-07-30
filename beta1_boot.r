beta1<-function(x,y){
  Sxx<-sum((x-mean(x))*(x-mean(x)))
  Sxy<-sum((x-mean(x))*(y-mean(y)))
  Sxy/Sxx
}
beta0<-function(x,y){
  mean(y)-beta1(x,y)*mean(x)
}

r<-function(x,y){
  n<-length(x)
  Sxx<-sum((x-mean(x))*(x-mean(x)))
  mu<-beta0(x,y)+beta1(x,y)*x
  rr<-(y-mu)/sqrt(1-1/n-(x-mean(x))^2/Sxx)
  rr-mean(rr)
}

sigma<-function(x,y){
  n<-length(x)
  mu<-beta0(x,y)+beta1(x,y)*x
  sqrt(sum((y-mu)*(y-mu))/(n-2))
}
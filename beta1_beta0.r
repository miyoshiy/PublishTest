B<-2000

# version2

beta1.s<-numeric(B)
set.seed(314159)
residual<-r(dat1$x,dat1$y)
for (b in 1:B){
  e.s<-sample(residual,replace=TRUE)
  y.s<-beta0(dat1$x,dat1$y)+beta1(dat1$x,dat1$y)*dat1$x+e.s
  beta1.s[b]<-beta1(dat1$x,y.s)
}
hist(beta1.s,main="",xlab="beta_1^*")
sort(beta1.s)[c(B*0.05,B*0.95)]
sort(beta1.s)[c(B*0.025,B*0.975)]

beta0.s<-numeric(B)
set.seed(314159)
residual<-r(dat1$x,dat1$y)
for (b in 1:B){
  e.s<-sample(residual,replace=TRUE)
  y.s<-beta0(dat1$x,dat1$y)+beta1(dat1$x,dat1$y)*dat1$x+e.s
  beta0.s[b]<-beta0(dat1$x,y.s)
}
hist(beta0.s,main="",xlab="beta_0^*")
sort(beta0.s)[c(B*0.05,B*0.95)]
sort(beta0.s)[c(B*0.025,B*0.975)]

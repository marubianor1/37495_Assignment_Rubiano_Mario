#Problem A
#a)
f<-function(x){
  cos(x)*exp(-x^2)
}
integrate(f,0,3)$value

#b)
index<-0:1000
min(sin(index))
which.min(sin(index))
index[which.min(sin(index))]

#c)
f1 <- function(x){
  (1/x) + exp(x)
}
nlm(f1,0.3)

#d)
f2<- function(x) {
  1 + x + x^2 + x^3 + x^4
}
plot(f2,xlim=c(-1,1))

#e)
roots<-polyroot(c(1, 1, 1, 1, 1))
plot(roots)

#Problem B
#a)
dat<- c(1, 2, 3, 1, 0, 9, 1, 8, 0)
X <- matrix(dat, nrow = 3, ncol = 3, byrow = T)
#t(X) #transpose
t(X)%*%X #matrix multipliction
#t(X)*X # one by one

#b)
G<-t(X)%*%X
eigen(G,symmetric=T)

#c)
#install.packages("expm") #one time
library(expm)
sqrt(G)

sqrtm(G)
expm(-G)

#Problem C
#a)
x<-seq(from = 0.3, to = 9.1, by = 0.1)
length(x)

#b)
y<-x^2-x-10
duplicated(y)
which(duplicated(y))

#c)
z<-y[-which(duplicated(y))]
length(z)

#d)
z[order(z,decreasing=T)]

#e)
z[-seq(from=2, to=length(z), by=2)]

#Problem D
set.seed(2026)
#a)
x1<-rnorm(60)
Y<-matrix(x1,nrow=4,ncol=15,byrow=T)
Z<-matrix(x1,nrow=4,ncol=15,byrow=F)

#b)
apply(Z,MARGIN = 1, FUN = mean)
apply(Z,MARGIN = 2, FUN = mean)

#c)
X1<-x1
dim(X1)<-c(3,4,5)
M<-apply(X1,sum, MARGIN=c(1,2))

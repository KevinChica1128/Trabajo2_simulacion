#-----------------------------------#
# Kevin Steven Garcia Chica
# Cesar A. Saavedra 
# Simulacion Estadistica Trabajo 2
#-----------------------------------#
install.packages("goftest")
library("goftest")
#funci?n:
Fun<-function(x,n,k){
  N=length(x)
  medias=rep(0,k)
  for (i in 1:k) {
    O=order(runif(N))
    medias[i]=mean(x[O[1:n]])
  }
  m=round(mean(medias),4)
  s2=round((var(medias)*(k-1)/k*10000)/10000,4)
  hist(medias,freq=FALSE,ylab="Densidad",xlab = paste("Promedios con n =",n,"datos"),
       main = paste("Media =",m,"\n Varianza =",s2))
  
  curve(exp(-((x-mean(medias))/sd(medias))^2/2)/(sd(medias)*sqrt(2*pi))
        ,min(medias),max(medias),add=T)
  cvm.test(medias,"pnorm",mean=m,sd=sqrt(s2))
  }




#Distribución Logistica: 
set.seed(1)
x<-rlogis(20000,0,1)  #Parámetros 0,1
x11()
par(mfrow=c(2,2))
Fun(x,1,1000)
Fun(x,10,1000)
Fun(x,25,1000)
Fun(x,50,1000)

set.seed(2)
x1<-rlogis(20000,9,4)  #Par?metros 9,4 
x11()
par(mfrow=c(2,2))
Fun(x1,1,1000)
Fun(x1,10,1000)
Fun(x1,25,1000)
Fun(x1,50,1000)

set.seed(3)
x2<-rlogis(20000,15,6)  #Par?metros 15,6 
x11()
par(mfrow=c(2,2))
Fun(x2,1,1000)
Fun(x2,10,1000)
Fun(x2,25,1000)
Fun(x2,50,1000)

#Distribuci?n Poisson:
set.seed(4)
x3<-rpois(20000,1)  #Par?metro lambda=1
x11()
par(mfrow=c(2,2))
Fun(x3,1,1000)
Fun(x3,10,1000)
Fun(x3,25,1000)
Fun(x3,50,1000)

set.seed(5)
x4<-rpois(20000,5)  #Par?metros lambda=5 
x11()
par(mfrow=c(2,2))
Fun(x4,1,1000)
Fun(x4,10,1000)
Fun(x4,25,1000)
Fun(x4,50,1000)

set.seed(6)
x5<-rpois(20000,10)  #Par?metros lambda=10
x11()
par(mfrow=c(2,2))
Fun(x5,1,1000)
Fun(x5,10,1000)
Fun(x5,25,1000)
Fun(x5,50,1000)

#Poblaciones
x11()
par(mfrow=c(2,3))
plot(table(x3),xlab = "x",ylab = "Frecuencia",main=expression(paste(lambda,"=1")))
plot(table(x4),xlab = "x",ylab = "Frecuencia",main=expression(paste(lambda,"=5")))
plot(table(x5),xlab = "x",ylab = "Frecuencia",main=expression(paste(lambda,"=10")))
hist(x,xlab = "x",ylab = "Frecuencia",main="a=0,b=1")
hist(x1,xlab = "x",ylab = "Frecuencia",main="a=9,b=4")
hist(x2,xlab = "x",ylab = "Frecuencia",main="a=15,b=6")
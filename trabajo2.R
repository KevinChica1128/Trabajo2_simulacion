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
  cvm.test(medias,"pnorm")
  }

#Distribuci?n Logistica: 
x<-rlogis(20000,0,1)  #Par?metros 0,1
x11()
par(mfrow=c(2,2))
Fun(x,1,5000)
Fun(x,10,5000)
Fun(x,25,5000)
Fun(x,50,5000)


x1<-rlogis(20000,9,4)  #Par?metros 9,4 
x11()
par(mfrow=c(2,2))
Fun(x1,1,5000)
Fun(x1,10,5000)
Fun(x1,25,5000)
Fun(x1,50,5000)

x2<-rlogis(20000,15,6)  #Par?metros 15,6 
x11()
par(mfrow=c(2,2))
Fun(x2,1,5000)
Fun(x2,10,5000)
Fun(x2,25,5000)
Fun(x2,50,5000)

#Distribuci?n Poisson:
x3<-rpois(20000,1)  #Par?metro lambda=1
x11()
par(mfrow=c(2,2))
Fun(x3,1,5000)
Fun(x3,5,5000)
Fun(x3,10,5000)
Fun(x3,30,5000)

x4<-rpois(20000,5)  #Par?metros lambda=5 
x11()
par(mfrow=c(2,2))
Fun(x4,1,5000)
Fun(x4,5,5000)
Fun(x4,10,5000)
Fun(x4,30,5000)

x5<-rpois(20000,10)  #Par?metros lambda=10
x11()
par(mfrow=c(2,2))
Fun(x5,1,5000)
Fun(x5,5,5000)
Fun(x5,10,5000)
Fun(x5,30,5000)
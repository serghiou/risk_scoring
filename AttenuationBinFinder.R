#set up directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) 

#install required packages if not already installed
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}
if("reshape2" %in% rownames(installed.packages())==FALSE){install.packages("reshape2"); require(reshape2)}else{require(reshape2)}
if("useful" %in% rownames(installed.packages())==FALSE){install.packages("useful"); require(useful)}else{require(useful)}
if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}

library(dplyr)
#Load in MonteCarloDistanceAtten - values for dB and distance piece

dtPoint<-read.csv('MonteCarloDistanceAtten.csv')
dtPoint<-read.csv('MonteCarloData2.csv')
dtPoint<-read.csv('PointData.csv')
dtPoint <- na.omit(dtPoint %>% select(Distance, Attenuation))
names(dtPoint) <- c("Distance", "MeanAtten")
dtPoint <<- dtPoint %>% filter(MeanAtten > 20)
#Gaussian plume approach

InterOptAB <- function(x) {
  a = x[1]
  b = x[2]
  
  #Gaussian plume approach
  points<-100000
  
  #---- using spherical coordinates -----------
  
  #randomly sampling attenuation ranges
  tot <- length(dtPoint$MeanAtten)
  pA <- sum(dtPoint$MeanAtten<=a) /tot
  pB <- sum(dtPoint$MeanAtten>a & dtPoint$MeanAtten<=b) /tot
  pC <- sum(dtPoint$MeanAtten>b) /tot
  
  attenuation<-sample(c("<=a",">a & <=b", ">b"),points,replace=TRUE)
  rho<-rep(NA,points)
  
  rho[attenuation=="<=a"]<-sample(dtPoint$Distance[dtPoint$MeanAtten<=a],length(rho[attenuation=="<=a"]),replace=TRUE)
  rho[attenuation==">a & <=b"]<-sample(dtPoint$Distance[dtPoint$MeanAtten>a & dtPoint$MeanAtten<=b],length(rho[attenuation==">a & <=b"]),replace=TRUE)
  rho[attenuation==">b"]<-sample(dtPoint$Distance[dtPoint$MeanAtten>b],length(rho[attenuation==">b"]),replace=TRUE)
  
  #randomly sampling phi and theta
  phi<-rep(NA,points)
  theta<-rep(NA,points)
  
  phi[rho<=1]<-pi/2
  phi[rho>1]<-runif(length(phi[rho>1]),0,pi)
  
  theta[rho<=1]<-0
  theta[rho>1]<-runif(length(theta[rho>1]),0,2*pi)
  
  #convert to Cartesian for plume equation 
  x<-rho*sin(phi)*cos(theta)
  y<-rho*sin(phi)*sin(theta)
  z<-rep(NA,points)
  z[rho>1]<-rho[rho>1]*cos(phi[rho>1])
  z[rho<=1]<-0
  
  #source of plume is at (0,0,0)
  
  #check on distances...
  test<-4
  sqrt(x[test]^2 + y[test]^2 + z[test]^2)
  rho[test]
  
  #holding constant for weight setting
  C.emit<-50 #in arbitrary units
  
  #exhalation rates
  X<-(rtrunc(points,"norm",a=0,mean=16.3,sd=4.15)/(24*60*60))
  
  Q<-C.emit * X #viral particles/m^3 x m^3/s exhalation rates (Exposure Factors Handbook)
  
  #U = (m^3 / s) / m^2 of cross sectional area --> m/s
  #A (surface area of mouth cross sectional) - large bite
  A<-runif(points,23,59)/(100^2) #convert from cm^2 to m^2
  U<-X/A
  
  
  #moderately stable
  I.y<-runif(points,0.08,0.25)
  I.z<-runif(points,0.03,0.07)
  
  omega.y<-rep(NA,points)
  omega.z<-rep(NA,points)
  
  omega.y[x>0]<-I.y[x>0]*x[x>0]
  omega.z[x>0]<-I.z[x>0]*x[x>0]
  
  #concentration at given x, y, z points
  C<-rep(NA,points)
  C[x>0]<-(Q[x>0]/U[x>0])*(1/(2*pi*omega.y[x>0]*omega.z[x>0]*1))*exp(-y[x>0]^2/(2*omega.y[x>0]^2))*exp(-z[x>0]^2/(2*omega.z[x>0]^2))
  C[x<=0]<-0
  
  I<-(rtrunc(points,"norm",a=0,mean=16.3,sd=4.15)/(24*60))
  
  duration<-30 #placeholder just so units make sense
  
  #viral particles/m^3 x m^3/min x min
  Dose<-C*I*duration
  
  frame<-data.frame(x=x,y=y,z=z,C=C,rho=rho,theta=theta,phi=phi,attenuation=attenuation,Dose=Dose,
                    omega.y=omega.y,omega.z=omega.z,Q.U=Q/U,I=I,X=X,A=A)
  
  dtA <- frame %>% filter(attenuation == "<=a")
  
  dtB <- frame %>% filter(attenuation == ">a & <=b")
  
  dtC <- frame %>% filter(attenuation == ">b")
  
  ExDA <- mean(dtA$Dose)
  ExDB <- mean(dtB$Dose)
  ExDC <- mean(dtC$Dose)
  
  DistOut <- sqrt( (2*pA*pB)*((ExDA - ExDB)^2) + (2*pB*pC)*((ExDB - ExDC)^2))# + (2*pC*pA)*((ExDC - ExDA)^2))
  #data.frame(ExpDose = c(ExDA,ExDB,ExDC), LocProb = c(pA,pB,pC),Metric = rep(DistOut,3))
  return(data.frame(ExpDose = c(ExDA,ExDB,ExDC), BinFreq = c(pA,pB,pC),Metric = rep(DistOut,3)))
}



nextfun <- function(x) {
  a = sample(30:75,1,replace = T)
  b = sample((a+10):85,1,replace = T)
  return(c(a,b))
}


tuples <- lapply(1:5, nextfun)
luples <- sapply(tuples, InterOptAB)
df.uples <- data.frame(tup = matrix(unlist(tuples), nrow=length(tuples), byrow = T), out = luples)

tuples <- lapply(1:5, nextfun)
luples <- sapply(tuples, InterOptAB)
df.uples <- rbind(df.uples, data.frame(tup = matrix(unlist(tuples), nrow=length(tuples), byrow = T), out = luples))
df.uples

df.sorted <- df.uples[order(-df.uples$out),]
#InterOptAB(c(25,52))

df.max <- head(df.sorted,10)
df.maxtest <- df.max
for (i in 1:length(df.max$out)) {
  vec.pair <- c()
  for (j in 1:100) {
    vec.pair <- c(vec.pair, InterOptAB(c(df.max$tup.1[i],df.max$tup.2[i])))
  }
  df.maxtest$out[i] <- mean(vec.pair)
}
df.mmax <- df.maxtest[order(-df.maxtest$out),]

InterOptAB(c(35,50))
InterOptAB(c(50,70))

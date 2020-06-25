#set up directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) 

#install required packages if not already installed
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}
if("reshape2" %in% rownames(installed.packages())==FALSE){install.packages("reshape2"); require(reshape2)}else{require(reshape2)}
if("useful" %in% rownames(installed.packages())==FALSE){install.packages("useful"); require(useful)}else{require(useful)}
if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}
if("lamW" %in% rownames(installed.packages())==FALSE){install.packages("lamW"); require(lamW)}else{require(lamW)}

dataforlambda<-function(duration,spouse=c('TRUE'),iter){
  
  #duration = # of hours of contact
  #spouse = TRUE for spouse scenario, FALSE for adults in same household scenario
  #iter = number of iterations per time step
  
  duration.halfhour<-duration*2 #number of half hour time steps
  
  #assume close contact, so phi = pi/2 and theta = 0 (on x-axis, face-to-face with emitter)
  y=0
  z=0
  
  C.emit<-50 #in arbitrary units
  
  #initializing vector for final dose saving
  final.dose<-rep(NA,iter)
  
  for (j in 1:iter){
    
    #Gaussian plume approach
      
    #preparing vectors for loop
    x<-runif(duration.halfhour,0.5,1) #distance in m away from emitter
    X<-(rtrunc(duration.halfhour,"norm",a=0,mean=16.3,sd=4.15)/(24*60*60)) #exhalation rates in m^3/s
    A<-runif(duration.halfhour,23,59)/(100^2) #cross section of mouth in m^2 (converted from cm^2)
    U<-X/A
    I.y<-runif(duration.halfhour,0.08,0.25)
    I.z<-runif(duration.halfhour,0.03,0.07)
    omega.y<-I.y*x
    omega.z<-I.z*x
    
    Q<-C.emit * X #viral particles/m^3 x m^3/s exhalation rates (Exposure Factors Handbook)
    C<-(Q/U)*(1/(2*pi*omega.y*omega.z*1))*exp(-y^2/(2*omega.y^2))*exp(-z^2/(2*omega.z^2))
    I<-(rtrunc(duration.halfhour,"norm",a=0,mean=16.3,sd=4.15)/(24*60)) #inhalation rates in m^3/s
    
    Dose.cumulative<-rep(NA,duration.halfhour)
    time<-rep(NA,duration.halfhour)
    
    Dose.cumulative[1]<-C[1]*I[1]*(30*60) #dose in first half hour
    time[1]<-1
    
    for (i in 2:duration.halfhour){
      Dose.cumulative[i]<-Dose.cumulative[i-1]+(C[i]*I[i]*(30*60))
      time[i]<-i
    }#end of for loop for calculating cumulative dose for iter j

    final.dose[j]<-Dose.cumulative[duration.halfhour]
    
    #saving frames
    if (j==1){
      frame<-data.frame(time=time,Dose=Dose.cumulative,spouse=spouse,duration=duration,j=j,x=x,X=X,A=A,I.y=I.y,I.z=I.z,C=C,I=I)
    }else{
      frametemp<-data.frame(time=time,Dose=Dose.cumulative,spouse=spouse,duration=duration,j=j,x=x,X=X,A=A,I.y=I.y,I.z=I.z,C=C,I=I)
      frame<-rbind(frame,frametemp)
    }
 
  }#end of iter
  
  #save frame globally
  frame.save<<-frame
  final.dose.all<<-final.dose
  
} #end of function

#6 hours, face-to-face, spouse

dataforlambda(10,TRUE,10000)
frame.spouse<-frame.save
final.dose.spouse<-final.dose.all

dataforlambda(1,FALSE,10000)
frame.nonspouse<-frame.save
final.dose.nonspouse<-final.dose.all

frame.all<-rbind(frame.spouse,frame.nonspouse)

frame.dose<-data.frame(dose=c(final.dose.spouse,final.dose.nonspouse),c(rep("scenario 1",length(final.dose.spouse)),
                                                                   rep("scenario 2",length(final.dose.nonspouse))))
ggplot(frame.dose)+geom_density(aes(x=dose,group=dose))
ggplot(frame.all)+geom_point(aes(x=time,y=Dose,colour=duration))

mean(log(frame.all$Dose[frame.all$time==20]))
mean(log(frame.all$Dose[frame.all$duration==1 & frame.all$time==2]))


#-------------lam W section ------------------------------------

rLISest <- function(n, mu, sd, lambda) {
  #IS Estimator of e^{-lambda X}
  Y <- rnorm(n,mean=0, sd = sd)
  Wu <- lambertW0(sd*lambda)
  Nu <- exp(-(Wu^2)*(exp(Y)-1-Y))
  LIS <- 1 - exp(-lambda * mu)*exp(-((Wu^2) * 2*Wu)/(2*sd))*Nu
  LIS.output<<-mean(LIS)
}

n<-10000
sd<-log(10)
seq<-runif(n,-4,2)

scenario<-c("Spouse, 10 hours","Nonspouse, 1 hour")

for (j in 1:length(scenario)){
  
  if (j==1){
    mu<-mean(log(final.dose.spouse))
  }else{
    mu<-mean(log(final.dose.nonspouse))
  }
  
  for (i in 1:length(seq)){
    
    lambda<-1*10^seq[i]
    
    rLISest(n=n,mu=mu,sd=sd,lambda=lambda)
    
    if(i==1 & j==1){
      frame.LIS<-data.frame(LIS=LIS.output,lambda=lambda,scenario=scenario[j])
    }else{
      frametemp<-data.frame(LIS=LIS.output,lambda=lambda,scenario=scenario[j])
      frame.LIS<-rbind(frame.LIS,frametemp)
    }
    
  }
}

frame.LIS$distance<-sqrt((frame.LIS$LIS[frame.LIS$scenario=="Spouse, 10 hours"]-0.28)^2+(frame.LIS$LIS[frame.LIS$scenario=="Nonspouse, 1 hour"]-0.17)^2)

windows()
B<-ggplot(frame.LIS)+geom_point(aes(x=lambda,y=LIS,colour=log10(distance),shape=scenario))+
  geom_hline(yintercept=0.28,linetype="solid",colour="red",size=1,alpha=0.5)+
  geom_hline(yintercept=0.17,linetype="dashed",colour="red",size=1,alpha=0.5)+
  geom_vline(xintercept=mean(frame.LIS$lambda[frame.LIS$distance==min(frame.LIS$distance)]),linetype="dashed",colour="green",size=1,alpha=0.5)+
  scale_y_continuous(trans="log10",name="Infection Risk")+
  scale_x_continuous(name="lambda",trans="log10")+theme_pubr()+ggtitle("Transform Method")


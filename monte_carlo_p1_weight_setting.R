#set up directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) 

#install required packages if not already installed
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}
if("reshape2" %in% rownames(installed.packages())==FALSE){install.packages("reshape2"); require(reshape2)}else{require(reshape2)}
if("useful" %in% rownames(installed.packages())==FALSE){install.packages("useful"); require(useful)}else{require(useful)}
if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}


#Load in MonteCarloDistanceAtten - values for dB and distance piece

dtPoint<-read.csv('MonteCarloDistanceAtten.csv')

#Gaussian plume approach

points<-100000

#---- using spherical coordinates -----------

#randomly sampling attenuation ranges

attenuation<-sample(c("<=60",">60 & <=80", ">80"),points,replace=TRUE)

#randomly sampling distances that go with these attenuations

rho<-rep(NA,points)
rho[attenuation=="<=60"]<-sample(dtPoint$Distance[dtPoint$MeanAtten<=60],length(rho[attenuation=="<=60"]),replace=TRUE)
rho[attenuation==">60 & <=80"]<-sample(dtPoint$Distance[dtPoint$MeanAtten>60 & dtPoint$MeanAtten<=80],length(rho[attenuation==">60 & <=80"]),replace=TRUE)
rho[attenuation==">80"]<-sample(dtPoint$Distance[dtPoint$MeanAtten>80],length(rho[attenuation==">80"]),replace=TRUE)

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
C.emit<- 11.5*10^6 #copies/cm3 (high emitter but asymptomatic) (conver to per m^3)

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

#----------------------------------------------spearman corr coeff-----------------------------------------------------

framecor = subset(frame,select=-c(attenuation))

cormat<-cor(framecor,method=c("spearman"))
melted_cormat<-melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  geom_text(aes(label = signif(value, 2))) +
  scale_fill_gradient(low = "white", high = "blue")

#-------------------------------------------------------------- Doses -----------------------------------------------------------------------------------------------

#doses by distance (colored by attenuation bucket)

#frame$front<-rep(NA,length(frame$rho))
#frame$front[frame$x>0]<-"in front"
#frame$front[frame$x<=0]<-"behind"

plot1<-ggplot(frame)+geom_point(aes(x=rho,y=Dose,colour=attenuation))+
  facet_grid(~attenuation)+
  theme_pubr()
plot1

windows()
plot2<-ggplot(frame)+geom_violin(aes(x=attenuation,y=Dose))+theme_pubr()
plot2

plot3<-ggplot(frame)+geom_violin(aes(x=attenuation,y=rho))+theme_pubr()+
  scale_y_continuous(name="Distance (m)")
plot3

ggarrange(plot2,plot3)

plot2B<-ggplot(frame[frame$Dose>1,])+geom_violin(aes(x=attenuation,y=Dose))+theme_pubr()+
  scale_y_continuous(trans="log10")

ggarrange(plot2B,plot3)


#checking out mean attenuation and distance
ggplot(dtPoint,aes(x=MeanAtten,y=Distance))+geom_point()

#------------------------ Ratios of Dose based on Distance -----------------------------------------------


mean(frame$Dose[frame$attenuation.distance=="<=.5 m"]) / mean(frame$Dose[frame$attenuation.distance==">.5 m & <=2 m"])
mean(frame$Dose[frame$attenuation.distance==">2 m"])/mean(frame$Dose[frame$attenuation.distance==">.5 m & <=2 m"])


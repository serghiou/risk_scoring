#set up directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) 

#install required packages if not already installed
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}
if("reshape2" %in% rownames(installed.packages())==FALSE){install.packages("reshape2"); require(reshape2)}else{require(reshape2)}
if("useful" %in% rownames(installed.packages())==FALSE){install.packages("useful"); require(useful)}else{require(useful)}

#Gaussian plume approach


points<-100000

#---- using spherical coordinates -----------

#rho = 3D distance from emitter
rho<-runif(points, 0, 6)
theta<-runif(points,0,2*pi)
phi<-runif(points,0,pi) #going to inform with Zhang et al. work - emailed on 6/21 to ask for mean and SD of head pitch angle
  
x<-rho*sin(phi)*cos(theta)
y<-rho*sin(phi)*sin(theta)
z<-rho*cos(phi)


#source of plume is at (0,0,0)

#check on distances...
test<-4
sqrt(x[test]^2 + y[test]^2 + z[test]^2)
rho[test]
  
    
C.emit<- 11.5*10^6 #copies/cm3 (high emitter but asymptomatic) (conver to per m^3)
    
  
Q<-C.emit * (rnorm(points,mean=16.3,sd=4.15)/(24*60*60)) #viral particles/m^3 x m^3/s exhalation rates (Exposure Factors Handbook)

U<-runif(points,min=1.3,max=1.4) #wind speed (m/s), used breathing velocity, Tang et al. (2013), mouth breathing (min) and nose breathing (max)
  
#moderately stable
I.y<-runif(points,0.08,0.25)
I.z<-runif(points,0.03,0.07)
  
omega.y<-I.y*x
omega.z<-I.z*x
  
#fraction of genome copies relates to infectious particles
fraction.infectious<-runif(points,0.0001,0.01)

#concentration at given x, y, z points
C<-(Q/U)*(1/(2*pi*omega.y*omega.z*1))*exp(-y^2/(2*omega.y^2))*exp(-z^2/(2*omega.z^2))*fraction.infectious
  
I<-(rnorm(points,mean=16.3,sd=4.15)/(24*60))

duration<-30 #placeholder just so units make sense

#viral particles/m^3 x m^3/min x min
Dose<-C*I*duration
  
frame<-data.frame(x=x,y=y,z=z,C=C,distance.3D=distance.3D,Dose=Dose,fraction.infectious=fraction.infectious,
                  omega.y=omega.y,omega.z=omega.z,Q.U=Q/U,I=I)


#assignment of distance ranges to our current bin categories
frame$attenuation.distance<-rep(NA,length(frame$x))
frame$attenuation.distance[frame$distance.3D<=.5]<-"<=.5 m"
frame$attenuation.distance[frame$distance.3D>.5 & frame$distance.3D<=2]<-">.5 m & <=2 m"
frame$attenuation.distance[frame$distance.3D>2]<-">2 m"

#----------------------------------------------spearman corr coeff-----------------------------------------------------

framecor = subset(frame,select=-c(attenuation.distance))

cormat<-cor(framecor,method=c("spearman"))
melted_cormat<-melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  geom_text(aes(label = signif(value, 2))) +
  scale_fill_gradient(low = "white", high = "blue")

#-------------------------------------------------------------- Doses -----------------------------------------------------------------------------------------------

#non-zero doses
A<-ggplot(frame[frame$Dose>0,])+geom_density(aes(x=log10(Dose)))+
  facet_wrap(~attenuation.distance)+
  theme_pubr()+
  scale_x_continuous(name=expression("Log"[10]*phantom(x)*"Dose"))+
  scale_y_continuous(name="Density")

windows()
A

#------------------------ Ratios of Dose based on Distance -----------------------------------------------


mean(frame$Dose[frame$attenuation.distance=="<=.5 m"]) / mean(frame$Dose[frame$attenuation.distance==">.5 m & <=2 m"])
mean(frame$Dose[frame$attenuation.distance==">2 m"])/mean(frame$Dose[frame$attenuation.distance==">.5 m & <=2 m"])


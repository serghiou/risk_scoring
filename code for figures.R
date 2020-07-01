#peak shedding in index case

#read in data from logic tables where 1st column
#is the discount on risk of getting sick under ideal testing to find
#asymptomatics as soon as they start shedding and column 2 is the
#probability that asymptomatics have finished shedding (from exposure, shedding
#starts 3 days pre what would be symptom onset)

risk.discount<-read.csv('risk_discount_data.csv')




#initial risk

#highest risk individual 15 min close contact
0.0505

#lowest risk individual, 15 min close contact
0.0103

risk.start<-c(0.0505,0.0103)

#fraction expected to be asymptomatic
fraction.asymptomatic<-c(0.15,0.25,0.50)

for (j in 1:length(risk.start)){
  
  for (i in 1:length(fraction.asymptomatic)){
    
    combined.discount<-1-(1-fraction.asymptomatic[i])*(1-risk.discount$discount)-fraction.asymptomatic[i]*risk.discount$asymp
    
    risks<-risk.start[j]*combined.discount
    days<-1:length(risks)
    
    if (i==1 & j==1){
      frame<-data.frame(risks=risks,days=days,risk.start=risk.start[j],fraction.asymptomatic=fraction.asymptomatic[i])
    }else{
      frametemp<-data.frame(risks=risks,days=days,risk.start=risk.start[j],fraction.asymptomatic=fraction.asymptomatic[i])
      frame<-rbind(frame,frametemp)
    }
  }
}

require(ggplot2)
require(ggpubr)

subframe<-frame[frame$fraction.asymptomatic==0.15,]
subframe$quarantine1<-rep(NA,length(subframe$risks))
subframe$quarantine1<-subframe$risks<=0.01
subframe$quarantine2<-rep(NA,length(subframe$risks))
subframe$quarantine2<-subframe$risks<=0.0241

#------------------------------------- figure 1 ------------------------------------------------------------

#panel 1

subframe1<-subframe[subframe$risk.start==0.0505 & subframe$days<=10,]
subframe2<-subframe[subframe$risk.start==0.0505 & subframe$days<=6,]
subframe3<-subframe[subframe$risk.start==0.0505 & subframe$days<=14,]
tempframe<-data.frame(risks=0.0241,days=6.8,risk.start=0.0505,fraction.asymptomatic=0.15,quarantine1=FALSE,quarantine2=TRUE)
subframe2<-rbind(subframe2,tempframe)
A<-ggplot(subframe[subframe$risk.start==0.0505,],aes(x=days,y=risks))+
  geom_area(data=subframe1,aes(x = ifelse(days<=10 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe2,aes(x = ifelse(days<=6.8 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe3,aes(x = ifelse(days<=14 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=1)+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0063,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection")+
  annotate("text",x=20,y=0.012,label="1% Threshold: Quarantine for 10 days",size=5)+
  annotate("text",x=20,y=0.0261,label="2.41% Threshold: Quarantine for 6 days",size=5)+
  annotate("text",x=20,y=0.0079,label="0.63% Threshold: Quarantine for 14 days",size=5)+
  theme_pubr()+theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  ggtitle("Peak Shedding in Index Case")

subframe4<-subframe[subframe$risk.start==0.0103 & subframe$days<=3,]
subframe5<-subframe[subframe$risk.start==0.0103 & subframe$days<=14,]
B<-ggplot(subframe[subframe$risk.start==0.0103,],aes(x=days,y=risks))+
  geom_area(data=subframe4,aes(x = ifelse(days<=3 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe5,aes(x = ifelse(days<=14 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=1)+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  geom_hline(yintercept=0.001293,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection",limits = c(0, 0.05))+
  annotate("text",x=20,y=0.012,label="1 % Threshold: Quarantine for 3 days",size=5)+
  annotate("text",x=20,y=0.0261,label="2.41 % Threshold: Quarantine for 0 days",size=5)+
  annotate("text",x=20,y=0.0033,label="0.13% Threshold: Quarantine for 14 days",size=5)+
  theme_pubr()+theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
  ggtitle("Low Shedding in Index Case")

windows()
ggarrange(A,B)


#panel 2



#------------------------------------ figure 2 -------------------------------------------------------------



subframe6<-frame[frame$risk.start==0.0505 & frame$days<=7 & frame$fraction.asymptomatic==0.25,]
tempframe<-data.frame(risks=0.0241,days=7.2,risk.start=0.0505,fraction.asymptomatic=0.25)
subframe6<-rbind(subframe6,tempframe)


subframe7<-frame[frame$risk.start==0.0505 & frame$days<=10 & frame$fraction.asymptomatic==0.5,]

A.2<-ggplot(frame[frame$risk.start==0.0505,],aes(x=days,y=risks))+
  geom_point(aes(shape=as.character(fraction.asymptomatic)),size=2,alpha=0.8)+
  geom_area(data=subframe2,aes(x = ifelse(days<=7 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe6,aes(x = ifelse(days<=7.2 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe7,aes(x = ifelse(days<=10 , days, 0)),fill="grey",alpha=0.3)+
  scale_shape_discrete(name="% Asymptomatic",labels=c("15%", "25%", "50%"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection",limits = c(0, 0.051))+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  annotate("text",x=20,y=0.032,label="2.41% Threshold",size=5)+
  annotate("text",x=20,y=0.030,label="Quarantine for 6 days (15% Asymptomatic)",size=5)+
  annotate("text",x=20,y=0.028,label="Quarantine for 7 days (25% Asymptomatic)",size=5)+
  annotate("text",x=20,y=0.026,label="Quarantine for 10 days (50% Asymptomatic)",size=5)+
  theme_pubr()+theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
                     legend.text = element_text(size=14), legend.title=element_text(size=14))+
  ggtitle("Peak Shedding in Index Case")

windows()
A.2


#peak shedding in index case

#read in data from logic tables where 1st column
#is the discount on risk of getting sick under ideal testing to find
#asymptomatics as soon as they start shedding and column 2 is the
#probability that asymptomatics have finished shedding (from exposure, shedding
#starts 3 days pre what would be symptom onset)

risk.discount<-read.csv('risk_discount_data.csv')

figure2<-read.csv('figure2_data.csv')


#initial risk

#highest risk individual 15 min close contact
high<-0.003

#lowest risk individual, 15 min close contact
low<-0.0003

risk.start<-c(high,low)

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

frame$days<-frame$days-1

require(ggplot2)
require(ggpubr)

subframe<-frame[frame$fraction.asymptomatic==0.15,]
subframe$quarantine1<-rep(NA,length(subframe$risks))
subframe$quarantine1<-subframe$risks<=0.01
subframe$quarantine2<-rep(NA,length(subframe$risks))
subframe$quarantine2<-subframe$risks<=0.03

#------------------------------------- figure 1 ------------------------------------------------------------

subframe1<-subframe[subframe$risk.start==high & subframe$days<=6,]
tempframe<-data.frame(risks=0.001,days=6.85,risk.start=high,fraction.asymptomatic=0.15,quarantine1=FALSE,quarantine2=FALSE)
subframe1<-rbind(subframe1,tempframe)
subframe3<-subframe[subframe$risk.start==high & subframe$days<=13,]
A<-ggplot(subframe[subframe$risk.start==high,],aes(x=days,y=risks))+
  geom_area(data=subframe1,aes(x = ifelse(days<=6.85 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe3,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=0.001,linetype="dashed",size=1)+
  geom_hline(yintercept=3.768626e-04,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of Infection",limits = c(0, 0.0035))+
  annotate("text",x=17,y=0.0030,label="0.1% Threshold: Quarantine for 7 days",size=6.5)+
  annotate("text",x=17,y=0.0025,label="0.038% Threshold: Quarantine for 14 days",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title = element_text(size=20),
                     axis.text=element_text(size=15))+
  ggtitle("A. Peak Shedding in Index Case")
A

subframe5<-subframe[subframe$risk.start==low & subframe$days<=13,]
B<-ggplot(subframe[subframe$risk.start==low,],aes(x=days,y=risks))+
  geom_area(data=subframe5,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=0.001,linetype="dashed",size=1)+
  geom_hline(yintercept=3.768626e-05,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of Infection",limits = c(0, 0.0035))+
  annotate("text",x=17,y=0.0030,label="0.1% Threshold: Quarantine for 0 days",size=6.5)+
  annotate("text",x=17,y=0.0025,label="0.0038% Threshold: Quarantine for 14 days",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title=element_text(size=20),
                     axis.text=element_text(size=15))+
  ggtitle("B. Low Shedding in Index Case")
B
windows()
ggarrange(A,B)



#------------------------------------ figure 2 -------------------------------------------------------------

subframe6<-frame[frame$risk.start==high & frame$days<=6 & frame$fraction.asymptomatic==0.15,]
tempframe<-data.frame(risks=1.179622e-03,days=6.3,risk.start=high,fraction.asymptomatic=0.15)
subframe6<-rbind(subframe6,tempframe)

subframe7<-frame[frame$risk.start==high & frame$days<=7 & frame$fraction.asymptomatic==0.25,]

subframe8<-frame[frame$risk.start==high & frame$days<=13 & frame$fraction.asymptomatic==0.5,]

A.2<-ggplot(frame[frame$risk.start==high,],aes(x=days,y=risks))+
  geom_point(aes(shape=as.character(fraction.asymptomatic),colour=as.character(fraction.asymptomatic)),size=4,alpha=0.8)+
  geom_area(data=subframe6,aes(x = ifelse(days<=6.3 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe7,aes(x = ifelse(days<=7 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe8,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  scale_shape_discrete(name="% Asymptomatic",labels=c("15%", "25%", "50%"))+
  scale_colour_discrete(name="% Asymptomatic",labels=c("15%", "25%", "50%"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of Infection",limits = c(0, 0.0035))+
  geom_hline(yintercept=1.179622e-03,linetype="dashed",size=1)+
  annotate("text",x=18,y=0.0030,label="0.12% Threshold",size=6.5)+
  annotate("text",x=18,y=0.0025,label="Quarantine for 7 days (15% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.0023,label="Quarantine for 8 days (25% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.0021,label="Quarantine for 14 days (50% Asymptomatic)",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
                     legend.text = element_text(size=20), legend.title=element_text(size=20),
                     axis.text=element_text(size=20),title=element_text(size=20))+
  ggtitle("A. Varying Asymptomatic %")

windows()
A.2

#negative test scenario
days<-c(frame$days[frame$risk.start==high & frame$fraction.asymptomatic==0.5],figure2$Days)
risks<-c(frame$risks[frame$risk.start==high & frame$fraction.asymptomatic==0.5],figure2$Risktest)
type<-c(rep("No test",length(frame$days[frame$risk.start==high & frame$fraction.asymptomatic==0.5])),
        rep("Negative Test Result",length(figure2$Days)))
frame2<-data.frame(days=days,risks=risks,type=type)


subframe9<-frame2[frame2$type=="Negative Test Result" & frame2$days<=4,]
subframe10<-frame2[frame2$type=="No test" & frame2$days<=13,]

windows()
B.2<-ggplot(data=frame2)+geom_point(aes(x=days,y=risks,group=type,colour=type,shape=type),size=4)+
    geom_hline(yintercept=1.179622e-03,linetype="dashed",size=1)+
    geom_vline(xintercept=4,linetype="solid",size=1)+
    geom_area(data=subframe9,aes(x = ifelse(days<=4 , days, 0),y=risks),fill="blue",alpha=0.3)+
    geom_area(data=subframe10,aes(x = ifelse(days<=13 , days, 0),y=risks),fill="red",alpha=0.3)+
    theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
                   legend.text = element_text(size=20), legend.title=element_text(size=20),
                   axis.text=element_text(size=20),title=element_text(size=20))+
    annotate("text",x=18,y=0.0030,label="0.12% Threshold",size=6.5)+
    annotate("text",x=18,y=0.0025,label="Quarantine for 4 days with Testing",size=6.5)+
    annotate("text",x=18,y=0.0023,label="Quarantine for 14 days without Testing",size=6.5)+
    scale_colour_discrete(name="",labels=c("Negative Test Result on Day 4","No Test"))+
    scale_shape_discrete(name="",labels=c("Negative Test Result on Day 4","No Test"))+
    scale_x_continuous(name="Days Since Exposure")+
    scale_y_continuous(name="Probability of Infection",limits = c(0, 0.0035))+
    ggtitle("B. No Test vs. Negative Test Result")


windows()
B.2

windows()
ggarrange(A.2,B.2)


#peak shedding in index case

#read in data from logic tables where 1st column
#is the discount on risk of getting sick under ideal testing to find
#asymptomatics as soon as they start shedding and column 2 is the
#probability that asymptomatics have finished shedding (from exposure, shedding
#starts 3 days pre what would be symptom onset)

risk.discount<-read.csv('risk_discount_data.csv')

figure4<-read.csv('figure4_data.csv')


#initial risk

lambda<-3.50E-6

weighted.sum<-31.53

#highest risk individual 15 min close contact
high<-1-exp(-lambda*(weighted.sum*1e2)) #high shedder: 1 x 10^2

#lowest risk individual, 15 min close contact
low<-1-exp(-lambda*(weighted.sum*1e1)) #low shedder: 1 x 10^1

risk.start<-c(high,low)

#fraction expected to be asymptomatic
fraction.asymptomatic<-c(0.2,0.25,0.50)

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

subframe<-frame[frame$fraction.asymptomatic==0.2,]


#------------------------------------- figure 3 ------------------------------------------------------------

subframe1<-subframe[subframe$risk.start==high & subframe$days<=4,]
#tempframe<-data.frame(risks=0.001,days=6.85,risk.start=high,fraction.asymptomatic=0.15,quarantine1=FALSE,quarantine2=FALSE)
#subframe1<-rbind(subframe1,tempframe)
subframe3<-subframe[subframe$risk.start==high & subframe$days<=13,]
A<-ggplot(subframe[subframe$risk.start==high,],aes(x=days,y=risks))+
  geom_area(data=subframe1,aes(x = ifelse(days<=4 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe3,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=8.4e-03,linetype="dashed",size=1)+
  geom_hline(yintercept=1.3e-03,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Transmitter","Low Shedding in Transmitter"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of current or future infectiousness",limits = c(0, 0.015))+
  annotate("text",x=15,y=0.010,label="0.84% Threshold: Quarantine for 5 days",size=6.5)+
  annotate("text",x=15,y=0.0030,label="0.13% Threshold: Quarantine for 14 days",size=6.5)+
  annotate("text",x=12,y=.015,label="A. Peak Shedding in Transmitter",size=10)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title = element_text(size=20),
                     axis.text=element_text(size=15))
  #ggtitle("A. Peak Shedding in Transmitter")
A

subframe5<-subframe[subframe$risk.start==low & subframe$days<=13,]
B<-ggplot(subframe[subframe$risk.start==low,],aes(x=days,y=risks))+
  geom_area(data=subframe5,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=1.3e-04,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Transmitter","Low Shedding in Transmitter"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of current or future infectiousness",limits = c(0, 0.015))+
  annotate("text",x=15,y=0.0030,label="0.013% Threshold: Quarantine for 14 days",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title=element_text(size=20),
                     axis.text=element_text(size=15))+
  annotate("text",x=12,y=.015,label="B. Low Shedding in Transmitter",size=10)
  #ggtitle("B. Low Shedding in Transmitter")

B
windows()
ggarrange(A,B)



#------------------------------------ figure 4 -------------------------------------------------------------

subframe6<-frame[frame$risk.start==high & frame$days<=13 & frame$fraction.asymptomatic==0.25,]
subframe6temp<-data.frame(risks=1.3e-3,days=13.3,risk.start=high,fraction.asymptomatic=0.25)
subframe6<-rbind(subframe6,subframe6temp)

subframe7<-frame[frame$risk.start==high & frame$days<=15 & frame$fraction.asymptomatic==0.5,]
subframe7temp<-data.frame(risks=1.3e-3,days=15.9,risk.start=high,fraction.asymptomatic=0.5)
subframe7<-rbind(subframe7,subframe7temp)


A.2<-ggplot(frame[frame$risk.start==high,],aes(x=days,y=risks))+
  geom_point(aes(shape=as.character(fraction.asymptomatic),colour=as.character(fraction.asymptomatic)),size=4,alpha=0.8)+
  geom_area(data=subframe3,aes(x = ifelse(days<=13 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe6,aes(x = ifelse(days<=13.3 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe7,aes(x = ifelse(days<=15.9 , days, 0)),fill="grey",alpha=0.3)+
  scale_shape_discrete(name="% Asymptomatic",labels=c("20%", "25%", "50%"))+
  scale_colour_discrete(name="% Asymptomatic",labels=c("20%", "25%", "50%"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Probability of current or future infectiousness",limits = c(0, 0.015))+
  geom_hline(yintercept=1.3e-03,linetype="dashed",size=1)+
  annotate("text",x=18,y=0.012,label="0.13% Threshold",size=6.5)+
  annotate("text",x=18,y=0.010,label="Quarantine for 14 days (20% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.009,label="Quarantine for 14 days (25% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.008,label="Quarantine for 16 days (50% Asymptomatic)",size=6.5)+
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


subframe8<-frame2[frame2$type=="Negative Test Result" & frame2$days<=9,]

B.2<-ggplot(data=frame2)+geom_point(aes(x=days,y=risks,group=type,colour=type,shape=type),size=4)+
    geom_hline(yintercept=1.3e-03,linetype="dashed",size=1)+
    geom_vline(xintercept=4,linetype="solid",size=1)+
    geom_area(data=subframe8,aes(x = ifelse(days<=9 , days, 0),y=risks),fill="blue",alpha=0.3)+
    geom_area(data=subframe7,aes(x = ifelse(days<=15.9 , days, 0),y=risks),fill="red",alpha=0.3)+
    theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
                   legend.text = element_text(size=20), legend.title=element_text(size=20),
                   axis.text=element_text(size=20),title=element_text(size=20))+
    annotate("text",x=18,y=0.012,label="0.13% Threshold",size=6.5)+
    annotate("text",x=18,y=0.010,label="Quarantine for 4 days with Testing",size=6.5)+
    annotate("text",x=18,y=0.009,label="Quarantine for 16 days without Testing",size=6.5)+
    scale_colour_discrete(name="",labels=c("Negative Test on Day 4","No Test"))+
    scale_shape_discrete(name="",labels=c("Negative Test on Day 4","No Test"))+
    scale_x_continuous(name="Days Since Exposure")+
    scale_y_continuous(name="Probability of current or future infectiousness",limits = c(0, 0.015))+
    ggtitle("B. No Test vs. Negative Test Result")


windows()
B.2

windows()
ggarrange(A.2,B.2)


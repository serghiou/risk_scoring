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
0.0505

#lowest risk individual, 15 min close contact
0.0052

risk.start<-c(0.0505,0.0052)

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
subframe$quarantine2<-subframe$risks<=0.0241

#------------------------------------- figure 1 ------------------------------------------------------------

subframe1<-subframe[subframe$risk.start==0.0505 & subframe$days<=9,]
subframe2<-subframe[subframe$risk.start==0.0505 & subframe$days<=5,]

subframe3<-subframe[subframe$risk.start==0.0505 & subframe$days<=13,]
tempframe<-data.frame(risks=0.0241,days=5,risk.start=0.0505,fraction.asymptomatic=0.15,quarantine1=FALSE,quarantine2=TRUE)
subframe2<-rbind(subframe2,tempframe)
A<-ggplot(subframe[subframe$risk.start==0.0505,],aes(x=days,y=risks))+
  geom_area(data=subframe1,aes(x = ifelse(days<=9 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe2,aes(x = ifelse(days<=5 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe3,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.03,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0063,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection",limits = c(0, 0.051))+
  annotate("text",x=19,y=0.012,label="1% Threshold: Quarantine for 10 days",size=6.5)+
  annotate("text",x=19,y=0.0321,label="3% Threshold: Quarantine for 5 days",size=6.5)+
  annotate("text",x=19,y=0.0079,label="0.63% Threshold: Quarantine for 14 days",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title = element_text(size=20),
                     axis.text=element_text(size=15))+
  ggtitle("A. Peak Shedding in Index Case")

subframe4<-subframe[subframe$risk.start==0.0052 & subframe$days<=2,]
subframe5<-subframe[subframe$risk.start==0.0052 & subframe$days<=13,]
B<-ggplot(subframe[subframe$risk.start==0.0052,],aes(x=days,y=risks))+
  geom_area(data=subframe5,aes(x = ifelse(days<=13 , days, 0)),fill="grey",alpha=0.3)+
  geom_point(size=3)+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.03,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0006532285,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection",limits = c(0, 0.051))+
  annotate("text",x=19,y=0.012,label="1% Threshold: Quarantine for 0 days",size=6.5)+
  annotate("text",x=19,y=0.0321,label="3% Threshold: Quarantine for 0 days",size=6.5)+
  annotate("text",x=19,y=0.0022,label="0.065% Threshold: Quarantine for 14 days",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title=element_text(size=20),
                     axis.text=element_text(size=15))+
  ggtitle("B. Low Shedding in Index Case")

windows()
ggarrange(A,B)



#------------------------------------ figure 2 -------------------------------------------------------------


subframe6<-frame[frame$risk.start==0.0505 & frame$days<=13 & frame$fraction.asymptomatic==0.25,]

subframe7<-frame[frame$risk.start==0.0505 & frame$days<=21 & frame$fraction.asymptomatic==0.5,]

A.2<-ggplot(frame[frame$risk.start==0.0505,],aes(x=days,y=risks))+
  geom_point(aes(shape=as.character(fraction.asymptomatic),colour=as.character(fraction.asymptomatic)),size=4,alpha=0.8)+
  geom_area(data=subframe1,aes(x = ifelse(days<=9 , days, 0)),fill="blue",alpha=0.3)+
  geom_area(data=subframe6,aes(x = ifelse(days<=13 , days, 0)),fill="red",alpha=0.3)+
  geom_area(data=subframe7,aes(x = ifelse(days<=21 , days, 0)),fill="grey",alpha=0.3)+
  scale_shape_discrete(name="% Asymptomatic",labels=c("15%", "25%", "50%"))+
  scale_colour_discrete(name="% Asymptomatic",labels=c("15%", "25%", "50%"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection",limits = c(0, 0.051))+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  annotate("text",x=18,y=0.036,label="1% Threshold",size=6.5)+
  annotate("text",x=18,y=0.034,label="Quarantine for 10 days (15% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.032,label="Quarantine for 14 days (25% Asymptomatic)",size=6.5)+
  annotate("text",x=18,y=0.030,label="Quarantine for 22 days (50% Asymptomatic)",size=6.5)+
  theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
                     legend.text = element_text(size=20), legend.title=element_text(size=20),
                     axis.text=element_text(size=20),title=element_text(size=20))+
  ggtitle("A. Varying Asymptomatic %")

windows()
A.2

#negative test scenario
days<-c(frame$days[frame$risk.start==0.0505 & frame$fraction.asymptomatic==0.5],figure2$Days)
risks<-c(frame$risks[frame$risk.start==0.0505 & frame$fraction.asymptomatic==0.5],figure2$Risktest)
type<-c(rep("No test",length(frame$days[frame$risk.start==0.0505 & frame$fraction.asymptomatic==0.5])),
        rep("Negative Test Result",length(figure2$Days)))
frame2<-data.frame(days=days,risks=risks,type=type)

subframe8<-frame2[frame2$type=="Negative Test Result" & frame2$days<=6,]
#tempframe<-data.frame(days=4.5,risks=0.01,type="No Test")
#subframe8<-rbind(subframe8,tempframe)
subframe9<-frame2[frame2$type=="No test" & frame2$days<=21,]


windows()
B.2<-ggplot(data=frame2)+geom_point(aes(x=days,y=risks,group=type,colour=type,shape=type),size=4)+
    geom_hline(yintercept=0.01,linetype="dashed",size=1)+
    geom_vline(xintercept=4,linetype="solid",size=1)+
    geom_area(data=subframe8,aes(x = ifelse(days<=6 , days, 0),y=risks),fill="blue",alpha=0.3)+
    geom_area(data=subframe9,aes(x = ifelse(days<=21 , days, 0),y=risks),fill="red",alpha=0.3)+
    theme_pubr()+theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),
                   legend.text = element_text(size=20), legend.title=element_text(size=20),
                   axis.text=element_text(size=20),title=element_text(size=20))+
    annotate("text",x=18,y=0.036,label="1% Threshold",size=6.5)+
    annotate("text",x=18,y=0.034,label="Quarantine for 7 days with Testing",size=6.5)+
    annotate("text",x=18,y=0.032,label="Quarantine for 22 days without Testing",size=6.5)+
   #annotate("text",x=14,y=0.048,label="Negative Test 4 Days Since Exposure",size=6.5)+
    scale_colour_discrete(name="",labels=c("Negative Test Result on Day 4","No Test"))+
    scale_shape_discrete(name="",labels=c("Negative Test Result on Day 4","No Test"))+
    scale_x_continuous(name="Days Since Exposure")+
    scale_y_continuous(name="Risk of Infection",limits = c(0, 0.052))+
    ggtitle("B. No Test vs. Negative Test Result")


windows()
B.2

ggarrange(A.2,B.2)


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

A<-ggplot(frame[frame$risk.start==0.0505,],aes(x=days,y=risks,colour=as.character(fraction.asymptomatic)))+
  geom_line(size=1)+
  scale_colour_discrete(name="Fraction of Asymptomatics")+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection")+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  theme_pubr()+
  ggtitle("Peak Shedding in Index Case")

B<-ggplot(frame[frame$risk.start==0.0103,],aes(x=days,y=risks,colour=as.character(fraction.asymptomatic)))+
  geom_line(size=1)+
  scale_colour_discrete(name="Fraction of Asymptomatics")+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection")+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  theme_pubr()+
  ggtitle("Low Shedding in Index Case")

windows()
ggarrange(A,B,common.legend = TRUE)


subframe<-frame[frame$fraction.asymptomatic==0.15,]
subframe$quarantine1<-rep(NA,length(subframe$risks))
subframe$quarantine1<-subframe$risks<=0.01
subframe$quarantine2<-rep(NA,length(subframe$risks))
subframe$quarantine2<-subframe$risks<=0.0241

windows()
ggplot(subframe,aes(x=days,y=risks,colour=as.character(risk.start),group=as.character(risk.start)))+
  geom_hline(yintercept=0.01,linetype="dashed",size=1)+
  geom_hline(yintercept=0.0241,linetype="dashed",size=1)+
  scale_colour_discrete(name="Scenario",labels=c("Peak Shedding in Index Case","Low Shedding in Index Case"))+
  scale_x_continuous(name="Days Since Exposure")+
  scale_y_continuous(name="Risk of Infection")+
  annotate("text", x = 20, y = 0.015, label = "1 % Threshold:")+
  annotate("text",x=20,y=0.013,label="Quarantine for 10 days (peak shedding), 3 days (low shedding)")+
  annotate("text", x = 20, y = 0.0291, label = "2.41 % Threshold:")+
  annotate("text",x=20,y=0.0271,label="Quarantine for 6 days (peak shedding), 0 days (low shedding)")+
  theme_pubr()+
  geom_line(size=1)

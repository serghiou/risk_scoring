
compare<-read.csv('compare.risk.level.csv')

plotA<-ggplot(compare)+geom_bar(aes(x=Days.since.post.symptom.onset,y=Risk.value,fill=Risk.value),stat="identity")+
  scale_x_continuous(name="Days Relative to Symptom Onset")+scale_y_continuous("Transmission Risk Value")+
  theme_pubr()+
  theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title = element_text(size=20),
                           axis.text=element_text(size=20),legend.position = "none",legend.text=element_text(size=20))+
  scale_fill_gradient(name="",low="lightgray",high="red")


time<-seq(0,30,0.1)
k<-3.10E-6

#scnenario 1, low shedder
dose.close.1<-time*2.39*compare$Risk.value[compare$Days.since.post.symptom.onset==-5]
dose.mid.1<-time*0.6*compare$Risk.value[compare$Days.since.post.symptom.onset==-5]
dose.far.1<-time*0.06*compare$Risk.value[compare$Days.since.post.symptom.onset==-5]

dose.1<-c(dose.close.1,dose.mid.1,dose.far.1)

risk.close.1<-1-exp(-dose.close.1*k)
risk.mid.1<-1-exp(-dose.mid.1*k)
risk.far.1<-1-exp(-dose.far.1*k)

risk.1<-c(risk.close.1,risk.mid.1,risk.far.1)

distance<-c(rep("<50 dB",length(time)),rep("50-80 dB",length(time)),rep(">80 dB",length(time)))

frame.plot.3<-data.frame(dose=dose.1,risk=risk.1,time=time,distance=distance)

#scenario 2, high shedder
dose.close.2<-time*2.39*compare$Risk.value[compare$Days.since.post.symptom.onset==0]
dose.mid.2<-time*0.6*compare$Risk.value[compare$Days.since.post.symptom.onset==0]
dose.far.2<-time*0.06*compare$Risk.value[compare$Days.since.post.symptom.onset==0]

dose.2<-c(dose.close.2,dose.mid.2,dose.far.2)

risk.close.2<-1-exp(-dose.close.2*k)
risk.mid.2<-1-exp(-dose.mid.2*k)
risk.far.2<-1-exp(-dose.far.2*k)

risk.2<-c(risk.close.2,risk.mid.2,risk.far.2)

frame.plot.4<-data.frame(dose=dose.2,risk=risk.2,time=time,distance=distance)

combined<-rbind(frame.plot.3,frame.plot.4)

combined$shedder<-c(rep("Low Shedding",length(distance)),rep("Peak Shedding",length(distance)))

combined$distance<-factor(combined$distance,c("<50 dB","50-80 dB",">80 dB"))


plotB<-ggplot(combined)+geom_line(aes(x=time,y=risk,group=interaction(distance,shedder),color=shedder,linetype=distance),size=1.5)+
  scale_x_continuous(name="Time (minutes)")+
  scale_y_continuous(labels = scales::percent,name="Risk",limits=c(0,0.03))+
  theme_pubr()+
  scale_color_manual(values = c("gray","red"),name="")+
  scale_linetype_manual(values=c("solid","dashed","dotted"),name="")+
  theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20),title = element_text(size=20),
      axis.text=element_text(size=20),legend.text=element_text(size=20),legend.position = c(0.45,0.9),
      legend.direction = "horizontal", legend.box = "vertical",legend.key.size = grid::unit(3, "lines"))

windows()
ggarrange(plotA,plotB)


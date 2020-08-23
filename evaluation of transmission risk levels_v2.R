
compare<-read.csv('compare.risk.level.csv')

plotA<-ggplot(compare)+geom_bar(aes(x=Days.since.post.symptom.onset,y=Risk.value,fill=Risk.value),stat="identity")+
  scale_x_continuous(name="Days Relative to Symptom Onset")+scale_y_continuous("Transmission Risk Value")+
  theme_pubr()+
  theme(axis.title.x = element_text(size=25),axis.title.y = element_text(size=25),title = element_text(size=25),
                           axis.text=element_text(size=22),legend.position = "none",legend.text=element_text(size=25))+
  scale_fill_gradient(name="",low="lightgray",high="red")

windows()
plotA

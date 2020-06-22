#set up directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) 

#install required packages if not already installed
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}

pitchdata<-read.csv('stand_stand_pitch.csv')

ggplot(pitchdata,aes(x=pitch))+
  geom_histogram(fill="light grey",aes(y=..density..),color="black",bins=50)+
  geom_density(fill="light blue",alpha=0.5)+
  geom_vline(aes(xintercept=mean(pitch)),color="red",linetype="solid",size=1)+
  geom_vline(aes(xintercept=mean(pitch)+2*sd(pitch)),color="red",linetype="dashed",size=1)+
  geom_vline(aes(xintercept=mean(pitch)-2*sd(pitch)),color="red",linetype="dashed",size=1)+
  theme_pubr()


iter<-10000
lambda<-runif(iter,10^-8,10^-5)

dose1<-354154.59 #dose for 30 hr in low attenuation range, high shedder
dose2<-708309.18 #dose for 60 hr in low attenuation range, high shedder

risks1<-1-exp(-lambda*dose1)
risks2<-1-exp(-lambda*dose2)

distance<-sqrt((risks1-.3)^2+(risks2-.52)^2)

data<-data.frame(lambda=lambda,risks1=risks1,risks2=risks2,distance=distance)
View(data)


ggplot(data)+geom_point(aes(x=lambda,y=risks1),colour="lightblue")+
  geom_point(aes(x=lambda,y=risks2),colour="grey")+
  geom_hline(yintercept=.3)+
  geom_hline(yintercept=.52)+
  geom_vline(xintercept=min(data$lambda[data$distance==min(data$distance)]),linetype="dashed",colour="red")+
  scale_y_continuous(name="Infection Risks")+
  scale_x_continuous(name=expression(lambda))+
  theme_pubr()
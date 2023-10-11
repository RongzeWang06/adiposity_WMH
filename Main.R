library(foreign)
library(rms)

WMH_all<-read.csv(...)

#INS2/0
attach(WMH_all)
dd<-datadist(WMH_all)
options(datadist='dd')

fit<-ols(WMH_log~rcs(BMI2,4)+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_all)
summary(fit)
an<-anova(fit)
an
B<-Predict(fit,BMI2)
tiff('WMH_BMI2.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=B, aes(BMI2,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI2+Age2+Sex,data = scaled_ins)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI2+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_ins)
summary(fit2)
confint(fit2)

scaled_ins<-lapply(WMH_all, scale)
geom_vline(xintercept=18.5,linetype=2,size=0.6)+
  geom_vline(xintercept=25,linetype=2,size=0.6)+
  geom_vline(xintercept=30,linetype=2,size=0.6)+
  
  ###change
  fit<-ols(WMH_log~rcs(BMI,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_all)
summary(fit)
an<-anova(fit)
an
B<-Predict(fit,BMI)
tiff('WMH_BMI.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B, aes(BMI,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=B, aes(BMI,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+geom_vline(xintercept=0,linetype=2,size=0.6)+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI+Age0+Sex+Date_y,data = scaled_ins)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_ins)
summary(fit2)
confint(fit2)

###
WMH_UW0<-subset(WMH_all,WMH_all$BMI0<"18.5")
WMH_NW0<-subset(WMH_all,WMH_all$BMI0<"25" & WMH_all$BMI0>="18.5")
WMH_OW0<-subset(WMH_all,WMH_all$BMI0<"30" & WMH_all$BMI0>="25")
WMH_OB0<-subset(WMH_all,WMH_all$BMI0>="30")

###Follow-up
WMH_S<-subset(WMH_all,WMH_all$Date_y<"9")
WMH_L<-subset(WMH_all,WMH_all$Date_y>="9")
scaled_S<-lapply(WMH_S, scale)
scaled_L<-lapply(WMH_L, scale)

attach(WMH_S)
dd1<-datadist(WMH_S)
options(datadist='dd1')
attach(WMH_L)
dd2<-datadist(WMH_L)
options(datadist='dd2')

fit1<-ols(WMH_log~rcs(BMI0,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_S)
an1<-anova(fit1)
an1
B1<-Predict(fit1,BMI0)
fit2<-ols(WMH_log~rcs(BMI0,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_L)
an2<-anova(fit2)
an2
B2<-Predict(fit2,BMI0)
tiff('Time_WMH_BMI0.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B1, aes(BMI0,yhat),linetype=1,size=1,alpha = 0.9,colour="skyblue")+
  geom_line(data=B2, aes(BMI0,yhat),linetype=1,size=1,alpha = 0.9,colour="orange")+
  geom_vline(xintercept=18.5,linetype=2,size=0.6)+
  geom_vline(xintercept=25,linetype=2,size=0.6)+
  geom_vline(xintercept=30,linetype=2,size=0.6)+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI0+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_S)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI0+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_L)
summary(fit2)
confint(fit2)
fit3<-lm(WMH_log~BMI0,data = scaled_S)
summary(fit3)
fit4<-lm(WMH_log~BMI0,data = scaled_L)
summary(fit4)

###Sex
WMH_M<-subset(WMH_all,WMH_all$Sex=="1")
WMH_F<-subset(WMH_all,WMH_all$Sex=="0")
scaled_M<-lapply(WMH_M, scale)
scaled_F<-lapply(WMH_F, scale)


attach(WMH_M)
dd1<-datadist(WMH_M)
options(datadist='dd1')
attach(WMH_F)
dd2<-datadist(WMH_F)
options(datadist='dd2')
fit1<-ols(WMH_log~rcs(BMI2,4)+Age2+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_M)
an1<-anova(fit1)
an1
B1<-Predict(fit1,BMI2)
fit2<-ols(WMH_log~rcs(BMI2,4)+Age2+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_F)
an2<-anova(fit2)
an2
B2<-Predict(fit2,BMI2)
tiff('Sex_WMH_BMI2.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B1, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="skyblue")+
  geom_line(data=B2, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="orange")+
  geom_vline(xintercept=18.5,linetype=2,size=0.6)+
  geom_vline(xintercept=25,linetype=2,size=0.6)+
  geom_vline(xintercept=30,linetype=2,size=0.6)+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI2+Age2+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_M)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI2+Age2+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_F)
summary(fit2)
confint(fit2)
fit3<-lm(WMH_log~BMI2,data = scaled_M)
summary(fit3)
fit4<-lm(WMH_log~BMI2,data = scaled_F)
summary(fit4)


###age
WMH_60<-subset(WMH_all,WMH_all$Age2<"60")
WMH_6075<-subset(WMH_all,WMH_all$Age2>="60"& WMH_all$Age2<="74")
WMH_75<-subset(WMH_all,WMH_all$Age2>"74")
scaled_60<-lapply(WMH_60, scale)
scaled_6075<-lapply(WMH_6075, scale)
scaled_75<-lapply(WMH_75, scale)

WMH_51<-subset(WMH_all,WMH_all$Age0<"51")
WMH_5165<-subset(WMH_all,WMH_all$Age0>="51"& WMH_all$Age0<="65")
WMH_65<-subset(WMH_all,WMH_all$Age0>"65")
scaled_51<-lapply(WMH_51, scale)
scaled_5165<-lapply(WMH_5165, scale)
scaled_65<-lapply(WMH_65, scale)

attach(WMH_60)
dd1<-datadist(WMH_60)
options(datadist='dd1')
attach(WMH_6075)
dd2<-datadist(WMH_6075)
options(datadist='dd2')
attach(WMH_75)
dd3<-datadist(WMH_75)
options(datadist='dd3')

fit1<-ols(WMH_log~rcs(BMI2,4)+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_60)
an1<-anova(fit1)
an1
B1<-Predict(fit1,BMI2)
fit2<-ols(WMH_log~rcs(BMI2,4)+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_6075)
an2<-anova(fit2)
an2
B2<-Predict(fit2,BMI2)
fit3<-ols(WMH_log~rcs(BMI2,4)+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_75)
an3<-anova(fit3)
an3
B3<-Predict(fit3,BMI2)
tiff('Age_WMH_BMI2.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B1, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="skyblue")+
  geom_line(data=B2, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="orange")+
  geom_line(data=B3, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="darkgrey")+
  geom_vline(xintercept=18.5,linetype=2,size=0.6)+
  geom_vline(xintercept=25,linetype=2,size=0.6)+
  geom_vline(xintercept=30,linetype=2,size=0.6)+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI2+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_60)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI2+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_6075)
summary(fit2)
confint(fit2)
fit3<-lm(WMH_log~BMI2+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_75)
summary(fit3)
confint(fit3)
fit4<-lm(WMH_log~BMI2,data = scaled_60)
summary(fit4)
fit5<-lm(WMH_log~BMI2,data = scaled_6075)
summary(fit5)
fit6<-lm(WMH_log~BMI2,data = scaled_75)
summary(fit6)


attach(WMH_51)
dd1<-datadist(WMH_51)
options(datadist='dd1')
attach(WMH_5165)
dd2<-datadist(WMH_5165)
options(datadist='dd2')
attach(WMH_65)
dd3<-datadist(WMH_65)
options(datadist='dd3')

fit1<-ols(WMH_log~rcs(BMI0,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_51)
an1<-anova(fit1)
an1
B1<-Predict(fit1,BMI0)
fit2<-ols(WMH_log~rcs(BMI0,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_5165)
an2<-anova(fit2)
an2
B2<-Predict(fit2,BMI0)
fit3<-ols(WMH_log~rcs(BMI0,4)+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = WMH_65)
an3<-anova(fit3)
an3
B3<-Predict(fit3,BMI0)
tiff('Age_WMH_BMI0.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B1, aes(BMI0,yhat),linetype=1,size=1,alpha = 0.9,colour="skyblue")+
  geom_line(data=B2, aes(BMI0,yhat),linetype=1,size=1,alpha = 0.9,colour="orange")+
  geom_line(data=B3, aes(BMI0,yhat),linetype=1,size=1,alpha = 0.9,colour="darkgrey")+
  geom_vline(xintercept=18.5,linetype=2,size=0.6)+
  geom_vline(xintercept=25,linetype=2,size=0.6)+
  geom_vline(xintercept=30,linetype=2,size=0.6)+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI0+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_51)
summary(fit1)
confint(fit1)
fit2<-lm(WMH_log~BMI0+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_5165)
summary(fit2)
confint(fit2)
fit3<-lm(WMH_log~BMI0+Age0+Sex+Date_y+Site2+Smoke0+SBP0+DBP0+LDLC+TAG+HbAlc,data = scaled_65)
summary(fit3)
confint(fit3)
fit4<-lm(WMH_log~BMI0,data = scaled_51)
summary(fit4)
fit5<-lm(WMH_log~BMI0,data = scaled_5165)
summary(fit5)
fit6<-lm(WMH_log~BMI0,data = scaled_65)
summary(fit6)


###White
WMH_white<-subset(WMH_all,WMH_all$White=="1")
scaled_white<-lapply(WMH_white, scale)

attach(WMH_white)
dd1<-datadist(WMH_white)
options(datadist='dd1')

fit<-ols(WMH_log~rcs(BMI2,4)+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = WMH_white)
summary(fit)
an<-anova(fit)
an
B<-Predict(fit,BMI2)
tiff('WMH_BMI2.tiff',height = 3000,width = 3000,res= 600)
ggplot()+geom_line(data=B, aes(BMI2,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=B, aes(BMI2,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+
  theme(
    axis.title.x=element_text(size=18),
    axis.title.y=element_text(size=18),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  labs(y="WMH_logit",x="Body mass index")
dev.off()

fit1<-lm(WMH_log~BMI2+Age2+Sex+Site2+Smoke2+SBP2+DBP2+LDLC+TAG+HbAlc,data = scaled_white)
summary(fit1)
confint(fit1)
fit3<-lm(WMH_log~BMI2,data = scaled_white)
summary(fit3)
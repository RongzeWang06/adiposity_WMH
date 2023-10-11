library(lmerTest)

WMH_long<-read.csv(...)
scaled_long<-lapply(WMH_long, scale)

lme1<-lmer(WMH_log~BMI+Date+BMI:Date+Sex+Age+(1|ID),data = scaled_long,na.action=na.omit)
summary(lme1)
confint(lme1)

#plot
library(forestplot)
longforest<-read.csv("long.csv",header = F)
tiff('forest.tiff',height = 2800,width = 6000,res= 600)
forestplot(labeltext = as.matrix(longforest[,1:3]),mean = longforest$V4,lower = longforest$V5,upper = longforest$V6,is.summary=c(T,F,F,F,F,F,F,F,F,F,F),zero = 0,boxsize = 0.3,lineheight = unit(8,'mm'),colgap = unit(2,'mm'),lwd.zero = 2,lwd.ci = 2,col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),lwd.xaxis=2,lty.ci = "solid",graph.pos = 3)
dev.off()
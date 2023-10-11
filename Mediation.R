library(mediation)
scaled_BP<-lapply(WMH_BP, scale)
b <- lm(SBP~BMI+Sex+Age, data=scaled_BP)
c <- lm(WMH_log~SBP+BMI+Sex+Age, data=scaled_BP)
contcont <- mediate(b, c, sims=50, treat="BMI", mediator="SBP", robustSE = TRUE)
summary(contcont)
contcont

PathA<-lm(SBP~BMI+Sex+Age, data=scaled_BP)
summary(PathA)
confint(PathA)

PathB<-lm(WMH_log~SBP+Sex+Age, data=scaled_BP)
summary(PathB)
confint(PathB)
library(lmtest)
library(gee)
par(mfrow=c(1,1))
table(factor(HTN), factor(rs174548))
fisher.test(factor(HTN), factor(rs174548))

htnsnplr<-glm(HTN~rs174548, family=binomial)
summary(htnsnplr)
exp(htnsnplr$coef)
exp(confint(htnsnplr))

htntglr<-glm(HTN~TG, family=binomial)
summary(htntglr)
exp(htntglr$coef)
exp(10*htntglr$coef) # this gives me the odds ratio for getting hypertension for someone who's triglycerides are higher by  10 mg/dl 
exp(confint(htntglr))

# Association between HTN and rs174548 adjusted for TG, what role does TG play
htntgsnp<-glm(HTN~rs174548+TG, family=binomial)
summary(htntgsnp)
# compare htntgsnp model to htnsnp model to see if adding tg is significant/important so use likelihood ratio test
lrtest(htnsnplr, htntgsnp)  # based on o utput where lrtest p value < 0.05, after accounting for snp, there is a significant association between tg and HTN



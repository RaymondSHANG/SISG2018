library(lmtest)
library(gee)
par(mfrow=c(1,1))
table(factor(HTN), factor(rs174548))
fisher.test(factor(HTN), factor(rs174548))

htnsnplr<-glm(HTN~factor(rs174548), family=binomial)
summary(htnsnplr)
exp(htnsnplr$coef)
exp(confint(htnsnplr))

htntglr<-glm(HTN~TG, family=binomial)
summary(htntglr)
exp(htntglr$coef)
exp(10*htntglr$coef) # this gives me the odds ratio for getting hypertension for someone who's triglycerides are higher by  10 mg/dl 
exp(confint(htntglr))

# Association between HTN and rs174548 adjusted for TG, what role does TG play
htntgsnp<-glm(HTN~factor(rs174548)+TG, family=binomial)
summary(htntgsnp)
# compare htntgsnp model to htnsnp model to see if adding tg is significant/important so use likelihood ratio test
lrtest(htnsnplr, htntgsnp)  # based on o utput where lrtest p value < 0.05, after accounting for snp, there is a significant association between tg and HTN

# now look at relative risk model where u use gee (glm w poisson distribution but using gee b/c variances not equal)
relrisk<-gee(HTN~factor(rs174548)+TG, family=poisson, id=seq(1,nrow(cholesterol), data=cholesterol))
summary(relrisk) # since it's a log link it gives  u a log rel risk so still exponentiate it
exp(relrisk$coef)

# now look at risk difference model where u use gee (glm w normal distribution b/c but using gee b/c fitting a linear model for binary data)

riskdif <-gee(HTN~factor(rs174548)+TG, id=seq(1,nrow(cholesterol), data=cholesterol)) # if u dont specify family, default is normal
summary(riskdif)
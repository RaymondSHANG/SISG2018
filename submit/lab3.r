library(gee)
library(lmtest)
library(multcomp)
library(dplyr)
library(ggplot2)
### Load data
cholesterol = read.csv("https://raw.githubusercontent.com/rhubb/SISG2018/master/data/SISG-Data-cholesterol.csv", header=T)

### Simple Linear Regression

str(cholesterol)
head(cholesterol)

attach(cholesterol)



table(cholesterol$rs4775401, cholesterol$APOE)


means <- aggregate(chol ~  as.factor(rs4775401), cholesterol, mean)
ggplot(cholesterol, aes(x = as.factor(rs4775401),
                        y = chol, 
                        color = as.factor(rs4775401))) + 
   geom_boxplot() 

   +stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3) + 
  geom_text(data = means, aes(label = chol, y = chol + 0.08))


ggplot(cholesterol, aes(x = as.factor(APOE),
                        y = chol, 
                        color = as.factor(APOE))) + 
   geom_boxplot() 

ggplot(cholesterol, aes(x = as.factor(APOE),
                        y = chol, 
                        color = as.factor(rs4775401))) + 
   geom_boxplot() 


# ANCOVA including age and SNP
fit1 = lm(chol ~ factor(rs4775401) + factor(APOE))
summary(fit1)


fit2 = lm(chol ~ factor(rs4775401) * factor(APOE))
summary(fit2)

anova(fit1, fit2)



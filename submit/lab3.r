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

# attach(cholesterol)

table(cholesterol$rs4775401, cholesterol$APOE)

# Boxplots 
means <- aggregate(chol ~  as.factor(rs4775401), cholesterol, mean)
colnames(means)[1]<- "rs4775401"
means[,2] <- format(means[,2], digits=5)
ggplot(cholesterol, aes(x = as.factor(rs4775401),
                        y = chol, 
                        fill = as.factor(rs4775401))) + 
	geom_boxplot() + 
	stat_summary(fun.y=mean, colour="darkred", geom="point", 
	shape=18, size=3) +
	geom_text(data = means, aes(label = chol, y = as.numeric(chol) + 2)) +
	xlab("rs4775401") +
	ggtitle("cholesterol vs. rs4775401")


means <- aggregate(chol ~  as.factor(APOE), cholesterol, mean)
colnames(means)[1]<- "APOE"
means[,2] <- format(means[,2], digits=5)
ggplot(cholesterol, aes(x = as.factor(APOE),
                        y = chol, 
                        fill = as.factor(APOE))) + geom_boxplot() + 
   stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)+
	geom_text(data = means, aes(label = chol, y = as.numeric(chol) + 2))+
	xlab("APOE") +
	ggtitle("cholesterol vs. APOE")



ggplot(cholesterol, aes(x = as.factor(APOE),
                        y = chol, 
                        fill = as.factor(rs4775401))) + 
xlab("APOE genotype")+
geom_boxplot() 


# ANCOVA including age and SNP
fit1 = lm(chol ~ factor(rs4775401) + factor(APOE))
summary(fit1)


fit2 = lm(chol ~ factor(rs4775401) * factor(APOE))
summary(fit2)

anova(fit1, fit2)



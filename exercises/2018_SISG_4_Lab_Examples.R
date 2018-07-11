### Install packages (only need to do this once)
install.packages("gee")
install.packages("multcomp")
install.packages("lmtest")

### Load data
cholesterol = read.csv("https://raw.githubusercontent.com/rhubb/SISG2018/master/data/SISG-Data-cholesterol.csv", header=T)

### Simple Linear Regression

str(cholesterol)
head(cholesterol)

attach(cholesterol)

# Some descriptive statistics
summary(chol)
summary(age)
group = 1*(age > 55)
group=factor(group,levels=c(0,1), labels=c("30-55","56-80"))
table(group)
by(chol, group, mean)
by(chol, group, sd)
mean(chol[group=="30-55"]) # identify a subset of the data using []
mean(chol[group=="56-80"])
sd(chol[group=="30-55"])
sd(chol[group=="56-80"])

# Boxplot and scatter plot
boxplot(chol~group,ylab="Total cholesterol(mg/dl)")
plot(chol ~ age, xlab = "Age (years)", ylab = "Total cholesterol (mg/dl)")

# Fit linear regression model
fit = lm(chol ~ age)
summary(fit)
confint(fit)

# Prediction of the mean
predict(fit, newdata=data.frame(age=c(46,47,48)), interval="confidence")

# Prediction of a new individual value
predict(fit, newdata=data.frame(age=c(46,47,48)), interval="prediction")

# ANOVA table
anova(fit)

### Linear regression assumptions and diagnostics

# R's standard linear regression diagnostic plots
par(mfrow = c(2,2)) # create a 2 row x 2 column grid of plots
plot(fit)

# Create individual plots by hand
par(mfrow = c(1,1))
## Fitted values vs. residuals
plot(fit$fitted, fit$residuals, xlab = "Fitted values", ylab = "Residuals")
abline(0,0, col = "red") # plot reference line

## QQ plot
qqnorm(fit$residuals, main = "QQ Plot")
qqline(fit$residuals, col = "red") # plot reference line

# Load GEE library for robust standard errors
library(gee)

# Refit our regression model using GEE to obtain empirical standard errors
fit.ese <- gee(chol ~ age, id = seq(1, length(age)))
summary(fit.ese)
beta = fit.ese$coef # Extract coefficients
se = sqrt(diag(fit.ese$robust.variance)) # Extract covariance matrix, take diagonal elements, and square root
p = 2*(1-pnorm(abs(beta)/se))
p

dfb = dfbeta(fit) # create delta-betas for fit
index=order(abs(dfb[,2]),decreasing=T) # sort dfb with largest delta betas at top
cbind(dfb[index[1:15],],age[index[1:15]])

### Multiple linear regression 

# Model with main effects only
fit2=lm(chol~age+sex) 
summary(fit2)

# Model with main effects and interaction term
fit3=lm(chol~age*sex) 
summary(fit3)
anova(fit2,fit3)

### ANOVA

# Creating dummy variables by hand
dummy1 = 1*(rs174548==1)
dummy2 = 1*(rs174548==2)

fit0 = lm(chol ~ dummy1 + dummy2)
summary(fit0)
anova(fit0)

# Using factor() to create dummy variables
fit1.1 = lm(chol ~ factor(rs174548))
summary(fit1.1)
anova(fit1.1)

# Model without intercept
fit1.2 = lm(chol ~ -1 + factor(rs174548)) 
summary(fit1.2)

# An alternative function for ANOVA
fit1.3 = aov(chol ~ factor(rs174548))
summary(fit1.3)
anova(fit1.3)
fit1.3$coeff

# Linear dose-response model
fit2 = lm(chol ~ rs174548)
summary(fit2)
anova(fit2)

# Relax assumption of equal variance
fit.oneway = oneway.test(chol ~ factor(rs174548))
fit.oneway

# Use robust standard errors
fit.gee = gee(chol ~ factor(rs174548), id=seq(1,length(chol)))
summary(fit.gee)

# Kruskal-Wallis (non-parametric) test
fit.kw = kruskal.test(chol ~ factor(rs174548))
fit.kw
### Multiple comparisons

library(multcomp)

# Fit model without intercept (ANOVA parameterization)
fit1 = lm(chol ~ -1 + factor(rs174548)) 

# All pairwise comparisons
M1 = contrMat(table(rs174548), type="Tukey")

# Construct contrast matrix by hand for two comparisons
M2 = rbind("mean(C/G) - mean(C/C)" = c(-1, 1, 0),"mean(G/G) - mean(C/C)" = c(-1, 0, 1))

mc1 = glht(fit1, linfct =M1)
summary(mc1, test = adjusted("none")) # no adjustment for multiple comparisons
summary(mc1, test = adjusted("bonferroni")) # bonferroni adjustment for multiple comparisons
summary(mc1, test = adjusted("fdr")) # FDR adjustment for multiple comparisons

mc2 = glht(fit1, linfct =M2)
summary(mc2, test = adjusted("none")) # no adjustment for multiple comparisons
summary(mc2, test = adjusted("bonferroni")) # bonferroni adjustment for multiple comparisons
summary(mc2, test = adjusted("fdr")) # FDR adjustment for multiple comparisons

### Two-way ANOVA

# One-way ANOVA including main effect of sex
fit0 = lm(chol ~ factor(sex))
summary(fit0)

# Two-way ANOVA including main effects of sex and SNP
fit1 = lm(chol ~ factor(sex) + factor(rs174548))
summary(fit1)

# Compare model with sex only to model including sex and SNP
anova(fit0,fit1)

# Two-way ANOVA with interaction between sex and SNP
fit2 = lm(chol ~ factor(sex) * factor(rs174548))
summary(fit2)

# Compare models with and without interaction
anova(fit1,fit2)

### ANCOVA

# Linear model including age only
fit0 = lm(chol ~ age)
summary(fit0)

# ANCOVA including age and SNP
fit1 = lm(chol ~ factor(rs174548) + age)
summary(fit1)

# ANCOVA including age, SNP, and interaction
fit2 = lm(chol ~ factor(rs174548) * age)
summary(fit2)

# Test of coincident lines
anova(fit0,fit2)

# Test of parallel lines
anova(fit1,fit2)

plot(age[rs174548==0], chol[rs174548==0], xlab = "Age (years)", ylab = "Cholesterol (mg/dl)")
points(age[rs174548==1], chol[rs174548==1], col = "red")
points(age[rs174548==2], chol[rs174548==2], col = "blue")
abline(fit2$coef[1],fit2$coef[4])
abline((fit2$coef[1]+fit2$coef[2]),(fit2$coef[4]+fit2$coef[5]), col = "red")
abline((fit2$coef[1]+fit2$coef[3]),(fit2$coef[4]+fit2$coef[6]), col = "blue")
legend(30,230,lty = 1, col = c("black","red","blue"),legend = c("C/C","C/G","G/G"))

## mean cholesterol for different genotypes adjusted by age
predict(fit1, new=data.frame(age=mean(age),rs174548=0))
predict(fit1, new=data.frame(age=mean(age),rs174548=1))
predict(fit1, new=data.frame(age=mean(age),rs174548=2))

### Logistic Regression

# Logistic regression for association between CHD and cholesterol
glm.mod1  <- glm(chd ~ chol, family = "binomial")
summary(glm.mod1)
# Exponentiate coefficients to obtain odds ratios
exp(glm.mod1$coef)
exp(confint(glm.mod1))

# Odds ratios for a 10 mg/dl difference in cholesterol
exp(10*glm.mod1$coef)

# Logistic regression including two predictor variables
glm.mod2 <- glm(chd ~ chol+factor(rs4775401), family = "binomial", data = cholesterol)
summary(glm.mod2)
exp(glm.mod2$coef)
exp(confint(glm.mod2))

# Load lmtest library for likelihood ratio tests
library(lmtest)

# Compare models with and without SNP
lrtest(glm.mod1,glm.mod2)

### Relative risk regression

glm.rr <- gee(chd ~ chol+factor(rs4775401), family = "poisson", id = seq(1,nrow(cholesterol)), data = cholesterol)
summary(glm.rr)
exp(glm.rr$coef)

### Risk difference regression
glm.rd <- gee(chd ~ chol+factor(rs4775401), id = seq(1,nrow(cholesterol)), data = cholesterol)
summary(glm.rd)

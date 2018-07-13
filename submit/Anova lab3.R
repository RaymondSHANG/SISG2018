## LAB3
# 1. Is rs4775401 associated with cholesterol levels
# 2. are rs174548 and APOE associated with cholesterol levels
# 3 Does the effect of APOE on cholesterol levels depend on rs174548

# descriptive analysis of the questions numerically  and graphically
bothmeansd<-function(x){
     c(mean(x), sd(x))
} #   here y ou are defining a function of something called x, which gives u the mean and sd of x

tapply(chol, factor(rs4775401), bothmeansd)  ## tapply gives u  a table of a variable, by a  group, that has the cell containing the function u asked it to do on that variable by  group
tapply(chol, factor(APOE), bothmeansd)
tapply(chol, factor(rs174548), bothmeansd)
tapply(chol1,list(rs174548, APOE), bothmeansd)  ## this gives the 3D table of the 2 categorical variables on the row and column, and the mean and SD of the chol level in the middle
boxplot(chol~factor(APOE))
boxplot(chol~factor(rs174548)+factor(APOE))

mod1=lm(chol~ -1+factor(rs174548)) ## this just tests whether all the means are 0 so u cant interpret this model 
# i.e. cant interpret the hypothesis tests when u use a model with  -1 which gets rid of the intercept
summary(mod1)
anova(mod1)
matrix1<- contrMat(table(rs174548), type="Tukey")
multcomp<-glht(mod1, linfct=matrix1)
summary(multcomp, test=adjusted("bonferroni"))
summary(multcomp, test=adjusted("BY")) ## benjamin-yekutieli test - more conservative than FDR but less than bonferroni, good for targeted SNPs
summary(multcomp, test=adjusted("FDR"))

oneway.test(chol~factor(rs174548))  ## is there an effect of the SNP on cholesterol assuming unequal variance
fit.gee=gee(chol~factor(rs174548),id=ID) ## id is r's language for the unique variable identifier, gee gives u the same output as the linear regression model WITHOUT robust SE
summary(fit.gee) # this gives u the z values not the p value
p=2*(1-pnorm(abs(fit.gee$coef)/sqrt(diag(fit.gee$robust.variance))))
p
kruskal.test(chol~factor(rs174548))

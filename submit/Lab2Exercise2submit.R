
##read in data
cholesterol <- read.csv("https://raw.githubusercontent.com/rhubb/SISG2018/master/data/SISG-Data-cholesterol.csv", header =T)

#attach tells R that you're referring to a specific dataset, can just use headers from that dataset from now on
attach(cholesterol)

#plot assoc btw TG and BMI. Does there appear to be an assoc?
plot(TG~BMI)

#use linear regression to investigate assoc btw TG and BMI
#null hypothesis is that slope = 0
fit = lm(TG~BMI)
summary(fit)
confint(fit)
abline(a=-208.5, b=15.44, col= "red")

# Exercise 2: Conduct a residuals analysis (using all data) to check the linear regression model assumptions. Do any modeling assumptions appear to be violated? How do model results change if you use robust standard errors?

# R's standard linear regression diagnostic plots: plot(fit) will automatically generate plot of four diagnostics
par(mfrow = c(2,2)) # create a 2 row x 2 column grid of plots (rather than needing to scroll through them one at a time)
plot(fit)

# Load GEE library for robust standard errors- need to load this each time you want to use it
library(gee)
fit.ese <- gee(TG ~ BMI, id = seq(1, length(BMI)))
summary(fit.ese)

dfb = dfbeta(fit) # create delta-betas for fit
index=order(abs(dfb[,2]),decreasing=T) # sort dfb with largest delta betas at top
cbind(dfb[index[1:15],],BMI[index[1:15]], TG[index[1:15]]) #pull out top ones and look at them



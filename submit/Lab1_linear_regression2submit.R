
#install.packages("gee")
#install.packages("multcomp")
#install.packages("lmtest")

library(gee)
library(multcomp)
library(lmtest)

cholesterol = read.csv("https://raw.githubusercontent.com/rhubb/SISG2018/master/data/SISG-Data-cholesterol.csv", header=T)

##### Regression Methods Lab 1 #####

attach(cholesterol) #allows you to be able to access the variables in cholesterol directly instead of cholesterol$

# 1. Provide descriptive statistics summarizing triglycerides and BMI.

summary(BMI)
summary(TG)

# 2. Create plots illustrating the relationship between triglycerides and BMI. Based on your graphical summaries doesthere appear to be an association between triglycerides and BMI?

group = 1*(BMI > 24.6) # separated at the median
group = factor(group, levels = c(0,1), labels = c("Below Median","Above Median"))
hist(BMI)
hist(TG)

boxplot(TG~group, ylab="TG",xlab="BMI Group")
t.test(TG~group)
plot(TG~BMI)


# 3. Use linear regression to investigate the association between triglycerides and BMI. What do the linear regression model results tell us about the association? Make sure you can interpret the model coefCcients and any hypothesis testing.
# 4. Compute the predicted value and its 95% conCdence interval for the mean value of triglycerides at BMI = 23 as well as for a new individual with BMI = 23. How do these two intervals differ and why?



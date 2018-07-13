# Logistic regression for association between rs174548 and HTN, adjusted for TG
glm.mod3  <- glm(HTN ~ TG+factor(rs174548), family = "binomial")

# Exponentiate coefficients to obtain odds ratios
exp(glm.mod3$coef)

## Use a GLM to estimate the relative risk of hypertension for patients with different rs174548 genotypes, 
## adjusting for triglyceries. Make sure you can interpret the coefficients. 
#How do these results compare to the results of the logistic regression analysis?

### Relative risk regression
glm.rr <- gee(HTN ~ TG+factor(rs174548), family = "poisson", id = seq(1,nrow(cholesterol)), data = cholesterol)
summary(glm.rr)
exp(glm.rr$coef)

#Compare with results from logistic reg
exp(glm.mod3$coef)

#5. Use a GLM to estimate the risk difference for hypertension according to rs174548 genotypes, 
#adjusting for triglyceries. Make sure you can interpret the coef=cients. 
#How do these results compare to the results of the logistic regression and relative risk regression analyses?

### Risk difference regression
glm.rd <- gee(HTN ~ TG+factor(rs174548), id = seq(1,nrow(cholesterol)), data = cholesterol)
summary(glm.rd)

#Compare with results from logistic reg
exp(glm.mod3$coef)
#compare with results from relative risk reg
exp(glm.rr$coef)
################################################################################
# Steps to Complete Computer Exercises
# Applied Econometrics
# Chapter 8:  Heteroskedasticity
# Author: Imran Arif
################################################################################
# Example 8.1
data(wage1, package='wooldridge')

# Create group categories. See example 7.6
wage1$male <- as.integer(!wage1$female)
wage1$single <- as.integer(!wage1$married)

wage1$marrmale <- wage1$male*wage1$married
wage1$marrfem <- wage1$female*wage1$married
wage1$singfem <- wage1$female*wage1$single

attach(wage1)

# Estimate the model
reg8.1 <- lm(lwage ~ marrmale + marrfem + singfem + educ + exper + I(exper^2) + tenure + I(tenure^2)) # how to create multiple DV
summary(reg8.1)

# You two packages to get heteroscedasticity-robust standard errors
library(lmtest); library(car)

# Usual standard errors
coeftest(reg8.1)

# Robust standard errors using heteroscedasticity-corrected covariance matrices
coeftest(reg8.1, vcov = hccm(reg8.1, type = "hc0"))

# The two sets of standard errors are not very different.
# The largest relative change in S.E. is for the coefficient on educ:
# the usual standard error is 0.0067, and the robust standard error is 0.0074.
# The robust standard errors can be either larger or smaller than the usual standard errors.
# The robust standard error on exper is 0.0051, whereas the usual standard error is 0.0052.
# The robust standard errors are often larger than the usual standard errors.

#################
# Example 8.2

data(gpa3, package='wooldridge')
attach(gpa3)

# Estimate the model (only for spring data)
reg8.2 <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, subset = (spring == 1))
summary(reg8.2)

library(lmtest); library(car)

# White heteroscedasticity-robust SE:
coeftest(reg8.2, vcov = hccm(reg8.2, type = "hc0")) # heteroscedasticity-corrected covariance matrices

# F-Tests using different variance-covariance formulas:
myH0 <- c("black","white")

# Usual F-test
linearHypothesis(reg8.2, myH0)

# Heteroscedasticity-robust F-test with Classical White VCOV (type = "hc0")
linearHypothesis(reg8.2, myH0, vcov = hccm(reg8.2, type = "hc0"))

#################
# Example 8.3
data(crime1, package = 'wooldridge')
attach(crime1)

# Estimate restricted model:
reg8.3 <- lm(narr86 ~ pcnv + avgsen + I(avgsen^2) + ptime86 + qemp86 + inc86 + black + hispan)
summary(reg8.3)

# White heteroscedasticity-robust SE:
coeftest(reg8.3, vcov = hccm(reg8.3, type = "hc0")) # heteroscedasticity-corrected covariance matrices

myH0 <- c("avgsen", "I(avgsen^2)")

# Usual F-test
linearHypothesis(reg8.3, myH0)

# Heteroscedasticity-robust F-test with Classical White VCOV (type = "hc0")
linearHypothesis(reg8.3, myH0, vcov = hccm(reg8.3, type = "hc0"))

################################################################################
# Testing for Heteroskedasticity

#################
# the Breusch-Pagan test for heteroskedasticity (BP test)

# Example 8.4
data(hprice1, package = 'wooldridge')
attach(hprice1)

# Estimate the level model
reg8.17 <- lm(price ~ lotsize + sqrft + bdrms)
summary(reg8.17)

# Estimate the log-log model
reg8.18 <- lm(lprice ~ llotsize + lsqrft + bdrms)
summary(reg8.18)

# Calculate the square of residuals
res8.17 <- resid(reg8.17)^2

# Manual regression of squared residuals
summary(lm(res8.17 ~ lotsize + sqrft + bdrms))

# BP test
library(lmtest)
bptest(reg8.17)

# The associated p-value is 0.002, which is strong evidence against the null.
# We conclude that there is heteroscedasticity in the model

# BP test
bptest(reg8.18)

# The associated p-value is 0.23, we reject the null hypothesis.
# We conclude that our model shows homoscedasticity.

#################
# The White test for heteroskedasticity

# Example 8.5

# for level-level model
bptest(reg8.17, ~ fitted(reg8.17) + I(fitted(reg8.17)^2))

# The associated p-value is 0.002, which is strong evidence against the null.
# We conclude that there is heteroscedasticity in the model

# for log-log model
bptest(reg8.18, ~ fitted(reg8.18) + I(fitted(reg8.18)^2))

# The associated p-value is 0.17, we reject the null hypothesis.
# We conclude that our model shows homoscedasticity.

################################################################################
# Weighted least squares (WLS) - When heteroscedasticity is known
library(lmtest); library(car)

# Example 8.6
data(k401ksubs, package = 'wooldridge')
attach(k401ksubs)

reg86a <- lm(nettfa ~ inc,
             subset = (fsize == 1))
# Robust S.E.
coeftest(reg86a, vcov = hccm(reg86a, type = "hc0"))

reg86b <- lm(nettfa ~ inc,
             weight = 1/inc,
             subset = (fsize == 1))

reg86c <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k,
             subset = (fsize == 1))

# Robust S.E.
coeftest(reg86c, vcov = hccm(reg86c, type = "hc0"))

reg86d <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k,
             weight = 1/inc,
             subset = (fsize == 1))

library(stargazer)
stargazer(reg86a, reg86b, reg86c, reg86d,
          column.labels = c("OLS", "WLS" , "OLS", "WLS"),
          type = "text",
          keep.stat = c("n","rsq"))

# Without controlling for other factors, another dollar of income is estimated
# to increase nettfa by about 82¢ when OLS is used; the WLS estimate is smaller,
# about 79¢. The difference is not large; we certainly do not expect them to be identical.
# The WLS coefficient does have a smaller standard error than OLS, almost 40% smaller,
# provided we assume the model is correct.
# Adding the other controls reduced the inc coefficient somewhat, with the OLS estimate still
# larger than the WLS estimate. Again, beta_inc for the WLS estimate is more precise.

################################################################################

# Feasible GLS (FLGS) - When heteroscedasticity is unknown

# Example 8.7
data(smoke, package = 'wooldridge')
attach(smoke)

# Step 1: Estimate the model
reg8.35 <- lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn)
summary(reg8.35)

# BP test
library(lmtest)
bptest(reg8.35)

# The p-value is less than 0.000015, which is very strong evidence of heteroskedasticity.

# Obtain the residuals from step 1:
u_hat <- resid(reg8.35)

# Step 2: Sqaure the residuals and take the natural log.
log_u_hatsqr <- log((u_hat)^2)

# Step 3: Regress the residual squares on the explanatory variables
reg_u_hat <-lm(log_u_hatsqr ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn)

# Obtain the fitted value from the step 3
g_hat <- fitted(reg_u_hat)

# Step 4: Exponetiate these fitted values
h_hat <- exp(g_hat)

# create the weights
W <- 1/h_hat

# Step 5: Estimate the model by using "W" weights.
reg8.36_fgls <- lm(cigs ~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn,
                   weight = W)

library(stargazer)
stargazer(reg8.35, reg8.36_fgls,
          column.labels = c("OLS S.E.", "FGLS"),
          type = "text",
          keep.stat = c("n","rsq"))

# OLS results:
# Neither income nor cigarette price is statistically significant in column 1, and their effects are not practically large.
# Each year of education reduces the average cigarettes smoked per day by one-half of a cigarette, and the effect is statistically significant.
# Cigarette smoking is also related to age, it increases with a decreasing rate.
# The presence of a restriction on smoking in restaurants decreases cigarette smoking by almost three cigarettes per day, on average.
#
# FGLS results:
# The income effect is now statistically significant and larger in magnitude.
# The price effect is also notably bigger, but it is still statistically insignificant.
#
# Conclusion:
#  Cigarette smoking is negatively related to schooling, has a quadratic relationship with age, and is negatively affected by restaurant smoking restrictions.

################################################################################
# What if heteroscedasticity function is wrong?

# Table 8.2
data(k401ksubs, package = 'wooldridge')
attach(k401ksubs)

regT8.2 <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k,
              weight = 1/inc,
              subset = (fsize == 1))

summary(regT8.2)

library(lmtest); library(car)

# White S.E.
coeftest(regT8.2, hccm(regT8.2, type = "hc0"))

################################################################################
# Linear probability model revisited

# Example 8.8
data(mroz, package = 'wooldridge')
attach(mroz)

reg8.8 <- lm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6)
summary(reg8.8)

library(lmtest); library(car)

# White S.E.
coeftest(reg8.8, hccm(reg8.8, type = "hc0"))

# Several of the robust and OLS standard errors are the same;  The differences are practically very small.
# While heteroskedasticity is a problem in theory, it is not in practice, at least not for this example.

#################
# Example 8.9

data(gpa1, package = 'wooldridge')

gpa1$parcoll <- ifelse(fathcoll == 1 | mothcoll == 1, 1, 0)

attach(gpa1)

# Estimate the model
reg8.9 <- lm(PC ~  hsGPA + ACT + parcoll)
summary(reg8.9)

library(lmtest); library(car)

# White S.E.
coeftest(reg8.9, hccm(reg8.9, type = "hc0"))

# There are no striking differences between the usual and robust standard errors.

# Let's estimate this model by WLS.

 y_hat <- fitted(reg8.9)
summary(y_hat)

h_hat <- y_hat*(1 - y_hat) # Calculate h(x)
W <- 1/h_hat # Create weights

# Estimate the model by WLS.
reg8.9WLS <- lm(PC ~  hsGPA + ACT + parcoll,
                weights = W)
summary(reg8.9WLS)

library(stargazer)
stargazer(reg8.9, reg8.9WLS,
          column.labels = c("OLS S.E.", "FGLS"),
          type = "text",
          keep.stat = c("n","rsq"))

# There are no important differences in the OLS and WLS estimates. The only significant explanatory variable is parcoll, and in both cases, we estimate that the probability of PC ownership is about 0.22 higher if at least one parent attended college.

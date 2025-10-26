library(wooldridge)
library(lmtest)
library(sandwich)
library(mfx) # this package does marginal (partial) effects
library(pscl) # this package does Vuong model selection test


# Data from package "wooldridge" that contains information on about 32,000 mothers
# with 2 or more kids:
data('labsup')
summary(labsup)

# This is the model for choosing to work that we settled on in class:
model<-worked~kids+age+educ+black+nonmomi

# OLS estimates of Linear Probability Model with HR standard errors:
LPM<-lm(model,data=labsup)
coeftest(LPM,vcov.=vcovHC) # Try to interpret coefficients: can you do it?


#---------------------------Probit and Logit estimation--------------------------------------------

# ML estimates of Probit model ("x=TRUE" part is needed for calculating APE and PEA):
Probit<-glm(model,data=labsup,family=binomial(link="probit"),x=TRUE)
summary(Probit) # Look at the signs and significance and compare to LPM. Try to interpret coefficients: can you do it?
logLik(Probit) #value of log-likelihood function

# ML estimates of Logit model ("x=TRUE" part is needed for calculating APE and PEA):
Logit<-glm(model,data=labsup,family=binomial(link="logit"),x=TRUE)
summary(Logit) # Look at the signs and significance and compare to LPM. Try to interpret coefficients: can you do it?
logLik(Logit)

#-------------------------------------------Partial Effects-----------------------------------

# Partial (Marginal) Effects using package mfx:
probitmfx(Probit,data=loanapp,atmean=FALSE) #APE
probitmfx(Probit,data=loanapp,atmean=TRUE) #PEA

logitmfx(Logit,data=loanapp,atmean=FALSE) #APE
logitmfx(Logit,data=loanapp,atmean=TRUE) #PEA

# Compare these Partial Effects (PEs) to LPM estimates (both magnitude and statistical significance): is there a big difference?
# Do you think that you can interpret these PEs now?

#----------------------------------------Vuong's MST-----------------------------------------------

# Check out function "vuong" in package "pscl": we will compare Probit and Logit models

vuong(Probit,Logit) 

# Look at the p-value: do we reject the null hypothesis that the two models are indistinguishable
# against the alternative that model1 (Probit) is better than model2 (Logit) at the 5% significance level? 

# Now let's switch the order: model1 is now Logit, and model2 is Probit:
vuong(Logit,Probit)

# Look at the p-value: do we reject the null hypothesis that the two models are indistinguishable
# against the alternative that model2 (Probit) is better than model1 (Logit) at the 5% significance level? 

#---------------------------Goodness-of-Fit that can be compared to LPM---------------------------------

# Running line 63 will predict x'beta rather than the probability:
labsup$ProbitPred<-predict(Probit) #check out variable ProbitPred: some values are above 1 or below 0

# Running line 66 will predict x'beta rather than the probability:
labsup$ProbitPred<-predict(Probit,type="response") #check out variable ProbitPred: all values are strictly between 0 and 1

# Now let's do the same for Logit:
labsup$LogitPred<-predict(Logit,type="response")

# Here's the Pseudo-R-squared for Probit and Logit models:
Probit.R2<-(cor(labsup$worked,labsup$ProbitPred)^2)
Logit.R2<-(cor(labsup$worked,labsup$LogitPred)^2)

# Compare these values to each other and to R-squared from LPM estimation

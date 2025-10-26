data('wagepan')

# Let's look at our panel:
summary(wagepan) 

# We want to estimate the causal effect of union membership on wage.
# Here is the model we settled on in class:
model<-lwage~union+exper+hours+married+educ+poorhlth+black+hisp

# Keep track of the coefficient for union variable:

# Pooled OLS estimates:
POLS<-plm(model,data=wagepan,index=c('nr','year'),model='pooling')
summary(POLS)

# Random Effects estimates:
RE<-plm(model,data=wagepan,index=c('nr','year'),effect='individual',model='random')
summary(RE)

# Fixed Effects estimates:
FE<-plm(model,data=wagepan,index=c('nr','year'),effect='individual',model='within')
summary(FE) #notice how some of the variables (educ, black, hisp) disappeared

# First-Difference estimates:
FD1<-plm(model,data=wagepan,index=c('nr','year'),effect='individual',model='fd')
summary(FD1) #note how exper variable disappeared, too. Reason: it's the same as time trend, which is captured by the intercept now

# First-Difference estimates without a time trend
FD2<-plm(lwage~union+exper+hours+married+educ+poorhlth+black+hisp-1,data=wagepan,index=c('nr','year'),effect='individual',model='fd')
summary(FD2) # no intercept now, but still no exper variable. Something is not right with 'fd' option...



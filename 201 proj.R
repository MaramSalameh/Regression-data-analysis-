# ISE 201 Semester Project
#Salameh

install.packages("leaps")
install.packages("MASS")
library(MASS)
library(leaps)

#read and import dataset
data_set = read.csv('ISE201_ProjectData_S2019_E.CSV')
data_set
attach(data_set)
plot(data_set)
# data_set consists of 90 obsevartions and 10 variables

#convert variables to factors/categorical
#data_set$TYPE = factor(data_set$TYPE)
#data_set$CH.SHAPE = factor(data_set$CH.SHAPE)
#data_set$CH.LINER = factor(data_set$CH.LINER)
#data_set$HOUSE = factor(data_set$HOUSE)
#data_set$DAMPER = factor(data_set$DAMPER)

## check for correlation: 
correlation = cor(data_set)
correlation

#Fit Multiple linear regression model for predicting 
#energy consumption 
lm.fit=lm(Effectivity~.-BTU.IN - BTU.OUT,data=data_set)
#lm.fit=lm(BTU.IN~ +TYPE,data=data_set)


#lm.fit=lm(BTU.OUT~.- Effectivity - BTU.IN,data=data_set)
lm.fit
summary(lm.fit)

anova_model = aov(Effectivity~.-BTU.IN - BTU.OUT,data=data_set)
summary(anova_model)


#Best subsets regression
# function for best subsets is regsubsets()

best.subset = regsubsets(Effectivity~.-BTU.IN -BTU.OUT, data=data_set)
best.subset.summary = summary(best.subset)
best.subset.summary
best.subset.by.adjr2 = which.max(best.subset.summary$adjr2)
best.subset.by.adjr2   #returns the model that has the highest Adjusted_R^2

best.subset.by.Cp = which.min(best.subset.summary$cp)
best.subset.by.Cp   #returns the model that has the lowest Cp


best_modelR2 = lm(Effectivity~ +CH.AREA+ CH.HT+CH.LINER+DAMPER ,data=data_set)
summary(best_model)

best_modelcp = lm(Effectivity~ +CH.AREA+CH.LINER+DAMPER ,data=data_set)
summary(best_model)
###plot eff vs TYpE
#plot(Effectivity, TYPE)
# stepwise Regression (stepAIC() function in MASS package)

#Damper IN
#smallest_model<-lm(BTU.IN~. - BTU.OUT,data=data_set)
#m2 = stepAIC(lm(BTU.IN~.- BTU.OUT,data=data_set),scope=smallest_model,direction="backward",steps=1000)
#summary(m2)
#m2$anova

#m3 = stepAIC(lm(BTU.IN~.,data=data_set),direction="both",steps=1000)
#summary(m3)
#m3$anova


#Damper OUT
#smallest_model = lm(BTU.OUT~1 -BTU.IN,data=data_set)
#m2.1=stepAIC(lm(BTU.OUT~.-BTU.IN,data=data_set),scope=smallest_model,direction="backward",steps=1000)
#summary(m2.1)
#m2.1$anova

#m3.1=stepAIC(lm(BTU.OUT~.-BTU.IN,data=data_set),direction="both",steps=1000)
#summary(m3.1)
#m3.1$anova



## check for normality: shapiro test

### if p-value > 0.05 implying that the distribution 
#of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.

#shapiro.test(TYPE)  
#shapiro.test(CH.AREA)
#shapiro.test(CH.SHAPE)
#shapiro.test(SH.HT)
#shapiro.test(CH.LINER)
#shapiro.test(HOUSE)
#shapiro.test(AGE)
#shapiro.test(BTU.IN) ### normally distributed
#shapiro.test(BTU.OUT) ### normally distibuted
#shapiro.test(DAMPER)


### plots
#plot(data_set)
#pairs(data_set)
##plot(BTU.IN,AGE)


## histograms
hist(TYPE) ##skewed
hist(BTU.OUT)
hist(CH.AREA)
##### transform variables so that they have a normal distribution
#install.packages('rcompanion')
#library(rcompanion)

#T_tuk = transformTukey(TYPE, plotit=FALSE)
#plotNormalHistogram(T_tuk)

#T_tuk_H = transformTukey(HOUSE,plotit=FALSE)
#plotNormalHistogram(T_tuk_H)

###normal qq plots
#qqnorm(CH.AREA)
#qqline(CH.AREA, col="red")


###boxplots:
boxplot(BTU.OUT) ## outlier
#boxplot(BTU.IN) ### no outliers
#boxplot(DAMPER) ##no outliers
#boxplot(AGE) ## no outliers
boxplot(CH.AREA) ## outliers
#boxplot(CH.LINER) ## no outliers
#boxplot(CH.SHAPE) ### no outliers
boxplot(SH.HT)   ## no outliers
boxplot(TYPE) ##outliers
boxplot(HOUSE)  ##outliers

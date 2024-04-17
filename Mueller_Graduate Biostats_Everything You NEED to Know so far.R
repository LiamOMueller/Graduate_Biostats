
## This document is presented by Dr. Liam O'Connor Mueller for the purpose of a lecture demonstration at the University of California, San Diego on the 25th of May 2021

### This document should be used in concert with your lecture notes! This document is to help you separate the R code I demonstrate in class for theory purposes and the R code I demonstrate in class that has practical application. Use this document as a starting guide for the coding in your assignments.


#####The Binomial#####

#For nominal data with two possible outcomes; Alive or Dead, Treatment or refusal, Test positive or test negative ect.

#The parameter pi which we estimate with p = x/n, where x is the number of "successes" and n is the number of trials

#The shape of the binomial can be estimated with the probability mass function:
dbinom(x = ,size = ,prob = ) #Do not worry about the log argument, we will look at that much later

#The cumulative mass function can be explored with:
pbinom(q = ,size = ,prob = ,lower.tail = ) #q is the quantile we are interested in


#####The Poisson#####

#For count data, especially useful with data containing lots of zeros

# The parameter lambda which we estimate with l= sum(Y)/length(Y) where Y is your count data

# The shape of a Poisson distribution can be estimated with the PMF:
dpois(x = ,lambda = )

#The Cumulative mass function:
ppois(q = ,lambda = ,lower.tail = )


#####The Normal#####

#For continuous data with symmetry around a central tendency. The "bell" curve

#The location parameter mu is estimated with mean= sum(Y)/length(Y)
mean(Y)

#The shape parameter sigma is estimated with the standard deviation 
sd(Y)

#The probability density function for a normal is:
dnorm(x = ,mean = ,sd = )

#The cumulative density function for a normal is:
pnorm(q = ,mean = ,sd = ,lower.tail = )

#Z transformations

#There is a special type of normal distribution called the standard normal which we like because it has a mean of 0 and a standard deviation of 1

#We can transform our data to z units:

ZY<- (Y-mean(Y))/sd(Y)
#or
ZY<-scale(x = Y, center = TRUE, scale = TRUE)

#####Central Limits theorem, law of large numbers, and the SE#####

#These two concepts are the foundation of probability. You wont need to do anything explicitly in r with them, just understand the theory.

#The Standard error is the standard deviation of the distribution of sampled means (from the central limit theorem) and is calculated

SE<- sd(Y)/sqrt(length(Y)-1)

#We can use the standard error to determine the confidence intervals around our mean

CI<-mean(Y)+qnorm(p = )*SE #Where P is the upper quantile(s) you want (e.g. p=c(0.025,0.925) when the goal is to determine the 95%CI)


#####t tests#####

#The goal of a t test is to determine the probability of your data given the null hypothesis is true
# P(data|null=TRUE)

#We do this by calculating a t statistic and determining how likely it would be to have that t statistic or greater in magnitude given a certain t distribution.
theta<-#your null hypothesized value
  SE<- #your calculated standard error
  tstat<- (mean(Y)-theta)/SE
p.value<-pt(q = tstat,df = ,lower.tail = )*2 #Lower tail determined by what sign your tstat has. Multiplied by 2 for a two tailed t test(The one you often want to do!).

#Or, if you know what kind of t test you are doing use the shortcuts (only for what are traditionally considered t tests, a test of the mean. We will see later, that t tests are much broader)

#single sample
t.test(x = ,mu= )

#Paired
t.test(x = ,y= ,paired = TRUE)

#Two sample
t.test(x = ,y= )


#####Tests for normality and transformations#####

#When using the normal distribution, we want the mean and standard deviation to be meaningful estimates, and the only way that is true is if the data are (roughly) normally distributed. There are a few tests that we can use to determine if our data are normally distributed

#The "eye ball" test
hist(Y) #Do our data look like a bell?

#The "best" "actual" test, the D'Agostino test. I like it because it tests for multiple possible issues with your data.
install.packages("fBasics") # you only need to do this once, ever.
library(fBasics) # you only need to do this once per session. Run this each time you are restarting R.
#This is a package, it increases the functionality of R.
normalTest(x = Y,method = "da")

?normalTest

#So, your data are not normal. You might want to use a transformation to get the data more resembling a normal.

#Common transformations

LY<-log(Y+1)
#Back transform with
Y<-exp(LY)-1

SY<-sqrt(Y) #As long as all values in Y are >0
#Back transform with
(SY)^2

#Box-Cox transformations

library(MASS)

boxcox(lm(Y~1)) #Generates a plot of the likelihood of a given transformed data set being drawn from a normal distribution. The maximum value would be the best value of lambda given the following transformation Y'= Y^lambda - 1/lambda

#There are plenty of others! Don't be afraid to look up other ones.


#Last ditch effort. If you can't find a transformation to work, the rank transformation is used:
RY<- rank(Y)
#Back transformation
#impossible, that's why we don't like it!


#####Covariation and correlation####

#covariation is how data rise and fall together from a center point (the mean of X and Y).

cov(X,Y)

#correlation is the covariation on the z-transformed data:

cov(scale(X),scale(Y))
cor(X,Y)

#A t test can be used to determine if the r we calculate is different from some null hypothesized value.

cor.test(X,Y)


#####General Linear Models#####

# A linear model where the variation in Y is some function of X

model<-lm(Y~X) #A linear model can be saved as an object
summary(model) #Test if the slope and intercept are different from zero

anova(model) #sums of squares 

names(model) # A lot of useful information is hidden in the model object!

plot(model) #check residuals
#power
ModelR2<- #find this value from the summary function or SSregression over SStotal
  f2<-ModelR2/(1-ModelR2) #useful for power calculation

install.packages("pwr")
library(pwr)

pwr.f2.test(u = ,v = ,f2 = ,sig.level = ,power = ) #Just like the other ones, leave one out to solve for.

#####Plots#####

par(mar=c(1,1)) #Set the number of plots in your window c(how many rows, how many columns)
plot(X,Y) #see https://www.statmethods.net/graphs/index.html for all sorts of help.

abline(model) #draw the slope of your regression


pred.frame<-data.frame(X=seq(from=min(X),to=max(X),length=100))#creating a series of values of X
pred.c<-predict(model,int="confidence",newdata=pred.frame)#  is confidence intervals over  fitted data 
matlines(pred.frame,pred.c) #plot the 95% CI of the model, many graphical parameters can spruce this up.


#####Multiple Linear Regression#####
#Remember the steps!
###1.	Identify the question

###2.	Examine the data

###3.	Chose the appropriate test

###4.	Build a full model
fullmod<-lm(Y~X1+X2+X3)
###5.	Test model assumptions and modify model as necessary

library(car)
vif(fullmod)
?vif
#Remove factors which are collinear
###6.	Use model selection methods and your brain to determine which model should be analyzed

step(Mymod)
#Use AIC to determine "best" model/s
###7.	Examine your final model for problems in your error structure and possible outliers

plot(Mymod)

###8.	Test your hypothesis with the model chosen

summary(Mymod)

###9.	Report the meaningful results
library(QuantPsyc)
lm.beta(Mymod)
###10.	Visualize the model and data

### https://www.datamentor.io/r-programming/3d-plot/


#####ANOVA/ANCOVA/Factorial design#####


#ANOVA all factors are categorical

#Same steps as above, with modifications:

#Use Anova() instead of summary. Anova() is in the car package

library(car)
Anova(model) #This tests which parameters in your model are significant

#To determine which groups are different, use a Tukey test:
aov.mod<-aov(Y~X) #create aov object which is required for TukeyHSD()
TukeyHSD(aov.mod) #Not for ancova, just ANOVA

####Interactions#####

Factormod<-lm(Y~X1*X2) # is shorthand for
Factormod<-lm(Y~X1+X2+X1:X2) # this

VIF(Factormod) #Still important to examine collinearity in you main effects

step(Factormod) # Model selection still useful as well! AIC is an important tool, but make sure the model you choose is relevant to your hypothesis!

Anova(Factormod) #Consider what the p-values mean in the full model. A significant interaction term does not mean that all groups have slopes different from 0.


#####Mixed Effect Models/Random Effects#####

library(lme4)

Random_inter <-lmer(Y~X1 + (1|R)) #where X1 is our fixed effect and R is our random intercept.

Random_slope <-lmer(Y~X1 + (1+X1|R)) #Now both the intercept and slope can vary among groups in the random effect.

Nested_Mod <- lm(Y~X1 + X1/R) #Where R is our hierarchical grouping factor

###Hypothesis testing can be difficult in mixed effect models, especially when considering the error degrees of freedom (go back to your lecture notes!). Sometimes the best choice is a likelihood ratio test between our full model and a null model.

Null_Mod <- lmer(Y~1|R) #A model with no fixed effect, a null model

anova(Random_inter,Null_Mod) # Performs a likelihood rratio test.












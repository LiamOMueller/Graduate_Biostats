
## This document is presented by Dr. Liam O'Connor Mueller for the purpose of a lecture demonstration.

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



#rank transformed t tests

wilcox.test(G1,G2)


#Permutation test of medians

set.seed(26)
G1<-rexp(15,rate = 2)#Make up some non normal data.
G2<-rexp(15,rate = 12)

hist(G1)
hist(G2)


totalgroups<-c(G1,G2)
hist(totalgroups)
abline(v=median(G1),col="lightgreen",lwd=2)
abline(v=median(G2),col="darkolivegreen",lwd=2)
deltamed<-median(G1)-median(G2)# The medinas are different! But is it significant??

#how often would I get a delta median of 0.3686405 or more extreme if the null is true?

group<-c(rep("G1",15),rep("G2",15))
cbind(group,totalgroups)


sampled_medians<-NA

for(i in 1:10000){
  tempdat<-sample(x = totalgroups,size = 30,replace = F)
  sampled_medians[i]<-median(tempdat[1:15])-median(tempdat[16:30])
}

hist(sampled_medians)
abline(v=deltamed,lwd=2,col='darkorchid')
abline(v=-deltamed,lwd=2,col='darkorchid')


(sum(sampled_medians>deltamed)+sum(sampled_medians<-deltamed))/10000 # p value






#####Linear Models#####


modelobject<-lm(Y~X) #build a linear model 
plot(modelobject) #Fast way to test the assumptions of the model
anova(modelobject) #If you meet the assumptions of the model, an anova will give you a p value

TukeyHSD(aov(Y~X)) #Post hoc test should only be performed if the null hypothesis is rejected. Have to use an aov() model object instead of a lm() model object. I don't know why.

summary(modelobject) #Examine specific parameters and calculate r squared

######MLR####
modelobject<-lm(Y~X1+X2...)
#multicollinearity

library(car)
vif(modelobject)# numbers over 5 are bad, numbers over 10 are worse

#AIC
step(modelobject)# smallest AIC is best. models within 2 AIC are considered similar (the choice is yours to make)

Anova(modelobject)#Type 2 sums of squares
summary(modelobject)

#don't do! anova(modelobject) unless you are sure about your sums of squares!

#3d plots
library(rgl)

#More to come here in the next few weeks. Hang tight.


####Mixed effect Models####

library(lme4)

#Random intercept
model<-lmer(Y~ X + (1|block))
Null<-lmer(Y~ (1|block))

#Random intercept and slope

model<-lmer(Y ~ X + (X|block))
null<-lmer(Y ~ (X|block))

#Nested 

model<-lmer(Y~ X + (1|Plate:X)+(1|Plate))
null<-lmer(Y~ (1|Plate:X)+(1|Plate))

#All of the models above can have multiple variables and AIC model selection can still be used.

#Hypothesis testing mixed effect models with likelyhood ratio tests
anova(null,model)

#####GLM####

#Binomial

glmmod<-glm(Y~X,family="binomial")

#Test residuals
#Using the residual deviance, test using a chisquare

pchisq(q =glmmod$deviance,df = "n-k")#If we fail to reject the null that means the residual values are not different from a binomial dist.


summary(glmmod) #The coefficients are in log odds ratios. exponentiation to get slope in odds ratio numbers.

#R squared

# 1-(residual deviance/null deviance)

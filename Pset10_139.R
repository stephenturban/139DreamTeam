
setwd("~/Downloads")
data = read.csv("ABC.csv")


# 1. a
# So let's find the variance! 
# This is just the residual sum of squares divided by the degrees of freedom

data$variance = data$Residual.Sum.of.Squares / data$Degrees.of.Freedom 

data$variance
## So, let's find the adjusted R squared! 
# to find this, we just have  1 - variance/(variance of model with no predictors) 

data$r.squared = 1 - data$variance / (8100 / 27) 

data$r.squared


## Okay, now let's find the BIC 
# The BIC is  nlog(SSRes/n) +plog(n) 
data$BIC = 28*log(data$Residual.Sum.of.Squares  / 28) + (28 - data$Degrees.of.Freedom)*log(28) 



# 1. B 
# So, let's figure out which of the models are best for each of the statisitcs 


# variance  - we want to minimize variance! 
# So, the best models are 
# 1. AC 
# 2. ABC
# 3. AB 

# For adjusted R squared we have 
# 1. AC
# 2. ABC
# 3. AB 


# For BIC
# 1. AC
# 2. B
# 3. AB 





# 1. C) 
# So, let's start with the model that has the highest R^2.
# This is the B model! Great. Let's check to see if its significant using the F-stest 

# this is ((SSR(Reduced) - SSR(Full)) / (DF(reduced) - DF (full)) )/ SSR(Full) /DF(Full) 

# So, this is ( (8100 - 5980) / (27 - 26) ) / (5980 / 26)
f.value = ( (8100 - 5980) / (27 - 26) ) / (5980 / 26)

# We have degrees of freedom (1, 27) 
1 - pf(f.value, 1, 27) 

# So, this looks significant! This means that our new model is a signficantly
# better predictor than the orginal model 

#Okay, now from the B model, we want to go to the next model that has the 
# a higher r-squared. This is AB. Now, let's find the f-statistic to see if this
# is signicicantly differet (or better.) 


f.value = ((5980  - 5500) / (26 - 25)) / ( 5500 / 25) 

1 - pf(f.value, 1, 26) 

# So, since this p-value is greater than .05. We can say that this model is not signficantly
# better than the model with just B. This means that we will just stop at B. 


# No! In this case, based upon the residual sums of squares, we have not found the
# best model (which is AC). So, in this case, we've run into a method that doesn't necessarily
# find the best model 




# 2. a 

setwd("~/Downloads")
gssincome = read.csv("gssincome.csv")

# Set ID equal to Null! 
gssincome$ID = NULL 

# So, let's begin by exploring the data! 
# First, I'm going to look at the assumptions of normality for our predictor. 
hist(gssincome$income)

# oof. This is looking really right skewed. Let's try a log transformation 

log(gssincome$income)

# let's see how this looks now 
hist(log(gssincome$income))


# This isn't bad! Now, let's check a qqplot
qqnorm(log(gssincome$income))
qqline(log(gssincome$income))


# This is looking pretty good! The inferences will be fine anyways because 
# via central limit theorem our estimators will be normally distributed. 


# Now, let's check linearity. Recall that we don't need to check this #
# For any variables that are binaries. So, we are only going to do this for 
# variables that are not binary.
pairs(~log(income) + age + educ + siblings + height +numchildren, gssincome)


# So, I'm looking at the top four right plots. 
#  It looks like the age looks pretty linear as does height. 

# Now let's investigate education and siblings to see if there are outliers 
hist(gssincome$educ)

# This looks roughly symmetric, so I'm going to keep it as is 


# Now let's investigate sibling 
hist(gssincome$siblings)

#Hmm.. this looks pretty right skewed. Let's log this and see what it looks like 
hist(log(gssincome$siblings))

# Looks a little bit better,it has breaks because the original 
# variable siblings is discrete. Let's check out linearity with log(income) 
plot(log(gssincome$siblings), log(gssincome$income))

# this looks much more linear than we have before. So, over all, I'm going 
# to only update the model by logging income and logging siblings. 
gssincome$siblings = log(gssincome$siblings + 1) 

# let's find out which variables are factors 
lapply(gssincome, is.factor)

# select columns to be changed 
cols <- c("race", "religious", "sexorient", "political", "sexpartners", "zodiac")
# change to factor! 
gssincome[,cols] <- data.frame(apply(gssincome[cols], 2, as.factor))


# 2. b
# let's create a linear model with education and race and the interaction 
model1 = lm(log(income)~ race*educ, data=gssincome)
  
# So, what is the slope for black individuals? Well we know that this is the combination 
# of educ + race:2:educ   
slope_black = sum(model1$coefficients[c(4,5)])
slope_black


# What is the slope for other individuals? This is 
slope_other = sum(model1$coefficients[c(4,6)])
slope_other



# 2. C 

# So, let's perform a t-test to see whether there is a significant difference
# between slop_other and slope_black 
gamma_hat = slope_black 
gamma = slope_other
# create the matrix that picks out only the coefficients for education and black 
n = dim(gssincome)[1]
k = length(model1$coefficients)
C = rep(0,6)
C[c(4,5)] = 1 
betas = model1$coefficients


cov = vcov(model1)
T_obs = (gamma_hat-gamma) / sqrt(t(C)%*%cov%*%C)
k = length(betas)
df = n-(k+1)
2*(1-pt(abs(T_obs), df)) 

# So it appears that this is very signiificant ! 


# 2. D 
# So, let's check to see whether the slopes for all of the 3 race groups are the 
# same or not. 

# create a model with only education
d.lm = lm(log(income) ~ educ, data = gssincome) 
anova(model1,d.lm) 

#so Model 1 is significantly better than Model 2. So, we can 
# say that we do need the terms race 



# 2. E 
# So, we want to create the 95 percent confidence interval for 
# black students with a  college degree! This means that there education is 
# 16 or more. 

newdata = data.frame(educ = 16 , race = '2')
# exponentialte because we were originally in log(income)
exp(predict(model1, newdata, interval = "prediction"))



# 2. F 
# 

# So, we want a model with all of the potential predictors
# need to add log(siblings), this comes out to roughly 
# the same amount of degrees of freedom we need 
model2 = lm(log(income) ~ ., data = gssincome)
summary(model2)



# It looks like these variables are significant! 
# ID
# age
# female
# evermarried
# educ
# sexorient2
# happiness_pretty happy
# own gun
# own home 
# work govt 


# 2. G
# Stepwise Regression 
library(MASS)
library(stats) 

# Backward
# model3 = step(model2, scope=list(lower=model2), direction="backward")
model3 = step(model2, direction="backward")
summary(model3)
extractAIC(model3) 
# this is the AIC 

terms = c(model3$terms[[3]])
c(model3$terms[[3]])
variables_to_keep <- c("income", "age", "foreignborn", "female", "crack", "evermarried", "educ", "sexorient", "height","marijuana","happiness","owngun","ownhome","catholic","workgovt")


# 2. H 
# Now let's create a forward stepping model using the interactions 
interactionModel = lm(log(income) ~.^2, data=gssincome[variables_to_keep]) 
summary(interactionModel)
model4 = step(model3, scope=list(upper=interactionModel), direction="forward")
summary(model4)
extractAIC(model4) 



# 2. I 
model0 = lm(log(income)~1, data = gssincome)
FullModel = lm(log(income) ~.^2, data=gssincome)
model5 = step(model2, scope = list(lower = model0, upper = FullModel), direction = "both")
summary(model5) 
extractAIC(model5) 


# 2. j
# So, let's find out which of our models has the lowest AIC! 
extractAIC(model1) 
extractAIC(model2) 
extractAIC(model3) 
extractAIC(model4) 
extractAIC(model5) 


# So, it looks like model 5 has the best AIC! Now, let's check it's assumptions
# these are 

# 1. #
# 1. Linearity 
# So, all of these variables are linear because we checked this at the beginning!
# we already made sure that each of these variables were linear. 

# 2. Independence of residuals
res = residuals(model5)

## Hmm.. so the independence of residulas is difficult to know. In this case, 
# we can only look at the study design. As we look at the study design, we have that 
# each of the individuals might be indepdendent, but they also might not. (i.e. we might
# have family members within the study. etc.) Thus, it's difficult to say whether
# this is upheld or not in this example. 


# 3. Equal Variance of Errors 
plot(res~log(gssincome$income))


# Hmm.. this does not look randomly spread at all. In this case, I would say that it does 
# NOT seem that this assumption is upheld.  We probably need further transformations of 
# some of our variables 

# 4. Normality of Errors 
qqnorm(res) 
qqline(res)
hist(res)


## hmm.. so how normal are our errors?  Well if we look at the qqplot and the histogram
# it looks like we have  a slight left hand tail. But over all, this is roughly
# symmetric, so we can say this assumption of the linear model is mostly upheld. 


# 3. 
#

set.seed(420)
nsims=2000
n=nrow(gssincome)
sse3=sse4=sse5=rep(NA,nsims)
sse3.exp=sse4.exp=sse5.exp=rep(NA,nsims)

for(i in 1:nsims){
  reorder=sample(n)				
  train=gssincome[reorder[1:1000],]
  test=gssincome[reorder[1001:n],]
  
  # create model 3 with train data 
  model3 = lm(log(income) ~ age + female + crack + evermarried + educ + sexorient + 
                height + marijuana + owngun + ownhome + catholic + workgovt, data = train)
  
  
  # model4
  # Use the aforecreated interaction model to develop model4 
  model4 = lm(log(income) ~ age + female + crack + evermarried + educ + sexorient +  height + marijuana + owngun + ownhome + catholic + workgovt +   female:evermarried + age:evermarried + female:owngun + educ:marijuana +   age:sexorient + evermarried:height + crack:ownhome + age:educ +  female:catholic + age:crack + owngun:ownhome + evermarried:ownhome +   sexorient:ownhome + owngun:catholic + female:ownhome + age:workgovt + crack:owngun + crack:catholic, data = train)
  
  # model5 
  model5 = lm(log(income) ~ age + foreignborn + female + numchildren + crack + evermarried +  educ + race + siblings + sexorient + height + marijuana +  political + happiness + hispanic + owngun + ownhome + sexpartners +  otherlang + catholic + veteran + workgovt + religious + age:numchildren + female:evermarried + political:catholic + age:evermarried +  crack:veteran + age:otherlang + siblings:veteran + female:race +  numchildren:workgovt + female:owngun + educ:marijuana + female:numchildren +  female:sexorient + age:height + race:workgovt + numchildren:ownhome + race:owngun + female:ownhome + crack:ownhome + numchildren:crack +  crack:sexpartners + age:veteran + marijuana:veteran + educ:hispanic +  evermarried:happiness + height:political + height:religious +  age:sexpartners + age:owngun + crack:evermarried + crack:marijuana +  sexorient:ownhome + crack:sexorient + foreignborn:catholic + hispanic:catholic + race:ownhome + sexorient:height + race:political +  political:hispanic + siblings:happiness + female:catholic + female:hispanic + evermarried:educ + evermarried:hispanic + foreignborn:otherlang + race:siblings + crack:otherlang + crack:workgovt + evermarried:height, data = train)
  
  
  # Find the sums of squares errors for the three models
  sse3[i]=sum((log(test$income)-predict(model3,new=test))^2)
  sse4[i]=sum((log(test$income)-predict(model4,new=test))^2)
  sse5[i]=sum((log(test$income)-predict(model5,new=test))^2)
  

}

# list out the sums of square errors for  log income 
c(mean(sse3),mean(sse4),mean(sse5))/(n-1000)

# list out the sums of square errors for income! We
# did this by exponentiating the sums of square errors for our original income
exp(c(mean(sse3), mean(sse4), mean(sse5)) / (n-1000))


## So, in both cases we have that model5 has the smallest sums of square error. Note that I divided
# by the number of observations in the test case in order to normalize. This will not change
# the comparison between models. In this case,
# we can say that this shows that the stepwise model in part 2.j best predicts our data! 
# This aligns with what we had in part 2.j. And, intuitively ,this makes sense. In general, our step wise 
# model creation is better than just our forward or backwards model (and especially a model like in 3 that doesn't include interaction terms)

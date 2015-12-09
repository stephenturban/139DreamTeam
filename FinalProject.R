##### Looking at assumptions of insitution types 
install.packages('ggplot2') # This only needs to be run once
# I often need scales and grid when I work with ggplot2
install.packages(c('scales', 'grid'))
library('data.table')
library('ggplot2')
library('scales')
library('grid')

# Installs packages in order to use LASSO 
install.packages(c('glmnet', 'lars'))
library('Matrix')
library('foreach')
library('glmnet')
library('lars')


fname=file.choose()
#choose the 4.6 MB data! 
data=read.csv(fname,header=T)

# Look at the dimensions of our data
dim(data)

# now let's drop some of the data we don't need! 
drops <- c("gt_25k_p6","opeid6", "INSTNM","STABBR", "CITY", "INSTURL", "NPCURL", "SATVR25", "SATVR75", "SATMT25", "SATMT75", "SATWR25", "SATWR75", "ACTEN25", "ACTCM25", "ACTCM75", "ACTEN75","ACTMT25", "ACTMT75", "ACTWR25", "ACTWR75", "ACTCMMID", "ACTENMID", "ACTMTMID", "ACTWRMID")

# delete all of the columns that we don't need 
data[drops] <- list(NULL) 

convert_to_string <- c("INSTNM","STABBR")
data[,convert_to_string] <- lapply(data[,convert_to_string], as.character)

# We change control to become a factor (was a float.) 
data$CONTROL = as.factor(data$CONTROL)
data$CONTROL = as.factor(data$CONTROL)

# convert some of our variables from factor to numeric
convert_to_numeric<- c("RELAFFIL","CURROPER","NPT4_PUB","NPT4_PRIV","NPT41_PUB","NPT42_PUB","NPT43_PUB","NPT44_PUB","NPT45_PUB","NPT41_PRIV","NPT42_PRIV","NPT43_PRIV","NPT44_PRIV","NPT45_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","RPY_3YR_RT_SUPP","C150_4_POOLED_SUPP","md_earn_wne_p10", "UNITID", "OPEID","PREDDEG", "C200_L4_POOLED_SUPP","PCTFLOAN","UG25abv","RET_FT4","RET_FTL4","RET_PT4","RET_PTL4","PCTPELL","PPTUG_EF","SATVRMID","SATMTMID","SATWRMID","SAT_AVG","SAT_AVG_ALL","PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54","UGDS","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR","UGDS_NRA","UGDS_UNKN")

# Convert from factor to numeric! 
data[,convert_to_numeric] <- lapply(data[,convert_to_numeric], as.character)
data[,convert_to_numeric] <- lapply(data[,convert_to_numeric], as.numeric)



######## CHECKING ASSUMPTIONS

# let's check the normality of median earnings 
hist(data$md_earn_wne_p10)


# the histogram looks okay, let's check out the qqplot 
qqnorm(data$md_earn_wne_p10)
qqline(data$md_earn_wne_p10)

# this doesn't appear too normal. In particular, it looks
# like there might be a right tail, let's log transform the data

# Check out the histogram - this looks great! 
hist(log(data$md_earn_wne_p10))

# the qqplot also looks really good! 
qqnorm(log(data$md_earn_wne_p10))
qqline(log(data$md_earn_wne_p10))


## So, if we investigate the data, we compared the three types of institutions. 
# However, as we looked at the graphs, it appeared that private for-profit had a
# strange bump at around 50,0000 
ggplot(data, aes(x=md_earn_wne_p10, color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")

# roughly the same with logged data 
ggplot(data, aes(x=log(md_earn_wne_p10), color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")


# We investigated this by looking through our data set and trying to understand
# why there would be some data points that showed an uptick at 50,000.
# We thought there might be a nother type of private institution that we would need to
# Take into account. 

# We found, however, that this was a result of phoenix university. There are 69 campuses 
# for phoenix (and they all report median earnings of 53,400), so it appears that there is a large
# bump, but this is really attributed to one university.  We believe that Phoenix univeristy 
# might have a bump because it's students could have a higher average age. 

n = which(names(data) == "md_earn_wne_p10")
which(names(data) == "CONTROL")

n_total = dim(data)[2]

# Because we have a lot of missing variables, we need to impute our data 
# This essentially puts in the mean of each variable in place of each data
## IMPUTE DATA
for(i in 1:n_total){
  data[is.na(data[,i]),i] = mean(data[,i], na.rm=TRUE)
}

# identify which variables are numeric. We use this because lasso can only use
# numeric variables 
numeric_variables<- c("RELAFFIL","CURROPER","NPT4_PUB","NPT4_PRIV","NPT41_PUB","NPT42_PUB","NPT43_PUB","NPT44_PUB","NPT45_PUB","NPT41_PRIV","NPT42_PRIV","NPT43_PRIV","NPT44_PRIV","NPT45_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","RPY_3YR_RT_SUPP","C150_4_POOLED_SUPP","PREDDEG", "C200_L4_POOLED_SUPP","PCTFLOAN","UG25abv","RET_FT4","RET_FTL4","RET_PT4","RET_PTL4","PCTPELL","PPTUG_EF","SATVRMID","SATMTMID","SATWRMID","SAT_AVG","SAT_AVG_ALL","PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54","UGDS","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR","UGDS_NRA","UGDS_UNKN")


# We are attempting to use LASSO in order to pick out the variables that we want to look at
# we choose LASSO because it is a variable selection method. 
# In particular, we chose LASSO because we wanted to have an unbiased way of selecting
# predictors. Otherwise, we would create a linear model with 10-15 variables  

# we are then going to use every variable that this method uses within our linear model
# look for the significant variables of all of the numeric. 

# LASSO 
# Way to get rid of variables that we don't need them. 
# This is a way of pairing down the data if you don't know how to do it. 
# It penalizes for variables (and pick the penalty term for us.) 

#LASSO 
#we're trying to minimize risiduals, but there's a penalty for adding a variable,
#which is tough for us because you want 10,15,20 not 60 variables in a linear model
#we have 100 and we want 15, 20--we have two options
 # look through, and choose the 20 we think would be most importnat (qualitative)
  #pick 15 20 vars that are significant for us--mathematical way of doing it, instead
#  of a qualitative look
 # we added 3 more we though
  #were important
  
#in order to 
 # we can't run LASSO when we have NA's so we replace NA's with the mean of the other
  # so we can run LASSO (single imputing)
# weakness of model--false number of values equal to mean

# Probably should pick 10-15 variables that do relate to 
# Use a logical argument to get rid of some of the variables 
significant_variables = glmnet(x=scale(as.matrix(data[numeric_variables])), y=log(data$md_earn_wne_p10),  standardize = T, intercept = F, family = c("gaussian"), alpha = 1)


# we set the lambda value to be .01 because we want there to be a medium penalty
# we choose which variables are significant via the lasso
# Recall that the penalty s is related to how much you penalize for each 
# added variable you put into the model. The lower the number, the more likely you
# are to have more avraiables. 

#s - how rigorous you want to be about selecting the variables
#use LASSO to select variables, but use lm models to find coefficients
coef(significant_variables, s=.025)



## So, we end up with a model that has these variables as significant 
#  UGDS_ASIAN, UGDS, PCIP45, PCIP14, PCIP12, SATMTMID, PCTPELL, PREDDEG, PCTFLOAN, RPY_3YR_RT_SUPP, GRAD_DEBT_MDN_SUPP, GRAD_DEBT_MDN10YR_SUPP
# NPT4_PRIV, NPT4_PUB, 

# These are the factor variables we add because we are interested in them
# CONTROL, LOCALE, region



#####################
#  So, let's check the assumption of linearity in our model! 

# Let's investigate the linearity of these variables  
pairs(~log(md_earn_wne_p10) + PREDDEG + PCTFLOAN + RPY_3YR_RT_SUPP, data = data) 

pairs(~log(md_earn_wne_p10) + GRAD_DEBT_MDN_SUPP + GRAD_DEBT_MDN10YR_SUPP + NPT4_PRIV + NPT4_PUB , data = data)

pairs(~log(md_earn_wne_p10) + UGDS_ASIAN +  UGDS + PCIP45, data = data) 

pairs(~log(md_earn_wne_p10) + PCIP14 + PCIP12 + SATMTMID + PCTPELL, data = data) 



## Variables to investigate: PCIP 14, UGDS_ASIAN, RPY_3YR_RT_SUPP
plot(data$RPY_3YR_RT_SUPP, log(data$md_earn_wne_p))

# let's try a squaring our data to see if this works better
# this looks much better
plot((data$RPY_3YR_RT_SUPP * data$RPY_3YR_RT_SUPP) , log(data$md_earn_wne_p))

## REPORT WRITER. 
# Please note:  RPY_3YR_RT_SUPP = 3-year repayment rate  
hist(data$RPY_3YR_RT_SUPP)

# let's investigate PCIP14 
plot(data$PCIP14,log(data$md_earn_wne_p10))

# looks like it needs to be logged ! 
plot(log(data$PCIP14),log(data$md_earn_wne_p10))

# UGDS_ASIAN looks fine after all
plot(data$UGDS_ASIAN,log(data$md_earn_wne_p10))


## Let's transform these actually in the data 
# this will help us when we want to do lm(.) (AKA to use all of the variables) 
data$RPY_3YR_RT_SUPP = (data$RPY_3YR_RT_SUPP * data$RPY_3YR_RT_SUPP)
# plus 1 in order to avoid logging 0 
data$PCIP14 = log(data$PCIP14 + 1)

# These are all the important variables
v_important = c("md_earn_wne_p10", "UGDS_ASIAN", "UGDS", "PCIP45", "PCIP14", "PCIP12", "SATMTMID", "PCTPELL", "PREDDEG", "PCTFLOAN", "RPY_3YR_RT_SUPP", "GRAD_DEBT_MDN_SUPP", "GRAD_DEBT_MDN10YR_SUPP", "NPT4_PRIV", "NPT4_PUB", "CONTROL", "LOCALE", "region")

# Let's also complete the transformation of earning to its log form
data$md_earn_wne_p10 = log(data$md_earn_wne_p10 + 1)

# so, now we have transformed each of the variables!  

model1 = lm(md_earn_wne_p10 ~ . , data = data[v_important])

summary(model1)

#we used LASSO for the variable selection instead of the in-class methods
#because in class we started with a lot fewer variables so we didn't have to worry about this
#lm isn't good at selecting variables when we're dealing with 100 variables

# next step: make a linear model
# intercept only
model0 = lm(md_earn_wne_p10~1, data=data[v_important])
summary(model0)

library(MASS)

# Forward
step_forward = step(model0, scope=list(upper=model1), direction="forward")
summary(step_forward)
step_forward$anova

# Backward
step_backward = step(model1, scope=list(lower=model0), direction="backward")
summary(step_backward)
step_backward$anova

# Stepwise
step_both = step(model1, scope=list(lower=model0, upper=model1), direction="both")
summary(step_both)
step_both$anova

#linear model including interactions 
FullModel=lm(md_earn_wne_p10 ~ .^2, data = data[v_important])
summary(FullModel)

#Stepwise
step_both_full = step(model1, scope = list(lower = model0, upper = FullModel), direction = "both")
summary(step_both_full)
step_both_full$anova

#Cross Validation Test 
library(cvTools)
set.seed(1234)
cv_assignment = cvFolds(n, K=5, type="random")
cvFit(model0,data=data,K=5,R=1,y=md_earn_wne_p10,foldtype=c("random"))
cvFit(model1,data=data,K=5,R=1,y=md_earn_wne_p10,foldtype=c("random"))
cvFit(FullModel,data=data,K=5,R=1,y=md_earn_wne_p10,foldtype=c("random"))
cvFit(step_both,data=data,K=5,R=1,y=md_earn_wne_p10,foldtype=c("random"))

##### Looking at assumptions of insitution types 
install.packages('ggplot2') # This only needs to be run once
# I often need scales and grid when I work with ggplot2
install.packages(c('scales', 'grid'))
library('data.table')
library('ggplot2')
library('scales')
library('grid')


fname=file.choose()
#choose the 4.6 MB data! 
data=read.csv(fname,header=T)

# Look at the dimensions of our data
dim(data)

# now let's drop some of the data we don't need! 
drops <- c("opeid6", "CITY", "INSTURL", "NPCURL", "SATVR25", "SATVR75", "SATMT25", "SATMT75", "SATWR25", "SATWR75", "ACTEN25", "ACTCM25", "ACTCM75", "ACTEN75","ACTMT25", "ACTMT75", "ACTWR25", "ACTWR75", "ACTCMMID", "ACTENMID", "ACTMTMID", "ACTWRMID")

# delete all of the columns that we don't need 
data[drops] <- list(NULL) 

convert_to_string <- c("INSTNM","STABBR")
data[,convert_to_string] <- lapply(data[,convert_to_string], as.character)

# We change control to become a factor (was a float.) 
data$CONTROL = as.factor(data$CONTROL)
data$CONTROL = as.factor(data$CONTROL)

# convert some of our variables from factor to numeric
convert_to_numeric<- c("LOCALE","RELAFFIL","CURROPER","NPT4_PUB","NPT4_PRIV","NPT41_PUB","NPT42_PUB","NPT43_PUB","NPT44_PUB","NPT45_PUB","NPT41_PRIV","NPT42_PRIV","NPT43_PRIV","NPT44_PRIV","NPT45_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","RPY_3YR_RT_SUPP","C150_4_POOLED_SUPP","md_earn_wne_p10", "UNITID", "OPEID","PREDDEG", "C200_L4_POOLED_SUPP","PCTFLOAN","UG25abv","RET_FT4","RET_FTL4","RET_PT4","RET_PTL4","PCTPELL","PPTUG_EF","SATVRMID","SATMTMID","SATWRMID","SAT_AVG","SAT_AVG_ALL","PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54","UGDS","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR","UGDS_NRA","UGDS_UNKN")

# Convert from factor to numeric! 
data[,convert_to_numeric] <- lapply(data[,convert_to_numeric], as.character)
data[,convert_to_numeric] <- lapply(data[,convert_to_numeric], as.numeric)


#What we should do next
#Figure out what variables we want to drop 
#Group the majors, states 
#then convert whats left to numeric
#check assumptions and transformations 

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

# Installs packages in order to use LASSO 
install.packages(c('glmnet', 'lars'))
library('Matrix')
library('foreach')
library('glmnet')
library('lars')

n = which(names(data) == "md_earn_wne_p10")


significant_variables = glmnet(as.matrix(data[,-(n)]), log(data$md_earn_wne_p10),  standardize = T, intercept = F, family = c("gaussian"), alpha = 1)



# LASSO 
# Way to get rid of variables that we don't need them. 
# This is a way of pairing down the data if you don't know how to do it. 
# It penalizes for variables (and pick the penalty term for us.) 

# Probably should pick 10-15 variables that do relate to 
# Use a logical argument to get rid of some of the variables 
# 


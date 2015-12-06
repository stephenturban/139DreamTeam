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

# Run the data set testing mean earnings
typeof(data$UGDS_WHITE)

# Convert our Y variable to numeric (was factor) (THIS IS A TEST)
# We will delete this later
data$md_earn_wne_p10_NUM <- as.numeric(levels(data$md_earn_wne_p10))[data$md_earn_wne_p10]
data$UGDS_WHITE_NUM <- as.numeric(levels(data$UGDS_WHITE))[data$UGDS_WHITE]

# this is just a test to see if our modeling works! 
fit_test <- lm(md_earn_wne_p10_NUM ~ UGDS_WHITE_NUM, data=data)

# show results of test model 
summary(fit_test)


# now let's drop some of the data we don't need! 
drops <- c("opeid6", "CITY", "INSTURL", "NPCURL", "SATVR25", "SATVR75", "SATMT25", "SATMT75", "SATWR25", "SATWR75", "ACTEN25", "ACTCM25", "ACTCM75", "ACTEN75","ACTMT25", "ACTMT75", "ACTWR25", "ACTWR75", "ACTCMMID", "ACTENMID", "ACTMTMID", "ACTWRMID")

# delete all of the columns that we don't need 
data[drops] <- list(NULL) 

convert_to_string <- c("INSTNM","STABBR","PREDDEG")
data[,convert_to_string] <- lapply(data[,convert_to_string], as.character)

convert_to_factor <- c("CONTROL")
data[,convert_to_factor] <- lapply(data[,convert_to_string], as.factor)


# convert some of our variables from factor to numeric
# XXX TO-DO, Add all of the factor variables that need to be changed
# to this list 
convert_to_int<- c("LOCALE","RELAFFIL","CURROPER","NPT4_PUB","NPT4_PRIV","NPT41_PUB","NPT42_PUB","NPT43_PUB","NPT44_PUB","NPT45_PUB","NPT41_PRIV","NPT42_PRIV","NPT43_PRIV","NPT44_PRIV","NPT45_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","RPY_3YR_RT_SUPP","C150_4_POOLED_SUPP","md_earn_wne_p10",
                    "UNITID", "OPEID")
convert_to_float<- c("C200_L4_POOLED_SUPP","PCTFLOAN","UG25abv","RET_FT4","RET_FTL4","RET_PT4","RET_PTL4","PCTPELL","PPTUG_EF","SATVRMID","SATMTMID","SATWRMID","SAT_AVG","SAT_AVG_ALL","PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54","UGDS","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR","UGDS_NRA","UGDS_UNKN")

# Convert from factor to numeric! 
data[,convert_to_int] <- lapply(data[,convert_to_num], as.integer)
data[,convert_to_float] <- lapply(data[,convert_to_num], as.float)


#function that calls all the variable names
names = colnames(data)

#What we should do next
#Figure out what variables we want to drop 
#Group the majors, states 
#then convert whats left to numeric
#check assumptions and transformations 

######## CHECKING ASSUMPTIONS

# let's check the normality of median earnings 
hist(data$md_earn_wne_p10_NUM)

# the histogram looks okay, let's check out the qqplot 
qqnorm(data$md_earn_wne_p10_NUM)
qqline(data$md_earn_wne_p10_NUM)

# this doesn't appear too normal. In particular, it looks
# like there might be a right tail, let's log transform the data

# Check out the histogram - this looks great! 
hist(data$log_md_earn)

# the qqplot also looks really good! 
qqnorm(data$log_md_earn)
qqline(data$log_md_earn)


#Building our model 
#changing variable to numeric
data$UGDS_BLACK_NUM <- as.numeric(levels(data$UGDS_BLACK))[data$UGDS_BLACK]
data$PPTUG_EF_NUM <- as.numeric(levels(data$PPTUG_EF))[data$PPTUG_EF]

#testing variable significance by itself
regmodel1=lm(md_earn_wne_p10_NUM ~ UGDS_BLACK_NUM, data=data)
summary(regmodel)

regmodel2 = lm(md_earn_wne_p10_NUM ~ PPTUG_EF_NUM, data =data)
summary(regmodel2)

#running full model with all variables
model1=lm(md_earn_wne_p10_NUM ~ UGDS_WHITE_NUM+UGDS_BLACK_NUM, data=data)
summary(model1)

model2=lm(md_earn_wne_p10_NUM ~ PPTUG_EF_NUM+UGDS_WHITE_NUM+UGDS_BLACK_NUM, data=data)
summary(model2)



## So, if we investigate the data, we compared the three types of institutions. 
# However, as we looked at the graphs, it appeared that private for-profit had a
# strange bump at around 50,0000 
ggplot(data, aes(x=md_earn_wne_p10_NUM, color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")

# roughly the same with logged data 
ggplot(data, aes(x=log(md_earn_wne_p10_NUM), color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")


# We investigated this by looking through our data set and trying to understand
# why there would be some data points that showed an uptick at 50,000.
# We thought there might be a nother type of private institution that we would need to
# Take into account. 

# We found, however, that this was a result of phoenix university. There are 69 campuses 
# for phoenix (and they all report median earnings of 53,400), so it appears that there is a large
# bump, but this is really attributed to one university.  We believe that Phoenix univeristy 
# might have a bump because it's students could have a higher average age. 



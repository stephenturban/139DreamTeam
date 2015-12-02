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

# convert some of our variables from factor to numeric
# XXX TO-DO, Add all of the factor variables that need to be changed
# to this list 
convert_to_num <- c("SAT_AVG", "SAT_AVG_ALL", "md_earn_wne_p10", "UNITID", "OPEID")

# Convert from factor to numeric! 
data[,convert_to_num] <- lapply(data[,convert_to_num], as.numeric)

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





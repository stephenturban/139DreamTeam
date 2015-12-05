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

# convert some of our variables from factor to numeric
convert_to_num <- c("SAT_AVG", "SAT_AVG_ALL")

# Convert from factor to numeric! 
data[,convert_to_num] <- lapply(data[,convert_to_num], as.numeric)

# Convert University Type to factor! 
data$CONTROL = as.factor(data$CONTROL)



hist()

plot(data$md_earn_wne_p10_NUM ~ CONTROL, data = data)

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



#  This is a graph that looks the different median earnings 10 years 
# after graduation for the different types of schools 
ggplot(data, aes(x=log(md_earn_wne_p10_NUM), color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")

ggplot(data, aes(x=md_earn_wne_p10_NUM, color=CONTROL, fill=CONTROL, group=CONTROL)) + geom_density(alpha=0.3) + theme_light(base_size=16) + xlab("Median Earnings 10 Years after Matriculation") + ylab("")



## So, notice that there are is a second bump around 50,000. this occurs 
# because of the university of phoenix. The University of Phoenix has 69 campuses
# around the united states. Each of these schools has a mean income of 53400. 
# we hypothesized that this might occur because the university of phoenix has older students
# as a result 10 years out they might have a higher income than other for profit schools. 


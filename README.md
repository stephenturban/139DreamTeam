 

#Public Schools in the Private Market:
#Predicting Future Earnings Based on College Type

Team members: Stephen Turban, Kojin, Angelina, and Ezinne

#Research Question and Motivation

Everyone wants to make a lot of money, but do graduates of some types of colleges have a greater chance than other college graduates of securing higher incomes down the road? Choosing which college to attend is a daunting task, especially when tuition can be as high as $60,000 per year. Do the most expensive colleges yield the highest returns to investment? What specifically determines the future financial success of a college’s graduates? These are important questions for a young person choosing where to go to college--a decision that will most likely have major effects on their future success.
We are interested in the effects that various college-specific characteristics have on the future incomes of undergraduate students. More specifically, we wanted to investigate the differences--if any--in the future salaries of undergraduates at private schools versus public schools because this is a big area of contention. Many people believe that private colleges result in higher future incomes for their students on average, but others argue that the lack of enormous student loans makes public schools more beneficial for future financial success. We hope that our research and findings can help students #make this difficult decision by shedding light on the key differences among colleges that might influence future salary #levels. 

Our project primarily seeks to answer the research question: is there a significant difference in the earnings of graduates of public, private for profit, and private nonprofit colleges ten years after undergraduate graduation? We also do a more general investigation to determine which other factors contribute most significantly to future earnings of college graduate.

#Hypotheses

Our hypothesis is that on average, holding all else constant, graduates of private nonprofit colleges will have higher salaries than graduates of public and private for profit colleges ten years after graduation. This is mainly because Ivy League graduates are known to have relatively high future earnings, and all of the Ivy League schools fall under the #private nonprofit category. We also guessed that future earnings for private for profit college graduates would be slightly #higher than those of public college graduates. It is a commonly held belief--often supported by the media--that in general, #private schools lead to greater career success than public schools. This is largely why we hold the hypothesis that we do.

#Methods

Convert variables to appropriate types:

We analyzed the U.S. Department of Education’s College Scorecard Data, a dataset designed to “increase transparency, putting the power in the hands of the public — from those choosing colleges to those improving college quality — to see how well different schools are serving their students.”1 We have decided to use “the median earnings 10 years after college” as our response variable. The data includes 122 variables for each of the 7804 colleges in the dataset. 

In order to use the data, however, we had to put it into a usable format. After downloading  the data, we had that almost all of our 122 variables were considered “factors”.. This meant that they were considered categorical variables. This unfortunately wasn’t representative of most of our variables. Many of them, like SAT score, should be integers. Others, like percent African American, should be a float. In this case, we had to change the variables from factors into numeric.

Before converting and dropping variables:

This conversion proved relatively difficult. R is unable to directly convert from factors into percentages directly. So, in order to do that conversation, we needed to transform 80+ variables into CHAR and then into numeric. Overall, we converted over 80 variables from factors into numeric variables. 

Drop unnecessary variables:

After cleaning the data, we started by looking at each variable individually and deciding whether we should consider it. We dropped 25 variables that we did not find useful in this initial look through the data, mostly the students’ 25th and 75th percentile averages for each different SAT and ACT subject. We dropped these variables because they only had data on 22 out of the 1700+ schools listed. We felt that this was not a large enough number to do substantial analysis on. 
After converting and dropping variables

Even after removing these variables, there were 84 variables left, which was more than the ideal number for a linear model. For a good linear model, we want to have closer to 10 or 15 predictors, not 84. We could have continued with a qualitative approach, combing through all of the variables and choosing the 15 that we expected, based on research and our own knowledge, to be the most significant. However, we chose to take a more quantitative approach to variable selection and used the Least Absolute Shrinkage and Selection Operator (LASSO) to select the best predictors for our model.

Use LASSO to select significant variables:

What is LASSO? LASSO is a regression method that penalizes for having extra variables. In addition to the squares term (i.e. (y-bx)^2), LASSO also minimizes an extra term that has the coefficients of the variables (i.e. a*|b|). After going to office hours and talking to with some of the TFs, we realized LASSO is often considered a better method to use for variable selection.

To use LASSO however, we could not have any missing data (i.e. NA’s) in our dataset. In order to correct for this, we decided to impute the data. What is imputing? Imputing is the process of replacing missing data. We decided to use single imputation. Single imputation, for reference, is the simplest imputing process where any missing data is replaced by the mean value of the variable. There are drawbacks to single imputation, in particular if there is a large amount of data missing it gives a lot of weight to the mean value. However, we decided to use single imputation because it is significantly less computationally complex. Future studying in statistics will hopefully empower us to use more sophisticated imputing methods! 

We used LASSO at an s-value (measurement of selection rigorousness) of .025 to select the fourteen best predictors. This s-value is appropriate since rule of thumb for the s-value is 0.1 (rigorous), 0.01 (moderate) and 0.001 (lenient). We also manually added three more factor variables--CONTROL, LOCALE, and region--into our model because we were interested in them, particularly CONTROL, which is the variable that marks a college as public, private nonprofit, or private for profit. From there, we checked the assumptions of the variables and then tried a forward approach, backward approach, and stepwise approach and chose the model with the lowest AIC value.

#Assumptions

In the regressions we ran, there were four underlying assumptions: the errors are Normally distributed, each predictor variable has a linear relationship with the outcome variable, there is independence of the errors, and the errors have equal variance. We will investigate each of these four assumptions carefully.

1. Normality of Errors

We started by checking the Normality of our response variable, the median earnings 10 years after college. This data was very right-skewed, so we log transformed it to make the Normality assumption for the outcome variable hold. We also looked at the normality of our errors. In general, it looked the residuals that we looked at had a pretty normal (or at least symmetric) trend. In this case, we said that the assumption of normality of errors was upheld! 

2. Linearity

Next, we investigated the relationship between the predictors and the outcome variable. Ideally these would all be linear, but they did not turn out to be:Most of these predictors satisfy the assumption that there is a linear relationship between the predictor and the outcome variable. However, there were a few that needed to be investigated further, and possibly transformed: RPY_3YR_RT_SUPP (3-year repayment rate), PCIP 14 (number of engineering majors), and UGDS_ASIAN (total share of enrollment of Asian degree-seeking students). Relationship between untransformed RPY_3YR_RT_SUPP (3-year repayment rate) and median income 10 years after college graduation. Above left: Same relationship after squared transformation of the predictor. Please note, the vertical line in the center is the imputed data. So, we ignore it when looking at linearity.Relationship between untransformed PCIP 14 (engineering majors) and median income 10 years after college graduation. Above left: Same relationship after log transformation of the predictor.UGDS_ASIAN (total share of enrollment of Asian degree-seeking students)actually ended up looking best untransformed.

3. Independence of Residuals

The third assumption we had to investigate was independence of the residuals. This is an assumption that needs to be looked at from the study design. In general, there was a strong correlation between some of the observations. For example, Phoenix university (which will be discussed in further detail below) had 69 different campuses. Each of these are considered unique observations, but they all have the same data (e.g. median income, race, etc.) As well, it might be that certain types of schools affect each other. Schools that are similar in type and in owner for private schools might have similar characteristics. In general, we don’t have enough information to fully reject or uphold this assumption. We’ll continue with the project because we can’t do too much to avoid these internal relationships. 

4. Equal Variance of residuals
	
The last assumption to investigate is equal variance of the residuals. The graph looks like we uphold this assumption! 

Analyzing our variable of interest: CONTROL	

The variable that we were most interested in initially was CONTROL. Recall, CONTROL has three categories 1: Public 2. Private, Non-profit and 3. Private for-profit. Before we analyzed our data, we did a further analysis of the median earnings 10 years after matriculation and the three different types of institutions. When we looked at the graphs (below for earnings left and logged right.) we noticed a strange effect. It seemed like the three graphs were roughly normal. But, for the private for-profit section there were a couple of strange bumps.We decided to investigate this further. Perhaps, as we reasoned, there was a type of college that wasn’t captured by this three way split. Examples of a different type of private for-profit university could be a school of engineering or a school of welding, etc.  

To analyze why the second bump occurred we looked at all of the private for-profit colleges with a median earning above $50,000. After looking at these colleges, the phenomenon became clear. Within the data set, there were a few universities that had dozens of satellite campuses. The University of Phoenix, in particular, had 69 different campuses included as separate universities in the data. Unfortunately, the University of Phoenix also reported all of their median incomes as if it was one university. So, the bump you see below at $50,000 is actually 69 university of phoenix campuses that all report a median earning of $52,400. We decided not to investigate this further. It might be that the average age of the student at the University of Phoenix is higher than at the other private, for-profit schools. As a result, their median income might be higher 10 years out. However, our data set does not have sufficient data to confirm or deny this claim. 

#Results and Interpretation

The LASSO approach was an effective method for picking out the most significant variables for our model. We ran the lm function in R using the full model, which we called model 1. And then ran an intercept only model, called model 0.  Then we conducted a step forward (model 2), step backward (model 3) , and stepwise (model 4) linear regression to further determine which variables are significant from the ones we were left with after conducting the LASSO method. Model 2, 3, and 4 both produce the same AIC values.

	> extractAIC(model2)
	[1]     28.00 -22299.93
	> extractAIC(model3)
	[1]     28.00 -22299.93
	> extractAIC(model4)
	[1]     28.00 -22299.93

Then we conduct another test using cross validation. And since the value is the lowest for model 2, we can conclude that model 2 is the best fit model for our data. 

	cvFit(model2,data=data[v_important],K=5,R=1,y=data[v_important]$md_earn_wne_p10,foldtype=c("random"))
	5-fold CV results:
	       CV 
	0.2396285 
	> cvFit(model3,data=data[v_important],K=5,R=1,y=data[v_important]$md_earn_wne_p10,foldtype=c("random"))
	5-fold CV results:
	       CV 
	0.2397833 
	> cvFit(model4,data=data[v_important],K=5,R=1,y=data[v_important]$md_earn_wne_p10,foldtype=c("random"))
	5-fold CV results:
	       CV 
	0.2397596

Significant variables from our chosen model (Model2): 
UGDS_ASIAN (total share of enrollment of undergraduate degree seeking students)
UGDS (enrollment of degree seeking undergraduate students)
PCIP45 (social sciences)
PCIP14 (Engineering majors)
PCIP12 (personal and culinary services)
SATMTMID (midpoint of SAT scores at the institution for math)
PCTPELL (percentage of undergraduates who receive a pell grant)
PREDDEG (predominant degree awarded from that school)
PCTFLOAN (percentage of all undergraduates receiving federal loans)
RPY_3YR_RT_SUPP (3-year repayment rate)
GRAD_DEBT_MDN10YR_SUPP
NPT4_PRIV (Average net price for Title IV institutions for private for-profit and nonprofit institutions) 
LOCALE (locale of institution) 
CONTROL (type of institution: public, private nonprofit, private for profit) 
Region (region of institution) 

Based on the model, it looks like the variables PCIP14, PCIP45, UGDS, PCTFLOAN, PREDDEG, and UGDS_ASIAN have a positive relationship to our response variable, while the rest have a negative relationship based on their coefficients. Model 2 produced an R^2 value of 0.4488, meaning that 44 percent of our data follows this model. We also ran the stepwise linear regression with a full model that included interaction terms. This produced a higher R^2 value of 0.513, however our results were less interpretable so we chose the model without interactions as our best fit model for predicting income.

#Limitations

There are hundreds of factors that can differ even among colleges that are all public or among colleges that are all private: gender ratio, reputation, specialized programs of study, extracurricular offerings, size of city, region of country, alumni network and involvement, social culture, quality of career advising, and many other factors. These differing 
Our overall goal was to determine which college characteristics predict future success in graduates’ careers. The variable that we chose to look at as our measure of “success” was income level ten years after graduating college. Maybe this was not the best possible variable to choose--it might be limiting and leave out part of the picture. For example, it might be possible for a school to produce a large number of workers in a specialized field where large salary payoffs don’t come for over ten years. If graduates were to start making relatively very high incomes at that point, their college’s future success rate would have been underestimated.

One of the limitations of the LASSO method we used is that we needed to impute parts of our data. This method of imputation does have drawbacks. For some variables, like SAT Average, only 50% of the schools had data. This meant that almost half of the average SAT scores that we had in our data were imputed from the average of the other schools. Clearly, this could dilute any effect that we might otherwise see. As well, it might be overlooking another fundamental trait of the schools. Schools that don’t report SAT scores might have fewer students who took the test. In this case, we would actually be giving each school a higher SAT score than they would naturally.

Imputation does have disadvantages . However, we thought that the advantages that LASSO gave us with variable selection far outweighed the drawbacks. As well, frankly, we were excited to try out something different. We thought it would be a good learning opportunity for the group as a whole. Finally, another limitation was the drift in our residuals. This implies that there is an underlying mechanism that is not accounted for in our model. After numerous transformations of the Y and X variables, we were unable to eliminate this drift in the model. We believe that this might indicate an underlying relationship in the data that was not accounted for in our model. In the future, we would like to investigate what caused the drift in our errors. We hypothesize that there could be something in the study design that caused such a drift. However, without a deeper understanding of how the data was collected, we’re unable to investigate this assumption further.

#Conclusions and Further Discussion

We ended up getting a very clean result with an R2 value that seemed almost too high. This R2 value might be so high because we had so many variables to choose from. Since there were so many variables, many of them were probably good predictors of future salaries, and therefore, using LASSO to choose the most significant predictors would likely produce some great results.
There are many interesting ways we could extend this analysis with further data collection and analysis. It could be of interest to investigate how male and female incomes differ for graduates of the same schools. With all the discussion of income gaps between genders, it would be interesting to investigate whether there is a significant difference between male and female incomes of graduates from the same school, who even study the same subject.

It would also be interesting to look more broadly at overall economic well-being after college. We looked into median incomes ten years after college, but there is more to financial success than one’s future income level. We did not get the chance to explore the impact that student loans have on financial success. It is possible that graduates of public colleges are as well-off as private school graduates ten years after college because they do not have to worry about paying off as many student loans. With more time, it would have been interesting to take a more comprehensive look at the financial success of graduates of different college.


# Assignment: ASSIGNMENT 6.2
# Name: Myers, Cody
# Date: 2010-07-14

setwd("C:/Users/myers/OneDrive/Desktop/dsc520")
housing_df <- read.csv("data/week-7-housing.csv")


cor(housing_df$Sale.Price, housing_df$building_grade)^2 * 100
cor.test(housing_df$Sale.Price, housing_df$building_grade)
#15.306%

cor(housing_df$Sale.Price, housing_df$square_feet_total_living)^2 * 100
cor.test(housing_df$Sale.Price, housing_df$square_feet_total_living)
#20.664%

cor(housing_df$Sale.Price, housing_df$bedrooms)^2 * 100
cor.test(housing_df$Sale.Price, housing_df$bedrooms)
#5.083%

#The choice to use these two values is based upon the results of the correlation tests and
#seeing as building grade and living sq footage are the two highest results, they were the obvious choices.

price_and_sq_ft <- lm(housing_df$Sale.Price ~ housing_df$sq_ft_lot, data = housing_df)
price__plus_other_predictors <- lm(housing_df$Sale.Price ~ housing_df$sq_ft_lot + housing_df$square_feet_total_living + housing_df$building_grade, data= housing_df)

summary(price_and_sq_ft)
summary(price__plus_other_predictors)

#Per the summaries provided, We find that the variance of the sales price is due to a 1.4% R^2 for sq_foot,
#while we see there is a 21.4% variation when introducing the other two predictors as well. Furthermore,
#we see that the R^2 and adjusted R^2 percentages have almost the same value, We see a difference of 0%. 
#From that we can determine that there wouldn't be a difference between using a population or a sample.

library("QuantPsyc")
lm.beta(price__plus_other_predictors)

#sqft : 0.01872256
#total living : 0.36113656
#building grade : 0.11952538

#From this we can deduce that each predictor can vary the outcome is going to be 1 standard deviation.
#Looking at the above values, you can see that each variable will alter that outcome by that value of
#standard deviation. 

confint(price__plus_other_predictors, level=.95)

#Based on the results of the confidence interval we can see that there is a positive relation between all predictors and the overall outcome

anova(price_and_sq_ft, price__plus_other_predictors)
#Pr(>F) = 2.2*10^(-16) shows that the introduction of the other predictors has helped the model.

housing_df$std_res <- rstandard(price__plus_other_predictors)
head(housing_df$std_res)

housing_df$stud_res<-rstudent(price__plus_other_predictors)
head(housing_df$stud_res)

housing_df$c_dis<-cooks.distance(price__plus_other_predictors)
head(housing_df$c_dis)

housing_df$beta<-dfbeta(price__plus_other_predictors)
head(housing_df$beta)

housing_df$dffit<-dffits(price__plus_other_predictors)
head(housing_df$dffit)

housing_df$lev<-hatvalues(price__plus_other_predictors)
head(housing_df$lev)

housing_df$cov_ratios<-covratio(price__plus_other_predictors)
head(housing_df$cov_ratios)

housing_df$largeRes <- housing_df$stud_res > 2 | housing_df$stud_res < -2

sum(housing_df$largeRes)

#All records with large residuals (as per the definition on line 74)
housing_df[housing_df$largeRes, c('Sale.Price', 'sq_ft_lot', 'square_feet_total_living',  'building_grade', "std_res")]


housing_df[housing_df$largeRes, c("c_dis", "lev", "cov_ratios")]
cook_dist <- housing_df$c_dis > 1
sum(cook_dist)
#this tells us that only one record has a cooks distance greater than 1, so there is 1 record that has influence in the model.

lev_data <- housing_df$c_dis > 0.0006 | housing_df$c_dis < 0.0009
sum(lev_data)
k <- 3
sampleSize <- 12865
lev <- k/sampleSize
#lev = .0003, so we would need to look between that .0006 and .0009 (two and three times the leverage).

cvr_min <- 1 + 12/sampleSize
cvr_min
cvr_max <- 1 - 12/sampleSize
cvr_max
cvr_data <- housing_df$c_dis > cvr_min |  housing_df$c_dis < cvr_max
sum(cvr_data)
#All records lie between the min and max, so there are no outliers. 

library("car")
dwt(price__plus_other_predictors)
#From the durbin-watson test, we can see that we have a positive value (.52), which means we have a positive autocorrelation

vif(price__plus_other_predictors)
mean(vif(price__plus_other_predictors))
1/vif(price__plus_other_predictors)
#Based upon these values, we find that there is not collinearity in the dataset.

plot(price__plus_other_predictors)
hist(housing_df$stud_res)

#Based upon the first plot, we see that assumptions have been met. In the Q-Q plot, the normality is slightly smaller, but this can be attibuted to the smaller sample size.

#By observing the Q-Q plot, we can see that the distribution is fairly normal, so we can assume that the model is unbiased and therefore is a good generalization of the population.

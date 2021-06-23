# Student number:19200231
# Student name: Vivekanand Kulkarni
# Predictive Analysis Project code
House=read.csv("V:\\Study\\Predictive Analysis\\Assignment project\\House.csv", header = TRUE,sep = ",")
boxplot(House$ï..Price)
# From the boxplot we can see that the median of house prices lies around 275.
# Minimum house price and maximum house price range from 150 to 450.
# First Quantile lies at 250 and Third Quantile lies at 350
hist(House$ï..Price,breaks=10)
# The Distribution seems to be positively skewed.We can see that tail tapers on the right hand side of x-axis.
summary(House$ï..Price)
# Mean of the house price distribution is around 285.8
# Median of the house price distribution is around 276.0
# Mean and Median are bit apart indicating potential outlier presence.
# First Quantile which forms 25% of the house prices lies under 242.8
# Median which is at 276.0 means 50% of the houses are under the price 276.0
# Third Quantile which forms 75% of the house price datas are under 336.8

#Question 2
 
House$Bath <-(factor(House$Bath))
House$Bed <-(factor(House$Bed))
House$Garage<-(factor(House$Garage))
House$School<-(factor(House$School))



boxplot(House$ï..Price~House$Bath,xlab="Number of Bathrooms",ylab="Prices of House")
# Here we can observe that the median for the price of house is increasing for 1 and 1.1 bathrooms, but there is sudden decrease for 2 bathrooms and again a gradual increase is seen.
by(House$ï..Price,House$Bath,summary)
# House_1$Bath: 1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 235.0   263.8   292.5   292.5   321.2   350.0 
# ------------------------------------------------------------------------------------------ 
#   House_1$Bath: 1.1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 215.0   239.5   325.0   307.9   374.5   385.5 
# ------------------------------------------------------------------------------------------ 
#   House_1$Bath: 2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 155.5   219.2   259.4   271.3   321.8   435.0 
# ------------------------------------------------------------------------------------------ 
#   House_1$Bath: 2.1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 189.5   254.8   269.9   274.5   297.7   349.5 
# ------------------------------------------------------------------------------------------ 
#   House_1$Bath: 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 230.0   259.0   295.0   307.8   349.5   450.0 
# ------------------------------------------------------------------------------------------ 
#   House_1$Bath: 3.1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 285.0   309.4   336.0   324.2   342.5   345.0

boxplot(House$ï..Price~House$Bed,xlab="Number of Bedrooms",ylab="Prices of House")
# Here we can see that there are few outliers for Prices of the house in the case where the number of bedrooms are 4.
# There is just one sample for number of bedrooms in the list of prices of houses.
which(House$ï..Price > 350 & House$Bed == 4)
# House 1,2,3 are outliers when boxplotted for house price versus number of bedrooms
by(House$ï..Price,House$Bed,summary)
# House$Bed: 2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 299.0   319.4   339.9   329.6   344.9   350.0 
# ------------------------------------------------------------------------------------------ 
#   House$Bed: 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 189.5   256.2   297.0   297.3   342.5   435.0 
# ------------------------------------------------------------------------------------------ 
#   House$Bed: 4
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 155.5   231.5   254.4   266.6   283.5   450.0 
# ------------------------------------------------------------------------------------------ 
#   House$Bed: 5
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 185.0   199.0   269.0   259.5   295.0   349.5 
# ------------------------------------------------------------------------------------------ 
#   House$Bed: 6
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 252.5   252.5   252.5   252.5   252.5   252.5 

boxplot(House$ï..Price~House$Garage,xlab="Number of cars capacity",ylab="Prices of House")
# From the boxplot we can see that there is one outlier for the sample set of houses where the garage value is o and it is priced way too much. This data is arbitary and can be excluded.
which(House$ï..Price > 300  & House$Garage ==0)
# House number 1.
by(House$ï..Price,House$Garage,summary)
# House$Garage: 0
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 185.0   216.0   232.0   246.9   264.4   388.0 
# ------------------------------------------------------------------------------------------ 
#   House$Garage: 1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 155.5   220.0   242.0   260.6   324.5   385.5 
# ------------------------------------------------------------------------------------------ 
#   House$Garage: 2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 195.0   259.0   285.0   299.6   343.8   450.0 
# ------------------------------------------------------------------------------------------ 
#   House$Garage: 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 299.0   309.2   319.4   319.4   329.7   339.9 

boxplot(House$ï..Price~House$School,xlab="Areas",ylab="Prices of House")
# There is one outliers each in the case of house prices in the Area StLouis and StMarys
which(House$ï..Price>330& House$School=="StLouis")
# Row 40
which(House$ï..Price>400 & House$School=="StMarys")
# Row 74
by(House$ï..Price,House$School,summary)
# Exploratory Data Analysis
# Question 3
# Using the summary, correlation and the pairs plots discuss the relation-
# ship between the response sales price and each of the numeric predictor
# variables.
pairs(House$ï..Price~House$Year)
# Here we can observe that House prices for the house size 2 is well dispersed ranging from 200 to 450 and also the house size 1.5 pricing from 150 to 450. 

pairs(House$ï..Price~House$Size)
# Here we can see that similar pattern of increase in density of the plot in the range of price 200 to 400 over the years 1960 to 2000.
# Most number of mid range priced house where sold most from 1960 to 2000.
pairs(House$ï..Price~House$Size+House$Year)
# Most number of mid range priced house where sold most from 1960 to 2000 and where possibly sized around 2.0. 

# The Correlation between the response sales price and each of the numeric predictor variables.
cor(House[,c(1,2,6)],method = "pearson", use = "complete.obs")

# Import the psych library.
library(psych)
describeBy(House$ï..Price,House$Year)

# Aiding the interpretation of the intercept by minusing the mean from the predictor variable speed.
House$Year<-House$Year-mean(House$Year)
House$Size<-House$Size-mean(House$Size)
House$Lot<-House$Lot-mean(House$Lot)


# Regression Model
# Question 1
# Linear model with multiple variables.
model<- lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House)
summary(model)
model$coefficient

# Regression Model
# Question 2: Interpret the estimate of the intercept term Beta0
# The estimate of the intercept term Beta0 is 376.1015959

# Question 3: Interpret the estimate of BetaSize,the parameter associated with floor size(Size).
# Estimate of BetaSize is 59.4503306.

# Question 4: Interpret the estimate of Beta Bath1.1 the parameter associated with one and
# a half bathrooms.
# Estimate of Beta_Bath1.1 is 135.8983077.

# Question 5: Discuss and interpret the effect the predictor variable bed on the expected
# value of the house prices.
# The predictor variable beds shows that median value of house prices for 2 bed rooms is nearing 350,
# median house price value for 3 bedroom houses is decreasing when compared to 2 bedroom houses.
# Median house price value for 4 bedroom houses again decreases when compared to 3 bedroom houses.
# Median house price value for 5 bedroom houses increases a little bit compared to median value of 4 bedroom houses.
# House with 6 bedrooms is just one sample and this value hardly contributes anything.

# Question 6: List the predictor variables that are significantly contributing to the 
# expected value of the house prices.
# 1) Lot predictor variable is significant(**)
# 2) Bath1.1 is the significant(**) predictor variable is significant among beds.
# 3) Bed3, Bed4, Bed5,Bed6 is a significant(**) predictor variable
# 4) SchoolHigh (**) is a significant predictor variable
# 5) SchoolNotreDame (*) is a significant predictor variable
# 6) Garage3(*) is a significant predictor variable
# 7) Size(*) is a significant predictor variable
# 8) Bath3(.) has least significance among the significant variables

# Question 7:For each predictor variable what is the value that will lead to the largest
# expected value of the house prices.

# Question 9:By looking at the information about the residuals in the summary and by
# plotting the residuals do you think this is a good model of the expected
# value of the house prices.
par(mfrow=c(2,1))
boxplot(residuals(model), main="Residuals")
plot(density(residuals(model)))
# Answer: Having a look at the residuals and plot, the model seems to be good model.
# Because of the outliers the model seems to be tapering off on the right side of x-axis.
# From the residual summary we can state that
# Minimum residual is at -87.601
# 25% of the residuals are below -21.429
# 50% of the residuals are below 0.173
# 75% of the residuals are below 24.248
# Maximum residual is valued at 72.581

# Question 10: Interpret the Adjusted R-squared value.
# The Adjusted R-square value is 50% gives value of the influence of all the predictive variables involved in the model.

# Question 11: Interpret the F-statistic in the output in the summary of the regression
# model. Hint: State the hypothesis being tested, the test statistic and
# p-value and the conclusion in the context of the problem.
# Answer : F test gives the measure of significance for the over all model.
# Hypothesis being tested are :
# Null Hypothesis where all Beta's values are equal to zero
# and Alternate hypothesis where at least one of the Beta value is non zero.
# F test statistics is 4.942 on 20 Beta values and for 55 degrees of freedom.
# P-value is telling that probability of all the Beta being equal to zero, and 1.265e-06 is less than 0.05 hence we reject the 
# Null hypothesis. 
    
# Anova:
# Question 1:Compute the type 1 anova table. Interpret the output. Hint: State the
# hypothesis being tested, the test statistic and p-value and the conclusion
# in the context of the problem.
library(car)
anova1<-anova(model)
anova1
# Analysis of Variance Table
# 
# Response: ï..Price
# Df Sum Sq Mean Sq F value    Pr(>F)    
# Lot        1  16284 16284.4  9.1767  0.003729 ** 
# Size       1  10026 10025.7  5.6498  0.020964 *  
# Year       1   4741  4740.6  2.6715  0.107872    
# Bath       5  37939  7587.9  4.2760  0.002345 ** 
# Bed        4  20200  5049.9  2.8458  0.032393 *  
# Garage     3  16101  5367.1  3.0245  0.037179 *  
# School     5  70112 14022.4  7.9020 1.153e-05 ***
# Residuals 55  97599  1774.5                      
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# As we can see that year is not significant and do not contribute much to house price, 
# it will be removed to create a reduced model.
model_2=lm(ï..Price ~  Lot + Size + Bath + Bed+ Garage+ School, data = House)
anova2<-Anova(model_2)

# Comparing the two model
anova(model,model_2)


# Diagnostics
# Question 1: Check the linearity assumption by interpreting the added variable plots
# and component-plus-residual plots. What effect would non-linearity have
# on the regression model and how might you correct or improve the model
# in the presence of non-linearity?

install.packages("ggplot2")
install.packages("GGally")
library("GGally")
library(car)


avPlots(lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House))


# Linearity assumptions interpretation
# For Added variable plots for each predictive variable for Response variable : Price
# For Lot variable the best suited values are from -2 to 1 and the slope of the model is positive(Beta value is 11.7701)
# For Size, model is best suited for values from -0.2 to 0.2 and the Slope of the model is positive, the outlier point 76 is influencing the slop (Beta value is 59.4503)
# For Year, model is best suited for the values from -20 to 20 and the Slope of the model is slightly positive.(Beta value is 0.5567)
# For Bath1.1, model is best suited for the values around 0.0 and the Slope of the model is Highly positive.(Beta value is 135.8983)
# Amongst the outliers for Bath1.1 point 37 is influencing the Slope inclination most.
# For Bath2, model is best suited for the values around 0.0 and again the Slope is positive and is most effected by the outlier point 37.(Beta value = 73.9317)
# For Bath2.1, model is best suited for the values around 0.0 and again the Slope is positive and is mostly effected by point 37.(Beta value = 76.9433)
# For Bath3, model is best suited for the values around 0.0 and again the Slope is positive and is mostly effected by the outlier point 37.(Beta value = 98.0694)
# For Bath3.1, model is best suited for the values around 0.0 and again the Slope is positive and is mostly effected by the outlier point 37.(Beta value = 85.8037)
# For Bed3, model is best suited for values around 0.0 and the slope is negative(-228.1052) and is mostly effected by 4 and 37 numbered outliers.
# For Bed4, model is best suited for values around 0.0 and the slope is negative(-238.2609) and is mostly effected by 4 and 37 numbered outliers.
# For Bed5, model is best suited for values around 0.0 and the slope is negative(-237.6155) and is mostly effected by 4 and 37 numbered outliers.
# For Bed6, model is best suited for values around 0.0 and the slope is negative(-255.0211) and is mostly effected by 4 and 35 numbered outliers.
# For Garage1, model is best suited for values ranging from -0.2 to 0.1 and slope is slightly negative(-10.9191)
# For Garage2, model is best suited for values ranging from -0.2 to 0.4 and the slope is slightly positive(18.2435)
# For Garage3, model is best suited for values around 0.0 and the slope is negative and is highly influenced by outlier points 4 and 37.(-209.9038)
# For SchoolHigh, model is best suited for values around 0.0 and the slope is positive(113.2774) and probably is influenced by outlier points 6 and 5.
# For SchoolNotreDame, model is best suited for values around 0.0 and the slope is positive(80.9317) and probably is influenced by outliers points 6 and 5.
# For SchoolStLouis,model is best suited for values around 0.0 and the slope is positive(9.0367) and probably is influenced by outliers points 6 and 5.
# For SchoolStMarys,model is best suited for values around 0.0 and the slope is positive(27.3408) and probably is influenced by outliers points 6 and 5.
# For SchoolStratford,model is best suited for values around 0.0 and the slope is positive(31.9254) and probably is influenced by outliers points 6 and 5.

crPlots(lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House))
# For Lot,Size and Year, the slope is positive and is definitely contributing towards the prediction of the model.
# Few outliers in the model is distorting the fitted line and can be seen by the purple line.
# 

# Question 2 
dwt(lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House))
# lag Autocorrelation D-W Statistic p-value
# 1       0.1836122      1.614157   0.048
# Alternative hypothesis: rho != 0
# The Durbin Watson test statistic is 1.614157 and the p-value is 0.048
# so the hypothesis of no autocorrelation is rejected and because of the value obtained we can
# say that the observations cannot be classified as independent.
# Durbin Watson test statistics is 1.614157 but the the p-value is 0.048 which is less than 0.05.
# Diagnose : Durbin Watson test
# conclusion:
# The residuals have structural dependencies
# The different outliers are causing bias or inefficiency. This can be corrected by treating the outliers.
# Because of the Structural dependencies we might not get contstant variance.
# This can be better dealt with another type of analysis which is Time series Analysis.
#~~~~~~~~~~~~~~~~~`
# What are the two common violations of the random/i.i.d.
# sample assumption?
# Repeated observations and multiple observations occurence
# No bias with appropriate alternative assumptions(structured dependence)
# This can also cause non constant variance.
# Outliers from different distributions can cause inefficiency.
# 
# What effect would dependant samples have on the
# regression model and how might you correct or improve the model in the
# presence of dependant samples?
# When fitted the regression model in to the time series data, the dependecies of the structure,
# in form of the Autocorrelation can be found in the residuals.
# There are different time series versions regression analysis
# which are suitable for this type of analysis.
# 
# Question 3: Check the collinearity assumption by interpreting the correlation and 
# variance inflation factors. What effect would multicollinearity have on the
# regression model and how might you correct or improve the model in the
# presence of multicollinearity.
#
install.packages("corrplot")
library(corrplot)
Mi<- House[c(1,2,3,6)]
mri<-cor(Mi)
corrplot.mixed(mri)
# Check the collinearity assumption
# From Cor plot we can see that none of the correlation among 
# the predictive variables are at significant, i.e. they are all less than 0.7.
# Variance inflation factors 
vif(lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House))
# GVIF Df GVIF^(1/(2*Df))
# Lot     1.654167  1        1.286144
# Size    1.601785  1        1.265616
# Year    2.671175  1        1.634373
# Bath    9.757455  5        1.255838
# Bed    20.215797  4        1.456168
# Garage 19.811449  3        1.644950
# School  6.768538  5        1.210736
# VIF is around 1.5 means that there is no correlation among the jth predictor 
# and remaining predictor variables and hence the Beta value is also not inflated.
#
# Multicollinearity would have no effect if it is just for predicting the response variable,
# on the regression model, but if for interpretation then Beta values of multicollinearity has to be taken care.
# To improve the multicollinearity we can remove the highly correlated predictor from the model.
# Choose the highly correlated variables and the one which is to be dropped and perform anova test and compare with
# all variables in the model and with just one predictor variable removed, the value of F-test pvalue gives you 
# confirmation to go ahead dropping the predictor variable.
# Or use Partial Least Squares Regression(PLS), Principal Components Analysis,
# Ridge Regression. The is use a method that cuts the number
# of predictors to a smaller set of uncorrelated components.
#
# Question 4. Check the zero conditional mean and homoscedasticity assumption by
# interpreting the studentized residuals vrs fitted values plots and the 
# studentized residuals vrs predictor variable plots. What effect would 
# heteroscedasticity have on the regression model and how might you correct
# or improve the model in the presence of heteroscedasticity.
Fitted_model=lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = House)
plot(fitted(Fitted_model),rstudent(Fitted_model))
abline(h=0)
par(mfrow=c(3,3))
plot(House$Lot,rstudent(Fitted_model))
abline(0,0)
plot(House$Size,rstudent(Fitted_model))
abline(0,0)
plot(House$Year,rstudent(Fitted_model))
abline(0,0)
plot(House$Bath,rstudent(Fitted_model),xlab="house$Bath",ylab="rstudent(model)")
abline(0,0)
plot(House$Bed,rstudent(Fitted_model),xlab="house$Bed",ylab="rstudent(model)")
abline(0,0)
plot(House$School,rstudent(Fitted_model),xlab="house$School",ylab="rstudent(model)")
abline(0,0)
plot(House$Garage,rstudent(Fitted_model),xlab="house$Garage",ylab="rstudent(model)")
abline(0,0)

# The model here has Zero conditional mean and homoscedasticity.
# The Residual variance across the fitted line is almost same along the distance.There is no significant variance observed.
#
# Question 5: Check the Normality assumption by interpreting the histogram and quantile-
# quantile plot of the studentized residuals. What effect would non-normality
# have on the regression model and how might you correct or improve the
# model in the presence of non-normality.

hist(rstudent(Fitted_model))
# Histogram distribution is bell-shaped,and is peaking around zero and is relatively symmetric.

qqnorm(rstudent(Fitted_model))
qqline(rstudent(Fitted_model),col=2)
# From Quantile -quantile plot we can see that the errors from our dataset are tracking the red line and we can say 
# that the errors are normally distributed.
# Effects of Non-normality on regression model, the critical values of F-test and T-test are wrong.
# This can be diagnosed by checking studentized residuals and qq-plots.
# How to correct or improve
# By doing transformation of response or predictive variables
# Creating interaction models
# Or use different model as the current used model is not flexible enough.



# Leverage, Influence and Outliers:
# Quetion 1. What is a leverage point? What effect would a leverage point have on the
# regression model? Use the leverage values and the leverage plots to see if
# there is any leverage points.

# Leverage point is the one with an unusual X-value.
# The measure of leverage is the amount of change in the prediction values for any out of the trend unit in Y direction(influenctial point).
# Leverage always values between 0 to 1.
# When a point has leverage 1, the line will follow the point.
# and a point with leverage 0 has no effect on the regression line.
# All the predictive variables plots show the existence of leverage points

 
leverage_points = as.numeric(which(hatvalues(Fitted_model)>((2*3)/length(House$ï..Price))))
leverage_points
leveragePlots(Fitted_model)

# Influence
# Question 2.	What is an influential point? What effect would an influential point have on the regression model? 
# Use the influence plot to see if there is any influence points.
install.packages("olsrr")
library(olsrr)
ols_plot_cooksd_bar(Fitted_model)
plot(Fitted_model,5)
influencePlot(Fitted_model,main="Influence Plot",sub="Circle size is proportial to Cook's Distance")

# Outlier
# Performing outlier test
outlierTest(Fitted_model)
# Plotting the residual levels
ols_plot_resid_lev(Fitted_model)

# Transforming the original dataset to New1 and treating the outliers by removing them.
New1<-House
New1<-New1[-c(44),]
New1<-New1[-c(30),]
New1<-New1[-c(25),]
New1<-New1[-c(26),]
New1<-New1[-c(29),]
New1<-New1[-c(39),]
New1<-New1[-c(21),]
New1<-New1[-c(17),]

# Creating a new model with removed outliers
model1<-lm(ï..Price ~  Lot + Size + Year + Bath + Bed+ Garage+ School, data = New1)
outlierTest(model1)
ols_plot_cooksd_bar(model1)
ols_plot_resid_lev(model1)


# Expected Value, CI and PI:
# Using predict to create CI and PI. 
ci=predict(Fitted_model,level=0.95,interval='confidence')
pi=predict(Fitted_model,level=0.95,interval='prediction')

# Using ggplot2 to project the plots 
library(ggplot2)
# Plotting with Outliers included
cipiplot = ggplot(House, aes(House$ï..Price,pi[,1])) + geom_point() + geom_smooth(method=lm,aes(color="Regression Line")) + geom_line(aes(y=pi[,2], color="Prediction Interval")) +geom_line(aes(y=ci[,2], color="Confidence Interval"))+geom_line(aes(y=ci[,3], color="Confidence Interval")) + geom_line(aes(y=pi[,3], color="Prediction Interval")) +
  labs(x="Observed Price", y="Expected Price")+scale_color_manual(values = c("red","blue","black"))+ggtitle("With Outliers")

cipiplot

# Using predict to create new CI and PI.
cinew=predict(model1,level=0.95,interval='confidence')
pinew=predict(model1,level=0.95,interval='prediction')

# Plotting with Outliers removed
cipiplotnew = ggplot(New1, aes(New1$Price,pinew[,1])) + geom_point() + geom_smooth(method=lm,aes(color="Regression Line")) + geom_line(aes(y=pinew[,2], color="Prediction Interval")) +geom_line(aes(y=cinew[,2], color="Confidence Interval"))+geom_line(aes(y=cinew[,3], color="Confidence Interval")) + geom_line(aes(y=pinew[,3], color="Prediction Interval")) + 
  labs(x="Observed Price", y="Expected Price")+scale_color_manual(values= c("red","blue","black"))+ggtitle("With out Outliers")
cipiplotnew
 

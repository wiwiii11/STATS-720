## ---
## title: "STATS - 720 Homework-1"
## author: "Liedri Wiam (Student number: 400550999)"
## ---

## Load libraries
## BMB: packages!

library(dotwhisker)
library(effects)

#### Question 1

## Load the dataset
data("USArrests")
USArrests
summary(USArrests)
require(graphics) ## BMB: not necessary
pairs(USArrests, panel = panel.smooth, main = "USArrests data")



###Choice of predictor variable
# Calculate correlation matrix
correlation_matrix <- cor(USArrests)
print(correlation_matrix)

#the correlation between "Murder" and "Assault" is approximately 0.802, which indicates a strong positive 
#correlation between these two variables.
# "Murder" and "Rape" have a positive correlation of approximately 0.564.
#"UrbanPop" has weaker correlations with other variables, around 0.07 to 0.26.
#I choose to use the variables murder , assault and urbanpop as predictors , and the response variable as the "rape" variable
#so i can use the three variables to predict and explain the rape variable.I used the general rule of thumb: 50/15≈ 3.


###State the units of the response variable and of each predictor variable you plan to include; for each variable, 
#state what you would consider as a reasonable threshold for a small change in that variable.

#the unit of the response variable is: the number of rape arrests per 100,000 residents
#the unit of the murder variable is : the number of  murder arrests per 100,000 residents
#the unit of the urbanpop variable is : the percentage of urban population
#the unit of the assualt variable is : the number of assault arrests per 100,000 residents
#Reasonable threshold for a small change: murder: 1 unit change,  urbanPop: 10 unit change , assault: 1 unit change, rape: 1 unit change

 

###fitting the model:
#i am fitting a  linear model to estimate the relationship between "rape" and the predictor variables
model1 <- lm(Rape ~ Murder + UrbanPop + Assault, data = USArrests)
summary(model1)

#"UrbanPop" and "Assault" are statistically significant predictors of "Rape",
# while "Murder" is not statistically significant
#we can try removing "murder" to see how the model would react , to see if it's statistically
##efficient to remove that variable in our  model.

## BMB: what does this mean?? Why are you doing backward stepwise selection? The only reason
##  to do stepwise selection is as an efficient form of regularization for predictive models.

model2 <- lm(Rape ~ UrbanPop + Assault, data = USArrests)
summary(model2)
#we notice that both "urbanpop" and "assault" are statistically signficant predictors
#of "rape"
#The coefficient for "UrbanPop" (0.16585) indicates that, for each unit increase in the 
#percentage of the urban population, the predicted "Rape" rate increases by approximately 0.16585 units,
#while holding "Assault" constant.
#The coefficient for "Assault" (0.06731) suggests that, for each additional assault arrest per 100,000 residents,
#the predicted "Rape" rate increases by approximately 0.06731 units, holding "UrbanPop" constant.

##when comparing both models, we can conclude that the "urbanpop" and "assault" are more significant
#when we remove the "murder" even if the adjusted R-squared decreased slightly ,
#se-o they can predict "rape" better in the absence of "murder" ,the second model is still more appealing than
#the first one , so we are going to keep the second one

## BMB: this is a bad idea.

###diagnosing the model:
plot(model2, which = 1)  # Residuals vs. Fitted Values Plot
#the fitted values line of the plot is horizontal and the residuals are scattered across it
#which suggets a linear relationship between "rape" and the response variables
##there are few outliers : alaska and Nevada and rhode island

## BMB: these are only the largest three residuals, not 'outliers' in any formal sense

##a normal Q-Q plot for residuals
qqnorm(residuals(model2))
qqline(residuals(model2))

#we created a QQ plot to assess whether the residuals are normally distributed and if there are any outliers.
# From the plot , we notice that the points in the middle of the plot closely follow the straight line which indicates that the
#residuals are approximately normally distributed around the mean. This means that the linear regression model 
#is generally capturing the central part of the data well.
#there's a few deviations on both ends of the tail, which means that the residuals dont follow a perfectly 
#normal distribution (unsymmetrical distribution)
#we can also see a few outliers that deviate completely from the straight line.

## BMB: the residuals are more fat-tailed than asymmetric. (There are approximately equal deviations
## on both ends, in the direction of more extreme residuals.)

plot(model2) #to plot all the plots at the same time
# For the scale-location plot, we notice that the left part of the red line is horizontal which corresponds to a relatively constant 
#spread of residuals, indicating that the assumption of  homoscedasticity is true for a certain range of fitted values. but we also notice the presence 
#of a curvature in that red line which means that the assumption of constant variance is violated for some values of the predictor variables.
#This also means that the spread of residuals becomes larger for higher values of the predictor variables(heteroscedasticity).

###model transformations:
#we can try making adjustements to fix the heteroscedasticity,
# We can apply logarithmic transformation to "Rape" and create a new variable
USArrests$LogRape <- log(USArrests$Rape)
# Then fit the new linear regression model with the transformed response variable
model_transformed <- lm(LogRape ~ UrbanPop + Assault, data = USArrests)
plot(model_transformed)

#we can notice that this approach fixed the heteroscedasticity and the assumption of constant variance is met.

## BMB: I agree, this is much better
MASS::boxcox(model2)
## this also shows that log-transformation is a good idea

###Coefficient plot of the results:
dwplot(model_transformed) + expand_limits(x=0) + geom_vline(xintercept = 0, lty=2)
#interpretation: 
#we notice that the coefficient for "UrbanPop" in the log(Rape) model is 0.0085, this means that for a one-unit 
#increase in "UrbanPop," the log(Rape) is expected to increase by 0.0085. Since exp(0.0085) =1.0085,
#then it implies an approximate 0.85% increase in Rape for a one-unit increase in "UrbanPop.
#And "Assault" has a coefficient of 0.003, indicating that for a one-unit increase in "Assault," the log(Rape) 
#is expected to increase by 0.003, since exp(0.003)=1.0030045045 , then it implies an approximate 0.30% increase
#in Rape for a one-unit increase in "Assault".
#In summary , "UrbanPop" has a larger coefficient, ie: a stronger influence on the response variable.
# And "Assault" has a smaller coefficient, indicating a relatively weaker effect on the response variable.

##I dont think i need to center and scale the predictors since i dont really think i have 
#interpretability problems. The log(rape) response doesn't create necessarily interpretability problems,
#it just changes the interpretation of the coefficients in my model.

###effect plot:

effect_plot<-allEffects(model_transformed)
plot(effect_plot)
#interpretation
#As the percentage of the urban population ("UrbanPop") increases from 30% to 90%, the predicted value of "LogRape" also increases.
#Which means that there is a positive association between the percentage of the urban population and the log-transformed number of rape arrests.
# As urbanization increases, the predicted number of rape arrests tends to be higher.

#As the number of "Assault" arrests (per 100,000 residents) increases from 45 to 340, the predicted value of "LogRape" also increases.
#which means that there is a positive association between the number of assault arrests and the log-transformed number of rape arrests.
#In other words, as the incidence of assault arrests increases, the predicted number of rape arrests tends to be higher.

####question 2:
##BACI designs:
# Let's create the inverse contrast matrix
inverse_contrast <- matrix(
  c(
    0, 0, 1/2, 1/2,
    0, 0, -1, 1,
    0.5, 0.5, -0.5, -0.5,
    -1, 1, 1, -1
  ),
  nrow = 4,
  ncol = 4,
  byrow = TRUE
)

# then calculate the contrast matrix by taking the inverse of the inverse contrast matrix
contrast_matrix <- solve(inverse_contrast)

# printing the contrast matrix
print(contrast_matrix)

# we need to create dummy data for the BACI design
dummy_data <- data.frame(
  period = as.factor(c("after", "after", "before", "before")),
  treatment = as.factor(c("control", "impact", "control", "impact")),
  values = c(80, 65, 34, 26)
)
## BMB: you don't need a response variable for this example ...

# Let's generate a minimal model matrix
minimal_model_matrix <- model.matrix(~period + treatment, data = dummy_data)

# Let's generate a full model matrix
full_model_matrix <- model.matrix(~period * treatment, data = dummy_data)

# lLet's generate an interaction-only model matrix
interaction_model_matrix <- model.matrix(~0 + period:treatment, data = dummy_data)

# Printing the model matrices
print(minimal_model_matrix)
print(full_model_matrix)
print(interaction_model_matrix)

####Question3: 

#function that simulates data for linear regression with optional violations of assumptions
sim_fun <- function(n = 100, slope = 1, sd = 1, intercept = 0, violation = "linearity") {
  x <- runif(n)
  
  if (violation == "linearity") {
    y <- rnorm(n, intercept + slope * x^2, sd = sd)  # Violate linearity with a quadratic relationship
  } else if (violation == "homoscedasticity") {
    y <- rnorm(n, intercept + slope * x, sd = sd * (1 + x))  # Violate homoscedasticity
  } else if (violation == "normality") {
    y <- rt(n, df = 5)  # Violate normality with a t-distribution
  }
  
  data.frame(x, y)
}
# Initialize data frames for results
results_bias <- data.frame(Violation_Type = character(0), Bias = numeric(0))
results_rmse <- data.frame(Violation_Type = character(0), RMSE = numeric(0))
results_power <- data.frame(Violation_Type = character(0), Power = numeric(0))
results_coverage <- data.frame(Violation_Type = character(0), Coverage = numeric(0))

#  function to calculate metrics and append the results
calculate_metrics <- function(sim_data, violation_type) {
  # Fit a linear regression model to the simulated data
  model <- lm(y ~ x, data = sim_data)
  
  # Calculate bias, RMSE, power, and coverage
  estimated_slope <- coef(model)[2]
  bias <- mean(estimated_slope - 1)
  rmse <- sqrt(mean((estimated_slope - 1)^2))
  p_value <- coef(summary(model))[2, "Pr(>|t|)"]
  power <- mean(p_value < 0.05)
  conf_interval <- confint(model)[2, ]
  coverage <- (conf_interval[1] < 1) & (1 < conf_interval[2])

    ## BMB: don't grow matrices; avoid <<- if you can
    ## Why not make one data frame with all the results?
  # Append results to data frames
  results_bias <<- rbind(results_bias, data.frame(violation_type, Bias = bias))
  results_rmse <<- rbind(results_rmse, data.frame(violation_type, RMSE = rmse))
  results_power <<- rbind(results_power, data.frame(violation_type, Power = power))
  results_coverage <<- rbind(results_coverage, data.frame(violation_type, Coverage = coverage))
}

# Example1: Simulate data with a violation of linearity
sim_data1 <- sim_fun(n = 100, slope = 1, sd = 1, intercept = 0, violation = "linearity")
calculate_metrics(sim_data1, "Linearity_Violation")

# Example2: Simulate data with a violation of homoscedasticity
sim_data2 <- sim_fun(n = 100, slope = 1, sd = 1, intercept = 0, violation = "homoscedasticity")
calculate_metrics(sim_data2, "Homoscedasticity_Violation")

# Example3: Simulate data with a violation of normality
sim_data3 <- sim_fun(n = 100, slope = 1, sd = 1, intercept = 0, violation = "normality")
calculate_metrics(sim_data3, "Normality_Violation")

## Create a boxplot for bias

## BMB: this is a boxplot with one observation per group???

library(ggplot2)
ggplot(data = results_bias, aes(x = violation_type, y = Bias)) +
  geom_boxplot() +
  labs(title = "Bias vs. Violation type", y = "Bias")

# Create a scatterplot for RMSE
ggplot(data = results_rmse, aes(x = violation_type, y = RMSE)) +
  geom_point() +
  labs(title = "RMSE vs. Violation type", y = "RMSE")

#for the bias vs violation_type : we notice that both homoscedascticity and normality violation have a bias of -1.75 
#while the linearity violation has a bias of 0.4. this means that the model tends to significantly underestimate the 
#slope when there's a violation of homoscedasticity or normality, while in the case of a linearity violation
# the linear regression model tends to overestimate the slope.
#As for the rsme vs violation_type,we can notice that the homoscedasticity and normality violations have a rmse of 1.4 
#and linearity violation a rmse of 0.24,  which means that the model exhibits relatively low prediction error when linearity is violated.
#While an RMSE of 1.4 indicates that the model's predictions have larger errors when homoscedasticity is violated.

## BMB: mark 7.7

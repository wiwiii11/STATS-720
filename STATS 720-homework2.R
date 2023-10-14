---
title: "STATS - 720 Homework-2"
author: "Liedri Wiam (Student number: 400550999)"
---
  
## Load packages
  
library(rpart)
library(ggplot2)
library(performance)
library(DHARMa)
library(coefplot)
library(bbmle)
library(brglm2)
library(arm)
library(lmtest)





#### Question 1: analyse the dataset

## Load the dataset
data("kyphosis")
kyphosis
summary(kyphosis) 
#summary of the dataset:
#There's data for 64 patients who did not develop kyphosis and 17 who did.

#Age distribution:
#As for the age distribution it ranges from 1 year old to 206 years old(such high age values might be indicators of outliers).

#The number variable is a numeric variable that represents the number of vertebrae levels that were involved during the surgery,
#and it ranges from 2 to 10 vertebrae.

#The start variable represents the starting vertebrae level for the surgery.

#preprocessing:
kyphosis$Kyphosis <- ifelse(kyphosis$Kyphosis == "absent", 0, 1) #transforming the kyphosis variable to a binary variable instead of categorical

# Create a histogram of the age variable:
hist(kyphosis$Age, main = "Distribution of Patient Ages", xlab = "Age", ylab = "Frequency", col = "lightblue")
#There is a high frequency of patients in the early years (from 1 year olds to 20 year olds).
#The dataset shows a broad age range, with ages going from 55 to 175, where there is almost consistent frequency.
#There is a decrease in frequency in the age variable beyond 180 years. Also ages in this range are not common and 
#may indicate data anomalies or outliers.

#Create a boxplot of the age variable:
boxplot(kyphosis$Age, main = "Box Plot of Patient Ages", ylab = "Age", col = "lightblue")
#Based on the boxplot,we notice that there are no outliers in age (it doesn't fall 
# outside of the bounds for potential outliers).

#Create a boxplot of the response variable against the age variable:
boxplot(Age ~ Kyphosis,data = kyphosis, 
        col = "bisque",
        xlab = " Incidence of Pyphosis",
        ylab = "Age (in months)",
        main = "Incidence of Kyphosis vs. Age of Child")



#Based on the description, my analysis strategy is as follows:
#I will include the three predictor variables : age, number and start.
#For this dataset which has a binary response variable(kyphosis), im going to use logistic regression:
#the family is a binomial family with a logit link.



##Plot the data in some sensible way
# Create a histogram of the number variable:
hist(kyphosis$Age, main = "Distribution of vertebrae numbers", xlab = "number of levels", ylab = "Frequency", col = "green")
# Create a histogram of the start variable:
hist(kyphosis$Age, main = "Start distribution", xlab = "Start Vertebrae Level", ylab = "Frequency", col = "pink")

#create a barplot for the kyphosis variable frequency distribution(abscent or present: 0 or 1)
ggplot(kyphosis, aes(x = Kyphosis, fill = Kyphosis)) +
  geom_bar() +
  labs(title = "Kyphosis Distribution", x = "Kyphosis", y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

# Create a scatterplot for Age vs Number
plot(kyphosis$Age, kyphosis$Number, 
     main = "Scatterplot: Age vs. Number",
     xlab = "Age (in months)", 
     ylab = "Number of Levels",
     col = "blue")

# Create a scatterplot for Age vs Start
plot(kyphosis$Age, kyphosis$Start, 
     main = "Scatterplot: Age vs. Start",
     xlab = "Age (in months)", 
     ylab = "Start Vertebrae Level",
     col = "red")

# Create a scatterplot for number vs Start
plot(kyphosis$Number, kyphosis$Start, 
     main = "Scatterplot: Number vs. Start",
     xlab = "Number of Levels", 
     ylab = "Start Vertebrae Level",
     col = "black") # no direct linear relationship

##Fit a logistic regression model:
model <- glm(Kyphosis ~ Age + Number + Start, data = kyphosis, family = binomial(link = "logit"))

##comparing plots
#R base plots: # Put the base R plots all in one page
par(mfrow = c(2, 2))

# Residual vs. Fitted Values
plot(model, which = 1)

# Q-Q Plot
plot(model, which = 2)

#Scale location plot
plot(model, which = 3)

# Cook's Distance Plot
plot(model, which = 4)

par(mfrow = c(1, 1))#reset

#performance package plots 
check_model(model)

#DHARMa plot:
# Create a dharma QQ plot
DHARMa::plotQQunif(model)


##comparison of the plots:
#Both the normality residuals plot from the performance package and the Q-Q plot from base R
#serve the same purpose which is assessing the normality of the residuals but in different ways.
# In both plots we can observe  a resemblance in the distribution of the dots, the dots are distributed along the line with a few points deviating
#from the straight line which may suggest potential non-normality in the residuals, and that this model might not be a good fit.

#we can also compare the Q-Q residuals plot from the DHARMa package to the residuals vs. fitted plot from base R:
#The Q-Q residuals plot in the DHARMa package compares the quantiles of the observed residuals to the quantiles of 
#the residuals expected under our fitted model.In this plot we can see that the points follow a straight line, which
#means that the residuals are normally distributed.
#The residuals vs. fitted plot in base R, on the other hand, shows the actual residuals (observed) on the y-axis 
#against the fitted values on the x-axis, and we observe a straight line at first around y=0 which means that the model's residuals are relatively consistent 
#and then there's a slight deviation at the tail which could indicate a problem with the linearity assumption and the presence of some points scattered above zero
#mean that the model is biased.
#This suggests a non linearity problem in the model or heteroscedasticity.


#Interpret your results and draw coefficient plots
coefplot(model)
#we observe from the coef plot that the coef estimate of "Number" is the one with the most impact on the response variable "kyphosis",
#while the coef estimate of both "age" and "start" have very low impact ont the response variable.
#The positive coefficient for 'Number' suggests that an increase in the number of vertebrae levels involved in the surgery is associated with a higher 
#likelihood of kyphosis being present.
#On the other hand, the coefficient estimates for 'Age' and 'Start' appear to have relatively lower impact. For 'Age,' the coefficient is zero, indicating 
#that a one-unit increase in age results in no change in the odds of kyphosis. 
#Meanwhile, the coefficient for 'Start' is -0.25, suggesting that a one-unit change in the starting vertebrae level results in a small negative change in the odds of kyphosis.




### question 2:
#Gopher tortoise example
g_url <- "https://raw.githubusercontent.com/bbolker/mm_workshops/master/data/gopherdat2.csv"
g_data <- read.csv(g_url)

##plot the data

# Create your individual plots for numerical variables(pre, Area, density):
plot(g_data$shells, g_data$prev, main = "Scatter Plot of number of Shells by seroprevalence")
plot(g_data$shells, g_data$Area, main = "Scatter Plot of number of Shells by area")
plot(g_data$shells, g_data$density, main = "Scatter Plot of number of Shells by density")


# Create a bar plot for the number of shells by 'Site'
ggplot(data = g_data, aes(x = Site, y = shells)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Site", y = "Shells", title = "Bar Plot of number of Shells by Site")

# Create a bar plot for the number of shells by 'year'
ggplot(data = g_data, aes(x = year, y = shells)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "year", y = "Shells", title = "Bar Plot of number Shells by year")

# Fit a Poisson GLM to account for count data (shells)
model2 <- glm(shells ~ year + prev + offset(log(Area)), data = g_data, family = poisson)

# Check for overdispersion
summary(model2)
#The dispersion parameter for the Poisson family is set to 1, which suggests that the chosen model is not indicating overdispersion and is appropriate.

#model3 <- glm(shells ~ year + prev + Site+ offset(log(Area)), data = g_data, family = poisson)
#summary(model3)

#model4 <- glm(shells ~ year + prev + density + offset(log(Area)), data = g_data, family = poisson)
#summary(model4)

#We choose the model with the lowest AIC which is the model2.


###Using mle2 function from the 'bbmle' package
# We define the Poisson log-likelihood function
poisson_loglik <- function(logmu, y) {
  lambda <- exp(logmu)
  y <- data$shells
  -sum(dpois(y, lambda, log = TRUE))
}


# Fitting the model using mle2
fit_model <- mle2(shells ~ dpois(lambda=exp(loglambda)*Area), parameters=list(loglambda~factor(year)+prev), data=g_data, start=list(loglambda=0))

# Summary of the model
summary(fit_model)


##Writing a custom negative log-likelihood function:
# Define the negative log-likelihood function for a Poisson GLM
custom_poisson_loglik <- function(log_mean, data) {
  lambda <- exp(log_mean)  # Convert log(mean) to mean
  y <- data$shells
  -sum(-lambda + y * log(lambda) - log(factorial(y)))
}

# Initial parameter values fro mle2 and optim
start_values_mle2 <- list(log_mean = log(mean(g_data$shells)))
start_values_optim <- log(mean(g_data$shells))

                     
# Fit the model using mle2 from bbmle
model_bbmle <- mle2(custom_poisson_loglik, start = start_values_mle2, data = list(data = g_data))

#fit the model using mle2 from optim
model_optim <- optim(start_values_optim, fn = custom_poisson_loglik, data = g_data, method = "L-BFGS-B")


###Compare the coefficients:

# Get the parameter estimates
parameters_bbmle <- coef(model_bbmle)
parameters_optim <- model_optim$par
#the values are identical

# Calculating Wald CIs
# We assume 95% for a confidence level
SE_bbmle <- sqrt(vcov(model_bbmle)[1, 1])
Z <- 1.96
Wald_CI_bbmle <- parameters_bbmle + c(-1, 1) * Z * SE_bbmle
#Calculating the profile CI
profile_CIs_bbmle <- confint(model_bbmle)


# Print parameter estimates and CIs
cat("Parameter Estimates - bbmle:", parameters_bbmle, "\n")
cat("Parameter Estimates - optim:", parameters_optim, "\n")
cat("Wald CI - bbmle:", Wald_CI_bbmle, "\n")
cat("Profile CIs - bbmle:", profile_CIs_bbmle, "\n")
#the wald CI is wider than the profile CI, the profile CI might give a more accurate representation
#of the parameter's estimation.


###question 3:
#Endometrial data
# Load the endometrial data
data(endometrial)
#explore the data
summary(endometrial)
str(endometrial)

# Fitting the model using regular glm()
model_glm <- glm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link="logit"))
# Fitting the model using arm::bayesglm() with logit link function
model_bayesglm <- bayesglm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link = "logit"))

# Fitting the model using glm with method = "brglmFit" and logit link function
model_brglmFit <- glm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link = "logit"), method = "brglmFit")

# Summary of glm model
summary(model_glm)

# Summary of bayesglm model
summary(model_bayesglm)

# Summary of brglmFit model
summary(model_brglmFit)


#We can observe that the parameter estimates, standard errors, and AIC values vary slightly between the 3 models.
#The differences between the 3 models is due to the different estimation techniques used by each method.
#The regular glm model calculates maximum likelihood estimates, while the Bayesian glm is more of a Bayesian approach.
#we can see that the Bayesian glm model has smaller estimates for the coefficients with small magnitude
#compared to the other two models. The differences in standard errors, residual deviance, and AIC values are also relatively small.




#likelihood test for each parameter:
reduced_model_NV <- glm(HG ~ PI + EH, family = binomial(link = "logit"), data = endometrial)
reduced_model_EH <- glm(HG ~ PI + NV, family = binomial(link = "logit"), data = endometrial)
reduced_model_PI <- glm(HG ~ NV + EH, family = binomial(link = "logit"), data = endometrial)

# Perform the likelihood ratio test for the NV variable
lrt_result_NV <- lrtest(model_glm, reduced_model_NV)
# Perform the likelihood ratio test for the EH variable
lrt_result_EH <- lrtest(model_glm, reduced_model_EH)
# Perform the likelihood ratio test for the PI variable
lrt_result_PI <- lrtest(model_glm, reduced_model_PI)

print(lrt_result_NV)
print(lrt_result_EH)
print(lrt_result_PI)
##comparison:
#For the NV variable:
#the likelihood ratio test (lrt_result_NV) shows a significant result with p-value 0.002221, indicating that the 
#NV variable is a significant predictor.

#For the EH variable:
#The likelihood ratio test (lrt_result_EH) also shows a highly significant result with a very small p-value (8.777e-06),
#indicating that the EH variable is a highly significant predictor.

#For the PI variable:
#The likelihood ratio test (lrt_result_PI) shows a p-value of 0.3209, which is greater than 0.05.
#This suggests that the PI variable may not be a significant predictor in the model.

#Both NV and EH seem to be significant predictors of the HG variable.
#PI is not a significant predictor based on the likelihood ratio test.


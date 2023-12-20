#load necessary packages
library(faraway)
library(ggplot2)
library(lme4)
library(ggeffects)
library(mlmRev)
library(MASS)
library(glmmTMB)
library(MCMCglmm)






# Load the Nepali dataset
data(nepali)

# View the structure of the dataset
str(nepali)
summary(nepali) #There's 123 missing values in height and weight( ht and wt)


# Count the percentage of missing values to decide what to do :
missing_wt <- sum(is.na(nepali$wt))
missing_ht <- sum(is.na(nepali$ht))

# Calculate percentage of missing values
percentage_missing_wt <- (missing_wt / length(nepali$wt)) * 100
percentage_missing_ht <- (missing_ht / length(nepali$ht)) * 100

percentage_missing_wt
percentage_missing_ht
#with around 12.3% missing values, imputation could be a reasonable approach to retain the existing information while filling in the gaps,
#instead of completely removing them

# Impute missing values with mean
mean_wt <- mean(nepali$wt, na.rm = TRUE)
mean_ht <- mean(nepali$ht, na.rm = TRUE)

## BMB: this is a big topic that I didn't get a chance to cover
## both single (e.g. mean) imputation and casewise deletion are
##  potentially problematic ...
nepali$wt[is.na(nepali$wt)] <- mean_wt
nepali$ht[is.na(nepali$ht)] <- mean_ht

# Convert 'sex' variable into a factor with levels "male" and "female"
nepali$sex <- factor(nepali$sex, levels = c(1, 2), labels = c("male", "female"))

# Plot the data (age vs. weight, distinguishing data points for each child)
plot(nepali$age, nepali$wt)

ggplot(nepali, aes(x = age, y = wt, color = as.factor(id))) +
  geom_point(alpha = 0.5, size = 3) +
  labs(title = "Age vs. Weight for Each Child",
       x = "Age",
       y = "Weight") +
  theme_minimal() +
  guides(color = FALSE)
##Hard to visualize when trying to assign different colors to each child due to a large number of children
## BMB: good. You can also use group = id
ggplot(nepali, aes(x = sex, y = wt, color = as.factor(id))) +
  geom_point(alpha = 0.5, size = 3) +
  labs(title = "Age vs. Weight for Each Child",
       x = "Sex",
       y = "Weight") +
  theme_minimal() +
  guides(color = FALSE)

#plot wt vs. age group by sex
ggplot(nepali, aes(x=age,y=wt,group=id,color=sex)) +
  geom_point(alpha = 0.5, size = 3) +
  labs(title = "Weight vs. age grouped by sex",
       x = "age",
       y = "Weight") +
  theme_minimal() +
  guides(color = FALSE)
#the effect of 'age' on 'wt' varies among different children or households ('id')


#Considering that the data is hierarchically structured, where multiple children are nested within mothers,
#I will use a mixed-effects model to account for this hierarchy.

# Fitting a mixed-effects model
mixed_model <- lmer(wt ~ age + mage + lit + sex + (1 | id), data = nepali)
summary(mixed_model)

#here's an interpretation based on the summary's output:
#Fixed Effects:
##Intercept: The estimated intercept is 5.846030, This suggests that when all other predictors are zero (age,mage, sex, and lit), the estimated weight is approximately 5.846030.
## BMB: too many significant digits?
#Age: For every one-unit increase in age, there's an estimated average increase of 0.116132 in wt(statistically significant) It suggests that as children get older, their weight tends to increase on average.
#Sex (Female): The coefficient for 'sexfemale' is -0.4353. This suggests that, on average, females have an estimated weight that is around 0.4353 units lower than males
#Literacy (Lit): The effect of mother's literacy on weight is estimated to be 0.696403 . Though positive, it's not statistically significant.
#mage: For every one-year increase in the mother's age, there's an estimated average increase of 0.037980 kilograms in the child's weight(statistically significant but smaller compared to the effect of 'age').

#Random Effects:
#Id: The random effects show that there's variability in weight between different children ('id'). The estimated variance for 'id' is 1.456, suggesting variability in weight that's specific to individual children after 
#accounting for fixed effects.
#The correlation matrix indicates the correlation coefficients between the fixed effects(maybe high multicollinearity between mage and age)


plot(mixed_model)
###the plot shows a random scatter of points around the horizontal line at zero without any distinct pattern,which means that the assumption of constant variance (homoscedasticity) is met.
## BMB: you can see that the imputed points are weird!

#Normal q-q plot:
qqnorm(resid(mixed_model))
qqline(resid(mixed_model), col = 2)
##the points closely follow a diagonal line with a slight curvature, which suggests that in general, the residuals are approximately normally distributed.
## BMB: this is worse than you think (see added QQ line)


# Create a plot with predictions
plot_data1 <- ggpredict(mixed_model, c("age", "sex"))
plot(plot_data1)
plot_data2 <- ggpredict(mixed_model,terms="mage")
plot(plot_data2)
plot_data3 <- ggpredict(mixed_model,terms="lit")
plot(plot_data3)
#the lines representing different sexes run parallel without crossing or overlapping much as age increases
# This indicates that the relationship between 'age' and 'weight' is similar for both sexes
#on average, the rate of weight gain (or change) with age is similar between males and females in the dataset.
#The absence of crossing lines indicates that there might not be a significant interaction effect between 'age' and 'sex' in influencing 'weight'.

##Comparison with the paper "west":
#When comparing the analysis in the original paper [@west] , it is different from the approach I took earlier. In the paper, Nepali children were divided into distinct groups:
#a control group and a treatment group. The treatment group received regular doses of vitamin A every 4 months and were observed over a 16-month period. The study aimed to assess 
#the impact of vitamin A supplementation on various anthropometric measures such as arm circumference, weight, height, muscle, and fat area. In this particular investigation, the crucial grouping factor 
#lies in the distinction between the 'control' and 'treatment' groups.


#PART 2:
# Load the Contraception dataset
data("Contraception")

str(Contraception)
summary(Contraception)

# Create age groups based on centered age
Contraception$age_group <- ifelse(Contraception$age < 0, "Below Mean Age", "Above Mean Age")

# Plot contraceptive use by age group
ggplot(Contraception, aes(x = age_group, fill = use)) +
  geom_bar() +
  labs(title = "Contraceptive Use by Age Group (Relative to Mean Age)")

#For my model i will use a glmer model.Since the data involves women nested within districts and different contraceptive use patterns between urban and rural areas,
#a GLMM (glmer) is  appropriate.

#fitting a GLMER model :
# Assuming 'age', 'livch', 'urban' are predictors
glmer_model <- glmer(use ~ livch+age+urban+ (1 | district), family = binomial, data = Contraception)
summary(glmer_model)
#Significant Predictors:
#Having one (livch1), two (livch2), or three or more (livch3+) living children significantly relate to higher contraceptive use.
#Urban residence (urbanY) is positively associated with increased contraceptive use.
#Older age (age) is negatively associated with contraceptive use.
plot(glmer_model)
## BMB: what about this?

contra1 <- ggpredict(glmer_model,terms=c("urban","district[1,2,3,4,5]"),type="random")
plot(contra1,connect_lines = FALSE)

# plot predicted prob of usage vs age
contra2 <- ggpredict(glmer_model,terms=c("age","urban"),type="fixed")
contra.plt <- plot(contra2,connect_lines = FALSE)

# Fit models
# Model 1: glm
glm_model <- glm(use ~ livch + age + urban, family = binomial, data = Contraception)

# Model 2: glmmPQL
glmmPQL_model <- glmmPQL(use ~ livch + age + urban, random = ~ 1 | district,
                         family = binomial, data = Contraception)

# Model 3: Laplace approximation using glmer
glmer_model <- glmer(use ~ livch + age + urban + (1 | district),
                     family = binomial, data = Contraception)

# Model 4: Adaptive Gauss-Hermite quadrature using 20 quadrature points
glmer_quad_model <- glmer(use ~ livch + age + urban + (1 | district),
                          family = binomial, data = Contraception,
                          nAGQ = 20)


# Extract coefficients from each model
coefficients_df <- data.frame(
  "glm" = coef(glm_model),
  "glmmPQL" = fixef(glmmPQL_model),
  "glmer" = fixef(glmer_model),
  "glmer_quad" = fixef(glmer_quad_model)
)

# Create coefficient plot
barplot(t(coefficients_df), beside = TRUE, legend.text = TRUE,
        args.legend = list(x = "bottomright"), col = rainbow(4),
        main = "Comparison of Fixed-Effect Parameters")
##we can observe from the fixed coefficient plot that the fixed effects do not differ from each other significantly for the four different methods

## BMB: how do you conclude 'significantly'?
## should probably scale age if including it in a plot with other coeffs

#PART 3:
# Fit Bayesian GLMM
model_bayes <- MCMCglmm(
  wt ~ age + mage + lit + sex,
  random = ~id,  
  data = nepali,
  family = "gaussian",
  verbose = FALSE
)

# Check model summary
summary(model_bayes)
#Fixed effects interpretation:
#Intercept (Baseline Weight): 5.845729
#age: For each unit increase in age, there's an estimated weight increase of 0.116023.
#mage: Estimated weight increase per unit increase in mage is 0.038106.
#lit (Literacy): The effect of literacy on weight is less conclusive as the 95% credible interval spans from negative to positive values.
#sexfemale: The effect of gender (specifically female) on weight is not statistically significant as the credible interval includes zero.
#Interpretation: Age and mage seem to have a significant positive effect on weight. Literacy and gender (female) dont show a clear impact on weight 
#based on the given credible intervals.

## BMB: what about "two different Bayesian packages"?

#PART 4:
# simfun Function
simfun <- function(beta, theta, n, ngrp) {
  x <- rnorm(n)
  g <- as.factor(rep(1:ngrp, each = n/ngrp))
  true <- exp(beta[1] + beta[2] * x + theta * rnorm(ngrp))
  y <- unlist(lapply(true, function(t) rpois(n/ngrp, lambda = t)))
  data <- data.frame(y = y, x = x, g = g)
  return(data)
}


# fitfun Function
fitfun <- function(data, nAGQ) {
  if (nAGQ == -2) {
    fit <- glm(y ~ x + (1 | g), family = poisson, data = data)
  } else if (nAGQ == -1) {
    fit <- glmmPQL(y ~ x, random = ~ 1 | g, family = poisson, data = data)
  } else {
    fit <- glmer(y ~ x + (1 | g), family = poisson, data = data, nAGQ = nAGQ)
  }
  
  coef <- summary(fit)$coefficients
  return(coef)
}
set.seed(123)  
# Running the simulation study
beta1 <- c(-2, 0.5)
beta2 <- c(2, 0.5)
theta <- 1
n <- 500
ngrp <- 100
n_simulations <- 100

results <- list()

for (i in 1:n_simulations) {
  data_beta1 <- simfun(beta1, theta, n, ngrp)
  data_beta2 <- simfun(beta2, theta, n, ngrp)
  
  fit_beta1_pql <- fitfun(data_beta1, -1)
  fit_beta1_laplace <- fitfun(data_beta1, 1)
  fit_beta1_aghq <- fitfun(data_beta1, 20)
  
  fit_beta2_pql <- fitfun(data_beta2, -1)
  fit_beta2_laplace <- fitfun(data_beta2, 1)
  fit_beta2_aghq <- fitfun(data_beta2, 20)
  
  # Extract coefficients for beta[2]
  coef_beta2_pql <- fit_beta2_pql[2, ]
  coef_beta2_laplace <- fit_beta2_laplace$coefficients[2, ]
  coef_beta2_aghq <- fit_beta2_aghq$coefficients[2, ]
  
  # True value of beta[2]
  true_beta2 <- beta2[2]
  
  # Calculate bias
  bias_pql <- mean(coef_beta2_pql - true_beta2)
  bias_laplace <- mean(coef_beta2_laplace - true_beta2)
  bias_aghq <- mean(coef_beta2_aghq - true_beta2)
  
  # Calculate variance
  var_pql <- var(coef_beta2_pql)
  var_laplace <- var(coef_beta2_laplace)
  var_aghq <- var(coef_beta2_aghq)
  
  # Calculate scaled RMSE
  scaled_rmse_pql <- sqrt(mean(((coef_beta2_pql / true_beta2) - 1)^2))
  scaled_rmse_laplace <- sqrt(mean(((coef_beta2_laplace / true_beta2) - 1)^2))
  scaled_rmse_aghq <- sqrt(mean(((coef_beta2_aghq / true_beta2) - 1)^2))
  
  # Calculate coverage
  ci_pql <- confint(fit_beta2_pql)[2, ]
  ci_laplace <- confint(fit_beta2_laplace)[2, ]
  ci_aghq <- confint(fit_beta2_aghq)[2, ]
  
  coverage_pql <- ifelse(true_beta2 >= ci_pql[1] && true_beta2 <= ci_pql[2], 1, 0)
  coverage_laplace <- ifelse(true_beta2 >= ci_laplace[1] && true_beta2 <= ci_laplace[2], 1, 0)
  coverage_aghq <- ifelse(true_beta2 >= ci_aghq[1] && true_beta2 <= ci_aghq[2], 1, 0)
  
  # Store results in 'results' list
  results[[i]] <- list(
    bias_pql = bias_pql,
    bias_laplace = bias_laplace,
    bias_aghq = bias_aghq,
    var_pql = var_pql,
    var_laplace = var_laplace,
    var_aghq = var_aghq,
    scaled_rmse_pql = scaled_rmse_pql,
    scaled_rmse_laplace = scaled_rmse_laplace,
    scaled_rmse_aghq = scaled_rmse_aghq,
    coverage_pql = coverage_pql,
    coverage_laplace = coverage_laplace,
    coverage_aghq = coverage_aghq
  )
}

# Combine results from all simulations
all_results <- do.call(rbind, results)

# Calculate average values for each metric across all simulations
avg_bias_pql <- mean(all_results$bias_pql)
avg_bias_laplace <- mean(all_results$bias_laplace)
avg_bias_aghq <- mean(all_results$bias_aghq)

avg_var_pql <- mean(all_results$var_pql)
avg_var_laplace <- mean(all_results$var_laplace)
avg_var_aghq <- mean(all_results$var_aghq)

avg_scaled_rmse_pql <- mean(all_results$scaled_rmse_pql)
avg_scaled_rmse_laplace <- mean(all_results$scaled_rmse_laplace)
avg_scaled_rmse_aghq <- mean(all_results$scaled_rmse_aghq)

avg_coverage_pql <- mean(all_results$coverage_pql)
avg_coverage_laplace <- mean(all_results$coverage_laplace)
avg_coverage_aghq <- mean(all_results$coverage_aghq)

# Print the average results
cat("Average Bias - PQL:", avg_bias_pql, "\n")
cat("Average Bias - Laplace:", avg_bias_laplace, "\n")
cat("Average Bias - AGHQ:", avg_bias_aghq, "\n")

cat("\nAverage Variance - PQL:", avg_var_pql, "\n")
cat("Average Variance - Laplace:", avg_var_laplace, "\n")
cat("Average Variance - AGHQ:", avg_var_aghq, "\n")

cat("\nAverage Scaled RMSE - PQL:", avg_scaled_rmse_pql, "\n")
cat("Average Scaled RMSE - Laplace:", avg_scaled_rmse_laplace, "\n")
cat("Average Scaled RMSE - AGHQ:", avg_scaled_rmse_aghq, "\n")

cat("\nAverage Coverage - PQL:", avg_coverage_pql, "\n")
cat("Average Coverage - Laplace:", avg_coverage_laplace, "\n")
cat("Average Coverage - AGHQ:", avg_coverage_aghq, "\n")


## mark: 9/10



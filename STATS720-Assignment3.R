# Load required packages
library(mlmRev)
library(lmerTest)
library(nlme)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(broom.mixed)
library(car)
library(dplyr)
library(pbkrtest)




# Fit models
lmer_model <- lmer(cog ~ age + trt + (1 + age | id), data = Early)
summary(lmer_model)
lme_model <- lme(cog ~ age + trt, random = ~1 + age | id, data = Early, control = lmeControl(opt = "optim"))
summary(lme_model)
# Compare log-likelihoods
logLik(lmer_model)
logLik(lme_model)
#Based on the log-likelihood values, the lmer model and the lme model have very similar log-likelihoods.
#The log-likelihoods are almost identical, and the difference is minimal. In this case, it's challenging to conclude.
#But i would say that since the lmer model has a slightly higher log-likelihood , then it's a better fit than the lme model.

#2) Extract the fixed-effect coefficients and create a coefficient plot to compare estimated values and SEs between packages.
# Create a copy of the dataset and scale the 'age' variable
early_scaled <- Early
early_scaled$age <- scale(Early$age)

# Fit the lmer and lme models with the scaled 'age' variable
early_sca_lmer <- lmer(cog ~ age + trt + (1 + age | id), data = early_scaled)
early_sca_lme <- lme(fixed = cog ~ age + trt, random = ~1 + age | id, data = early_scaled, control = lmeControl(opt = "optim"))

# Extract fixed-effect coefficients and create coefficient plots
lmer_coefficients <- fixef(early_sca_lmer)
lme_coefficients <- fixef(early_sca_lme)

# Create coefficient plots
dwplot(early_sca_lmer)
dwplot(early_sca_lme)

# Print the fixed-effect estimates
lmer_coefficients
lme_coefficients

# get denominator degrees of freedom
anova(early_sca_lme)
anova(early_sca_lmer)

# Calculate the difference in SEs between the two models
se_lmer <- sqrt(diag(vcov(early_sca_lmer)))
se_lme <- sqrt(diag(vcov(early_sca_lme)))

se_difference <- se_lmer - se_lme
se_difference

#The estimated values for the intercept, 'age,' and 'trtY' are quite similar between the lmer and lme models. 
#There are slight differences, but they are generally in the same range. 

#The estimated denominator degrees of freedom show significant differences between the two models. 
#The lme model has 205 degrees of freedom for 'age' and 101 degrees of freedom for 'trt,' whereas the lmer model has approximately 108.83 degrees of freedom for 'age' and 101.06 degrees of freedom for 'trt.' 
#So they are very similar in terms of 'trt' and different in terms of 'age'

#the differences in the estimated errors are very small, so the estimates are very similar etween the two models

#3) Compare the estimated denominator df (ddf) for the lmer fit with the Satterthwaite vs. Kenward-Roger approximations.
anova(early_sca_lmer,ddf="Satterthwaite")
anova(early_sca_lmer,ddf="Kenward-Roger")

#In this specific example, the differences in ddf between the Satterthwaite and Kenward-Roger approximations appear to be
#relatively small, and they do not appear to have a substantial impact on the results since both methods have significant p-values (< 0.001) for the predictors 'age' and 'trt'.

#4) For the lmer fit, plot the random effect of age for each level (deviation of the slope from the population-level slope) against the corresponding random intercept
# Extract random effects for the 'age' predictor
# Extract random effects
random_effects <- ranef(early_sca_lmer)

# Create a data frame for random effects
random_effects_df <- data.frame(
  RandomIntercept = random_effects[[1]]$`(Intercept)`,
  RandomAgeEffect = random_effects[[1]]$age
)

# Create the scatter plot
with(random_effects_df, plot(
  RandomIntercept,
  RandomAgeEffect,
  xlab = "Random intercept",
  ylab = "Random effect of age",
  main = "random effect of age for each level against the corresponding random intercept"
))

# Treating 'trt' as a random variable doesn't make sense because it represents two possibilities : 'YES' or 'NO' , whether the infant was in the treatment group who were chosen to be exposed to an enriched environment or not
# and they don't naturally vary between individuals. In this type of model,we usually use 'trt' as a fixed effect to see how it affects the outcome.


#We consider modeling 'age' as both a random and fixed effect because we want to account for variations in 'age' across individuals, but we also wish to assess its impact within our observed age range. 
#This way we can capture the diversity of the 'age' values in our sample while investigating its influence on cognitive scores within our specific dataset.

# Fit the model with independent intercept and age variation
model_random_slope <- lmer(cog ~ age + trt + (1 + age | id), data = early_scaled)

# Fit the model with intercept variation only
model_intercept_only <- lmer(cog ~ age + trt + (1 | id), data = early_scaled)

# Perform LRT to compare models
lrt_random_slope <- anova(model_random_slope, early_sca_lmer)

lrt_intercept_only <- anova(model_intercept_only, early_sca_lmer)
#when comparing models with random slopes, the models are equivalent, as they result in the same AIC, BIC, log-likelihood, and deviance. When comparing models with an intercept-only random effect, 
#the model with random slopes is better, but the difference is not statistically significant according to the Likelihood Ratio Test.

# Implement parametric bootstrap 
# For model with independent slope/intercept vs. full model
pb_independent <- PBmodcomp(model_random_slope, early_sca_lmer, nsim = 1000)

# For model with intercept only vs. full model
pb_intercept_only <- PBmodcomp(model_intercept_only, early_sca_lmer, nsim = 1000)




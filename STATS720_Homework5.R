# Load the libraries
library(mgcv)
library(lme4)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(ggeffects)
library(ggemmeans)
library(gridExtra)

# Loading the data
data("Contraception", package = "mlmRev")

# Converting the response variable to numeric
Contraception <- transform(Contraception,
                           use_n = as.numeric(use) - 1,
                           age_sc = drop(scale(age)))

# Fitting a glmer model
glmer_model <- glmer(use_n ~ age_sc * urban + (1 | district),
                     data = Contraception,
                     family = binomial)

# Fitting a gam model
gam_model <- gam(use_n ~ s(age_sc) + urban + s(age_sc, by = urban) + s(district, bs = "re"),
                 data = Contraception,
                 family = binomial)

# Comparing the estimates and standard errors for fixed-effects coefficients
glmer_summary <- tidy(glmer_model, effects = "fixed")
gam_summary <- tidy(gam_model, effects = "fixed")

# Print the summaries
glmer_summary
gam_summary





# GLMER Model:
# The intercept significantly differs from zero with a small standard error of 0.0852 and a very small p-value.
# For the age coefficient, the estimate of 0.101 suggests a positive association with the log-odds of the response variable being 1, when holding other variables constant. 
# However, the evidence for this effect is not strong, as the p-value (0.0775) is greater than 0.05.
# Regarding the urban/rural variable, an estimated coefficient of 0.654 indicates that urban areas (coded as 1) show higher log-odds compared to rural areas (coded as 0).
# The small p-value signifies statistical significance for this relationship.
# The interaction between age and urban/rural has an estimated coefficient of -0.0711 with a standard error of 0.108. The p-value (0.510) suggests insufficient evidence 
#to reject the null hypothesis of no interaction effect.
# GAM Model:
# The smooth term for age is statistically significant (< 0.05), indicating a nonlinear relationship between age and the log-odds of the response variable being 1. 
# This suggests a complex relationship that goes beyond linearity.
# Involving the interaction between age and urban/rural, neither of the smooth terms appears statistically significant. This indicates that these interaction terms do not notably contribute to the model.
# The smooth term for district is statistically significant,the effective degrees of freedom point to a complex and potentially spatially varied effect of district on the response.
# In summary, the gam model demonstrates a nonlinear relationship between age and the log-odds of the response variable being 1.
# However, the interaction terms and district-specific smooth term do not seem to significantly contribute to the model.


# Fitting a model with a fixed quadratic function of age
gam_quad_model <- gam(use_n ~ poly(age_sc, 2) + urban + s(district, bs = "re"),
                      data = Contraception,
                      family = binomial)

# Fitting a model with an effect of age modeled as a thin-plate spline
gam_spline_model <- gam(use_n ~ s(age_sc, bs = "tp") + urban + s(district, bs = "re"),
                        data = Contraception,
                        family = binomial)

# The predicted values for the model with a fixed quadratic function of age
pred_quad <- as.data.frame(predict(gam_quad_model, type = "response", se.fit = TRUE))

pred_quad <- cbind(Contraception, pred_quad)

# Plotting the predictions for the model with a fixed quadratic function of age
plot1 <- ggplot(pred_quad, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Model with Fixed Quadratic Function of Age")

# The predicted values for the model with a thin-plate spline for age
pred_spline <- as.data.frame(predict(gam_spline_model, type = "response", se.fit = TRUE))

pred_spline <- cbind(Contraception, pred_spline)

# Plotting the predictions for the model with a thin-plate spline for age
plot2 <- ggplot(pred_spline, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Model with Thin-Plate Spline for Age")

# Display the plots side by side
grid.arrange(plot1, plot2, ncol = 2)

######################################################################
# Creating a quadratic-age term  for urban and rural settings
Contraception$age_sc_squared_urban <- ifelse(Contraception$urban == "Y", poly(Contraception$age_sc, 2), 0)
Contraception$age_sc_squared_rural <- ifelse(Contraception$urban == "N", poly(Contraception$age_sc, 2), 0)

# Fitting a model with a quadratic-age urban/rural interaction
gam_interaction_model <- gam(use_n ~ s(age_sc, by = urban) + age_sc_squared_urban + age_sc_squared_rural + s(district, bs = "re"),
                             data = Contraception,
                             family = binomial)


# Fitting a model with separate thin-plate splines for age_sc (for urban and rural settings)
gam_splines_model <- gam(use_n ~ s(age_sc, bs = "tp", by = urban) + s(district, bs = "re"),
                         data = Contraception,
                         family = binomial)

# The predicted values for the model with a quadratic-age urban/rural interaction
pred_interaction <- as.data.frame(predict(gam_interaction_model, type = "response", se.fit = TRUE))

# Combine predicted values with the original data
pred_interaction <- cbind(Contraception, pred_interaction)

# Plotting the predictions for the model with a quadratic-age urban/rural interaction
plot3 <- ggplot(pred_interaction, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Quadratic-Age Urban/Rural Interaction")

# Display the plot
print(plot3)

# Get predicted values for the model with separate thin-plate splines for age_sc for urban and rural settings
pred_splines <- as.data.frame(predict(gam_splines_model, type = "response", se.fit = TRUE))

# Combine predicted values with the original data
pred_splines <- cbind(Contraception, pred_splines)

# Plot the predictions for the model with separate thin-plate splines for age_sc for urban and rural settings
plot4 <- ggplot(pred_splines, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Separate Thin-Plate Splines for Age_sc by Urban/Rural")

# Display the plots side by side
print(plot4)

##############################################################
#glmer model 

# Fitting a glmer() model with a fixed quadratic-age by urban/rural interaction and a random effect for districts
glmer_model <- glmer(use_n ~ poly(age_sc, 2) * urban + (poly(age_sc, 2) | district),
                     data = Contraception,
                     family = binomial)

#model with gam 

# Fitting a gam() model with different population-level smooths for urban vs rural and different age-smooths for each district
gam_model <- gam(use_n ~ s(age_sc, by = urban) + s(age_sc, district, bs = "fs"),
                 data = Contraception,
                 family = binomial)

# Generatting predictions for model (a) using glmer()
pred_glmer <- as.data.frame(predict(glmer_model, newdata = nd, type = "response", re.form = NA))

# Generatting predictions for model (b) using gam()
pred_gam <- as.data.frame(predict(gam_model, newdata = nd, type = "response"))

pred_glmer <- cbind(nd, pred_glmer)
pred_gam <- cbind(nd, pred_gam)

# Plotting the results -

# For model (a)
p_glmer <- ggplot(pred_glmer, aes(x = age_sc, y = fit, color = urban, group = district)) +
  geom_line() +
  ggtitle("Model (a) with glmer()")

# For model (b)
p_gam <- ggplot(pred_gam, aes(x = age_sc, y = fit, color = urban, group = district)) +
  geom_line() +
  ggtitle("Model (b) with gam()")

# Display the plots 
grid.arrange(p_glmer, p_gam, ncol = 2)

#The lines in Plot2 graph look wider at the ends than those in the plot1 model , the urban Y line is higher around the average age compared to the urban N line

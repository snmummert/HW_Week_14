######################
##### HW 14 ######
## Sophia Mummert ####
#######################

## libraries ##
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)

# pull in Maddie's sim data
monkey_travel_df = read.csv("monkey_travel_data.csv")

# Check with model 1
model1 = lm(meanDailyDistance_km ~ meanDailyTemp_C * Habitat, data = monkey_travel_df)
anova(model1)
#can drop meanDailyTemp_C:habitat bc not significant

# fit model 2
model2 = lm(meanDailyDistance_km ~ meanDailyTemp_C + Habitat, data = monkey_travel_df)
anova(model2)

#meanDailyDistance_km: p < .05 significant
#Habitat: p < .05 significant
#meanDailyTemp_C:Habitat: p > .05 not significant

summary(model2)
###estimates:###
#Beta_0 = 5.9537, Beta_1 = -0.14, Beta_2 .43574, Beta_3 = -0.01 (?)

# These results show that mean daily temp had significant effects on daily travel 
# distance, such that when mean temp increased, daily distance in km decreased. 
# Additionally, living in a primary or secondary forest showed a significant
# difference in mean daily travel distances, such that in secondary forests
# primates are traveling more per day. 
# However, since mean daily temp x habitat differences was not significant, 
# that means that primary and secondary forests do not respond differently to
# temperature changes. This would mean the graph of these are showing parallel 
# slopes. 

# running a plot of Maddie's to see it
ggplot(monkey_travel_df, aes(x = meanDailyTemp_C, y = meanDailyDistance_km, color = Habitat)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() 



##### OBJ1: fit likelihood models #####

full_model = lm(meanDailyDistance_km ~ meanDailyTemp_C * Habitat, data = monkey_travel_df)
reduced_model = lm(meanDailyDistance_km ~ meanDailyTemp_C + Habitat, data = monkey_travel_df)

summary(full_model)
summary(reduced_model)

# log-likelihood

logLik(full_model)
logLik(reduced_model)

NLL_full = as.numeric(logLik(full_model))
NLL_reduced = as.numeric(logLik(reduced_model))

# NLL of full_model was lower than that of the reduced_model, 
# indicating that it is a better fit for the data

# likelihood ratio test

lrtest(reduced_model, full_model)
# The ratio test indicates that while the full model has better log-likelihood,
# the difference is not statistically significant, so reduced model is still
# preferred due to parsimony. This supports my previous modeling
# of Maddies data, such that primary and secondary forests do not respond
# differently to temperature changes. This implies additive effects, not interaction.

##### OBJ2: AIC #####
Y = monkey_travel_df$meanDailyDistance_km
X1 = monkey_travel_df$meanDailyTemp_C
X2 = monkey_travel_df$Habitat

#a 
model_full = lm(Y ~ X1 * X2, data = monkey_travel_df)
#b
model_main_ef = lm(Y ~ X1 + X2, data = monkey_travel_df)
#c 
model_X1   <- lm(Y ~ X1, data = monkey_travel_df)
#d
model_X2   <- lm(Y ~ X2, data = monkey_travel_df)
#e
model_intercept <- lm(Y ~ 1, data = monkey_travel_df)

models_list = list(
  "full" = model_full,
  "main effects" = model_main_ef,
  "X1" = model_X1,
  "X2" = model_X2,
  "intercept only" = model_intercept
)

aic_values = sapply(models_list, AIC)
AIC_table = data.frame(
  Model = names(aic_values),
  AIC = as.numeric(aic_values)
)
AIC_table

# add delta AIC as in the chapter

AIC_table$Delta_AIC <- AIC_table$AIC - min(AIC_table$AIC)
AIC_table

# The model with the lowest AIC is the full model, meaning
# it is the best fit for the data. The delta AIC of main effects model
# is less than 2, indicating it is also a good fit. 

# Using AIC, the full model is preferred, which contrasts with the
# likelihood ratio test results. This suggests that while the interaction
# term may not be statistically significant, it still improves model fit
# enough to be considered in model selection. The main effects test is equivalent
# to the reduced model (i believe?), and according to delta AIC, this is
# very close to the full model, so it could also be considered a good fit which
# aligns with the likelihood ratio test results.